-module(server).
-compile(export_all).
-include("commands.erl").


%% Start: inicia el esquema de la base de datos y las tablas a usar
%%  luego llama a iniciar al servidor
start(Port) ->  
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(user, [{attributes, record_info(fields, user)}, {disc_copies, [node()]}]),
    mnesia:create_table(game, [{attributes, record_info(fields, game)}, {disc_copies, [node()]}]),
    Pid = spawn(?MODULE, server, [Port]),
    ok. 

start(Node,Port) ->
    mnesia:start(),
    Res = rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
    case Res of
        {badrpc, Reason} -> io:format("ERROR [start/2] rpc returned ~p~n", [Reason]);
        Default          -> io:format("MNESIA [rpc_call] rpc returned ~p~n", [Default])
    end,
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:add_table_copy(user, node(), disc_copies),
    mnesia:add_table_copy(game, node(), disc_copies),
    spawn(?MODULE, server, [Port, Node]),
    ok. 

%% Server/1: comienza la ejecucion de un servidor
%%  escucha en el puerto indicado, spawnea un balanceador de carga unico y propio,
%%  un dispatcher de mensajes, y da comienzo a la escucha de mensajes.
server(Port) ->
    {ok, Sock} = gen_tcp:listen(Port, [list, {packet, 4}, {active, true}, {reuseaddr, true}]),
    
    PidStat     = spawn_link(?MODULE, pstat, []),
    PidBalance  = spawn_link(?MODULE, pbalance, [statistics(run_queue), element(2, statistics(reductions)), node()]),
    global:register_name("balance" ++ atom_to_list(node()), PidBalance),
    PidDispatch = spawn_link(?MODULE, dispatcher, [Sock, PidBalance, 0]),

    process_flag(trap_exit, true),
    
    %% Registro globalmente el nombre del server para poder enviarle
    %% el mensajito de stop_srvr
    global:register_name("server" ++ atom_to_list(node()), self()),

    %% Ante la caida de algun proceso cierro todos los otros
    receive
        {'EXIT',_,Reason} -> 
            exit(PidStat, Reason),
            exit(PidBalance, Reason),
            exit(PidDispatch, Reason),
            gen_tcp:close(Sock),
            receive after 30000 -> ok end,

            %% ... y reinicio el servidor
            spawn(?MODULE, start, [Port]),
            ok;
        stop_srvr ->  
            exit(PidStat, ss),
            exit(PidBalance, ss),
            exit(PidDispatch, ss),
            Sock ! "stop_server",
            gen_tcp:close(Sock),
            ok
    end,
    ok.

%% Server/2: ejecuta un servidor uniendolo a otro ya existente
server(Port,Server) ->
    true = net_kernel:connect_node(Server),
    receive after 1000 -> ok end,           %%TODO: esto para que era?
    server(Port).

stop_server() ->
    Server = lists:filter(fun(X) -> lists:prefix("server" ++ atom_to_list(node()), X) end, global:registered_names()),
    case Server of
        [H] -> global:send(H, stop_srvr);
        _   -> io:format("ERROR: server not found ~n")
    end,
    Loops     = lists:filter(fun(X) -> lists:prefix("loop", X) end, global:registered_names()),
    NodeLoops = lists:filter(fun(X) -> lists:suffix(atom_to_list(node()), X) end, Loops),
    lists:foreach(fun(X) -> global:send(X, stop_srvr) end, NodeLoops),
    ok. 

%% Dispatcher:
dispatcher(Sock, PidBalance, Count) ->
    {ok, NewSock} = gen_tcp:accept(Sock),
    io:format(">> NEW_CON: ~p~n", [NewSock]),
    Pid = spawn(?MODULE, psocket, [NewSock, PidBalance, Count]),
    ok = gen_tcp:controlling_process(NewSock, Pid),
    Pid ! ok,
    dispatcher(Sock, PidBalance, Count + 1).

%% PSocket: Actua como intermediario entre el cliente y el servidor
%%  crea un "socket virtual" por cada cliente en el servidor
psocket(Sock, PidBalance, Count) -> 
    receive ok -> ok end,
    ok = inet:setopts(Sock, [{active,true}]),
    psocket_loop(Sock, PidBalance, nil, Count).

%% PSocket_loop: es el "socket virtual" de cada cliente
%TODO: UserName tiene que ocurrir luego de un CON, es mas, todo tiene que ser error hasta un CON
psocket_loop(Sock, PidBalance, UserName, Count) ->
    %% Tenemos que registar el psocket
    %% para poder mandarle el mensaje de kill
    %% cuando usamos stop_server
    global:register_name("loop" ++ integer_to_list(Count) ++ atom_to_list(node()), self()),
    receive
        %% Ante un request al pcommand lo pasamos al nodo con menos trabajo.
        {tcp, Sock, Data} ->
            PidBalance ! {psocket, self()},
                receive
                    {pbalance, Node} -> spawn(Node, ?MODULE, pcommand, [Data, UserName, self()]);
                    _                -> io:format("ERROR [psocket_loop] mistaken pbalance answer.~n")
                end;
        %% Ante una respuesta de un pcommand la reenviamos al cliente.
        {pcommand, Msg}    ->
            case Msg of
                "CON valid " ++ NewUserName -> 
                    gen_tcp:send(Sock, Msg),
                    psocket_loop(Sock, PidBalance, NewUserName, Count);
                "PLA success " ++ Tail      -> 
                    gen_tcp:send(Sock, Msg),
                    [GameID|Status] = string:tokens(Tail, " "),
                    GID = element(1, string:to_integer(GameID)),
                    gen_tcp:send(Sock, game_get_table(GID)),
                    PidSocket2 = user_get_psock(game_get_opponent(GID, UserName)),
                    PidSocket2 ! {pcommand, "UPD pla " ++ Tail },
                    PidSocket2 ! {pcommand, game_get_table(GID)},
                    lists:foreach(fun(X) -> X ! {pcommand, "UPD obs " ++ Tail}, 
                                            X ! {pcommand, game_get_table(GID)} end, 
                                            game_get_observers_psock(GID)),
                    case Status of
                        ["continue"] -> ok;
                        _            -> game_delete(GID)
                    end;
                "ACC succes "++GameID     ->  
                    GID = element(1, string:to_integer(GameID)),
                    OSock = user_get_psock(element(1, game_get_players(GID))),
                    OSock ! {pcommand, "UPD acc " ++ GameID}, 
                    OSock ! {pcommand, game_get_table(GID)};
                 _                        ->
                     gen_tcp:send(Sock, Msg)
            end;
        %% Ante una repentina desconexion del usuario.
        {tcp_closed, Sock} ->
            lists:foreach(fun(X) -> X ! {pcommand, "UPD disconnect " ++ UserName} end, cmd_bye(UserName)),
            io:format(">> USER_DC: ~p.~n", [UserName]),
            exit(disconnect);
        stop_srvr -> exit(kill);
        Default ->
            io:format("ERROR [psocket_loop] mensaje desconocido ~p.~n", [Default]),
            ok
    end,
    psocket_loop(Sock, PidBalance, UserName, Count).

%% PCommand: procesa los comandos enviados por el cliente
pcommand(Command, UserName, PidSocket) ->
    io:format("PCommand received ~p.~n", [Command]),
    case string:tokens(Command, " ") of
        ["CON" | T] -> 
            case  T of
                [Name] -> PidSocket ! {pcommand, "CON "++cmd_con(Name, PidSocket)};
                _      -> io:format("ERROR [pcommand] mistaken CON format.~n"),
                          PidSocket ! {pcommand, "CMDE wformat CON"}
            end;
        ["NEW" | T] -> 
            case T of
                [] -> PidSocket ! {pcommand, "NEW "++cmd_new(UserName)};
                _  -> io:format("ERROR [pcommand] mistaken NEW format.~n"),
                      PidSocket ! {pcommand, "CMDE wformat NEW"}
            end;
        ["ACC" | T] -> 
            case T of
                [GameID] -> PidSocket ! {pcommand, "ACC "++cmd_acc(GameID,UserName)};
                 _        -> io:format("ERROR [pcommand] mistaken ACC format.~n"),
                             PidSocket ! {pcommand, "CMDE wformat ACC"}
            end;
        ["LSG" | T] -> 
            case T of
                [] ->
                    PidSocket ! {pcommand, "LSG wait"},
                    lists:foreach(fun(X) -> PidSocket ! {pcommand,X} end, cmd_lsg()),
                    PidSocket ! {pcommand, "LSG end"};
                _  -> io:format("ERROR [pcommand] mistaken LSG format.~n"),
                      PidSocket ! {pcommand, "CMDE wformat LSG"}
            end;
        ["PLA" | T] -> 
            case T of
                [GameID, Play] -> GID = element(1, string:to_integer(GameID)),
                                  PidSocket ! {pcommand, "PLA " ++ cmd_pla(GID, UserName, Play)};
                 _              -> io:format("ERROR [pcommand] mistaken PLA format.~n"),
                                   PidSocket ! {pcommand, "CMDE wformat PLA"}
            end;
        ["OBS" | T] -> 
            case T of 
                [GameID] -> GID = element(1, string:to_integer(GameID)),
                            PidSocket ! {pcommand, "OBS " ++ cmd_obs(GID, UserName)};
                _        -> io:format("ERROR [pcommand] mistaken OBS format.~n"),
                            PidSocket ! {pcommand, "CMDE wformat OBS"}
            end;
        ["LEA" | T] -> 
            case T of
                [GameID] -> GID = element(1, string:to_integer(GameID)),
                            PidSocket ! {pcommand, "LEA " ++ cmd_lea(GID, UserName)};
                _        -> io:format("ERROR [pcommand] mistaken LEA format.~n"),
                            PidSocket ! {pcommand, "CMDE wformat LEA"}
            end;
        ["BYE" | T] -> 
            case T of
                [] -> PidSocket ! {pcommand, "BYE"};
                _  -> io:format("ERROR [pcommand] mistaken BYE format.~n"),
                      PidSocket ! {pcommand, "CMDE wformat BYE"}
            end;
        [Cmd | _]   -> io:format("ERROR [pcommand] command \"~p\" not found.~n",[Command]),
                       PidSocket ! {pcommand, "CMDE noexist " ++ Cmd};
        _           -> io:format("ERROR [pcommand] command \"~p\" not found.~n",[Command])
    end.
        
%% PBalance
%%  funciona de la siguiente manera: tiene como argumento un nodo que es el de menor carga.
%% Cuando recibe información de algún pStat compara si este nuevo nodo está menos cargado que el
%% que ya tiene. En caso afirmativo, este pasa a ser el nuevo argumento de la función pBalance; caso
%% contrario se vuelve a llamar con los mismos argumentos.
pbalance(Queue, Reductions, Node) ->
    receive
        %% Si recibimos un pedido de un psocket, le indicamos al nodo quien debe ejecutar el comando
        {psocket, Pid} ->
            Pid ! {pbalance, Node};

        %% Si recibimos información de un pstat, comparamos con el mejor postor que tenemos
        {pstat, {New_Queue, New_Reductions}, New_Node} ->
            case Queue > New_Queue of
                true  -> 
                    pbalance(New_Queue, New_Reductions, New_Node);
                false ->
                    case Queue < New_Queue of 
                        true  -> 
                            pbalance(Queue, Reductions, Node);
                        false ->
                            case Reductions > New_Reductions of
                                true  -> pbalance(New_Queue, New_Reductions, New_Node);
                                false -> pbalance(Queue, Reductions, Node)
                            end
                    end
            end;
        Default -> io:format("ERROR [pbalance] mensaje \"~p\"incorrecto.~n",[Default])
    end,
    pbalance(Queue, Reductions, Node).

%% PStat
%%  obtiene datos de carga de los nodos y los envia
pstat() ->
    %% Obtenemos datos de carga
    Queue           = statistics(run_queue),
    {_, Reductions} = statistics(reductions),

    %% Enviamos datos de carga a todos los nodos
    ListBalances = lists:filter(fun(X) -> lists:prefix("balance", X) end, global:registered_names()), 
    lists:map(fun(X) -> global:send(X, {pstat, {Queue, Reductions}, node()}) end, ListBalances),

    %% Esperamos 5 segundos para volver a enviar
    receive after 5000 -> ok end,
    pstat().

