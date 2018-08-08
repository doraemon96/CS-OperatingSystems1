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
    spawn(?MODULE, server, [Port]),
    ok. 

start(Port, Node) ->
    mnesia:start(),
    Res = rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
    case Res of
        {badrpc, Reason} -> io:format("ERROR [start/2] rpc returned ~p~n",[Reason]);
        Default          -> io:format("MNESIA [rpc_call] rpc returned ~p~n",[Default])
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
    {ok, Sock} = gen_tcp:listen(Port, [list, {packet,4}, {active,true}, {reuseaddr,true}]),
    
    PidStat     = spawn_link(?MODULE, pstat, []),
    PidBalance  = spawn_link(?MODULE, pbalance, [statistics(run_queue),element(2,statistics(reductions)),node()]),
    global:register_name("balance" ++ atom_to_list(node()), PidBalance),
    PidDispatch = spawn_link(?MODULE, dispatcher, [Sock, PidBalance]),

    process_flag(trap_exit, true),
    
    %% Ante la caida de algun proceso cierro todos los otros
    receive
        {'EXIT',_,Reason} -> 
            exit(PidStat, Reason),
            exit(PidBalance, Reason),
            exit(PidDispatch, Reason),
            gen_tcp:close(Sock)
    end,

    receive after 30000 -> ok end,

    %% ... y reinicio el servidor
    spawn(?MODULE, start, [Port]),
    ok.

%% Server/2: ejecuta un servidor uniendolo a otro ya existente
server(Port,Server) ->
    true = net_kernel:connect_node(Server),
    receive after 1000 -> ok end,           %%TODO: esto para que era?
    server(Port).

%% Dispatcher:
dispatcher(Sock, PidBalance) ->
    {ok, NewSock} = gen_tcp:accept(Sock),
    io:format(">> NEW_CON: ~p~n",[NewSock]),
    Pid = spawn(?MODULE, psocket, [NewSock, PidBalance]),
    ok = gen_tcp:controlling_process(NewSock, Pid),
    Pid ! ok,
    dispatcher(Sock, PidBalance).

%% PSocket: Actua como intermediario entre el cliente y el servidor
%%  crea un "socket virtual" por cada cliente en el servidor
psocket(Sock, PidBalance) -> 
    receive ok -> ok end,
    ok = inet:setopts(Sock, [{active,true}]),
    psocket_loop(Sock, PidBalance, nil).

%% PSocket_loop: es el "socket virtual" de cada cliente
%TODO: UserName tiene que ocurrir luego de un CON, es mas, todo tiene que ser error hasta un CON
psocket_loop(Sock, PidBalance, UserName) ->
    receive
        %% Ante un request al pcommand lo pasamos al nodo con menos trabajo.
        {tcp, Sock, Data} ->
            PidBalance ! {psocket, self()},
            receive
                {pbalance, Node} ->
                    spawn(Node, ?MODULE, pcommand, [Data, UserName, self()]);
                _                -> io:format("ERROR [psocket_loop] mistaken pbalance answer.~n")
            end;
        %% Ante una respuesta de un pcommand la reenviamos al cliente.
        {pcommand, Msg}    ->
            case Msg of
                "CON valid "++NewUserName -> gen_tcp:send(Sock,Msg),
                                             psocket_loop(Sock, PidBalance, NewUserName);
                 _                        -> gen_tcp:send(Sock, Msg)
            end;
        %% Ante una repentina desconexion del usuario.
        {tcp_closed, Sock} ->
            lists:foreach(fun(X) -> X ! "UPD disconnect " ++ UserName end, get_opponents_psock(UserName)),
            delete_by_username(UserName),
            delete_username(UserName),
            io:format(">> USER_DC: ~p.~n",[UserName]),
            ok;
        Default ->
            io:format("ERROR [psocket_loop] mensaje desconocido ~p~n.", [Default]),
            ok
    end,
    psocket_loop(Sock,PidBalance,UserName).

%% PCommand: procesa los comandos enviados por el cliente
pcommand(Command, UserName, PidSocket) ->
    io:format("PCommand received ~p.~n",[Command]),
    case string:tokens(Command," ") of
        ["CON"|T] -> case T of
                        [Name] -> PidSocket ! {pcommand, "CON "++cmd_con(Name, PidSocket)};
                        _      -> io:format("ERROR [pcommand] mistaken CON format.~n")
                     end;
        ["NEW"|T] -> case T of
                        [] -> PidSocket ! {pcommand, "NEW "++cmd_new(UserName)};
                        _  -> io:format("ERROR [pcommand] mistaken NEW format.~n")
                     end;
        ["ACC"|T] -> case T of
                        [GameID] -> PidSocket ! {pcommand, "ACC "++cmd_acc(GameID,UserName)};
                        _        -> io:format("ERROR [pcommand] mistaken ACC format.~n")
                     end;
        ["LSG"|T] -> case T of
                        [] ->
                            PidSocket ! {pcommand, "LSG wait"},
                            lists:foreach(fun(X) -> PidSocket ! {pcommand,X} end, cmd_lsg()),
                            PidSocket ! {pcommand, "LSG end"};
                        _  -> io:format("ERROR [pcommand] mistaken LSG format.~n")
                     end;
        ["PLA"|T] -> PidSocket ! {pcommand, ok};
        ["OBS"|T] -> PidSocket ! {pcommand, ok};
        ["LEA"|T] -> PidSocket ! {pcommand, ok};
        ["BYE"|T] -> PidSocket ! {pcommand, ok};
        _         -> io:format("ERROR [pcommand] command \"~p\" not found.~n",[Command])
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

