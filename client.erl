-module(client).
-compile(export_all).

%%TODO: agregar forma de conectarse a otro servidor (ej: si se cae el sv al que estoy conectado).


%% Client: inicializa el proceso de cliente
%%  spawnea un escucha del servidor y continua a procesar comandos locales
start(Host, Port) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [list, {packet, 4}]),
    io:format("~n>> Bienvenido al server de TaTeTi! <<~n"),
    io:format(">> Escriba \"help\" para una lista de comandos disponibles <<~n~n"),
    UpdaterPid = spawn(?MODULE, updater, [Sock, self()]),
    gen_tcp:controlling_process(Sock, UpdaterPid),
    client_loop(1, "", UpdaterPid).

%% Updater: procesa mensajes entrantes
%%  actua como intermediario inmediato entre el cliente y el servidor
updater(Sock, CliLoopPid) ->
    receive
        %% Ante una respuesta del servidor la procesamos aca
        {tcp, Sock, Data} -> 
            %DEBUG: io:format("Updater received \"~p\"~n", [Data]),
            case string:tokens(Data, " ") of
                ["CMDE"|Msg] -> 
                    case Msg of
                        ["noexist", Cmd] -> io:format("El comando ~p no existe.~n",[Cmd]);
                        ["wformat", Cmd] -> io:format("Parametros incorrectos en ~p.~n",[Cmd]);
                        _                -> io:format("ERROR [updater] al procesar CMDE~n", [])
                    end;
                ["CON" | T] ->
                    case T of
                        ["error"] -> io:format("Usuario invalido. Intente de nuevo.~n");
                        ["valid", UserName] -> io:format("Bienvenido ~p.~n", [UserName]),
                                              CliLoopPid ! {username, UserName};
                        _         -> io:format("ERROR [updater] al procesar CON~n", [])
                    end;
                ["LSG" | T] -> 
                    case T of
                        ["wait"] -> io:format(" >> Lista de Juegos <<~n"),
                                    io:format(string:centre("GameID", 22) ++ string:centre("Player1", 22) ++ string:centre("Player2", 22) ++ "~n"),
                                    ok = lsg_loop(Sock),
                                    io:format(" >> Fin de la Lista <<~n~n");
                        _        -> io:format("ERROR [updater] al procesar LSG~n", [])
                    end;
                ["NEW" | T] -> 
                    case T of 
                        [ID] -> io:format("Nuevo juego creado con ID: ~p~n", [ID]);
                        _    -> io:format("ERROR [updater] al procesar NEW~n", [])
                    end;
                ["ACC" | T] -> 
                    case T of
                        ["success", ID] -> io:format("Aceptaste el juego #~p exitosamente.~n", [ID]);
                        _               -> io:format("ERROR [updater] al procesar ACC~n", [])
                    end;
                ["PLA" | T] ->
                    case T of
                        ["success", GId | T2] ->
                            io:format("Partida #~p: Jugada exitosa.~n", [GId]),
                            receive
                                {tcp, Sock, Table} -> print_table(Table);
                                _                  -> io:format("ERROR: [updater] al recibir tabla.~n")
                            end,
                            case T2 of
                                ["tie"]       -> io:format("+-+-+-+-+-+-+-+- EMPATE -+-+-+-+-+-+-+-+~n~n");
                                ["won", User] -> io:format("+-+-+-+-+-+-+-+- " ++ User ++ " GANÓ -+-+-+-+-+-+-+-+~n~n");
                                _             -> ok
                            end;
                        _                -> io:format("ERROR [updater] al procesar PLA~n", [])
                    end;
                ["OBS" | T] -> 
                    case T of 
                        ["success", GId] -> io:format("Estás observando el juego #~p. ~n", [GId]);
                        _                -> io:format("ERROR [updater] al procesar OBS~n", [])
                    end;     
                ["LEA" | T] -> 
                    case T of 
                        ["success", GId] -> io:format("Dejaste de observar el juego #~p. ~n", [GId]);
                        _                -> io:format("ERROR [updater] al procesar LEA~n", [])
                    end;     
                ["BYE"] -> 
                    io:format("Hasta luego~n"),
                    gen_tcp:close(Sock),
                    ok;
                ["UPD" | T] ->
                    case T of
                        ["acc", GID] -> 
                            io:format("Partida #~p: Aceptada. Es su turno.~n", [GID]),
                            receive
                                {tcp, Sock, Table} -> print_table(Table);
                                _                  -> io:format("ERROR: [updater] al recibir tabla.~n")
                            end;
                        ["pla", GID | Status] -> 
                            io:format("Partida #~p:~n", [GID]),
                            receive
                                {tcp, Sock, Table} -> print_table(Table);
                                _                  -> io:format("ERROR: [updater] al recibir tabla.~n")
                            end,
                            case Status of 
                                ["tie"]       -> io:format("+-+-+-+-+-+-+-+- EMPATE -+-+-+-+-+-+-+-+~n~n");
                                ["won", User] -> io:format("+-+-+-+-+-+-+-+- " ++ User ++ " GANÓ -+-+-+-+-+-+-+-+~n~n");
                                ["continue"]  -> io:format("+-+-+-+-+-+-+-+- " ++ "Juego en curso" ++ " -+-+-+-+-+-+-+-+~n~n")
                            end;
                        ["obs", GID | Status] -> 
                            io:format("Partida #~p: Observando.~n", [GID]),
                            receive
                                {tcp, Sock, Table} -> print_table(Table);
                                _                  -> io:format("ERROR: [updater] al recibir tabla.~n")
                            end,
                            case Status of 
                                ["tie"]       -> io:format("+-+-+-+-+-+-+-+- EMPATE -+-+-+-+-+-+-+-+~n~n");
                                ["won", User] -> io:format("+-+-+-+-+-+-+-+- " ++ User ++ " GANÓ -+-+-+-+-+-+-+-+~n~n");
                                ["continue"]  -> io:format("+-+-+-+-+-+-+-+- " ++ "Juego en curso" ++ " -+-+-+-+-+-+-+-+~n~n")
                            end;
                        ["disconnect", UName] -> io:format("~n~nUsuario ~p se ha desconectado.~n~n~n", [UName])
                    end;
                Default -> io:format("ERROR: [updater] al recibir paquete \"~p\".~n", [Default])
            end;
        %% Si llega un comando de la consola, redirigirlo al servidor
        {client, Message} -> ok = gen_tcp:send(Sock, Message);
        %% Si se cae el servidor, informarle al cliente
        {tcp_closed, _}   -> io:format("~n>> SERVIDOR CAIDO <<~n", []);
        Default           -> io:format("ERROR [updater] al recibir mensaje \"~p\".~n", [Default])
    end,
    updater(Sock, CliLoopPid).

%% Client_loop: recibe comandos de la consola
%%  imprime la ayuda o redirecciona a updater para ser enviados al server
client_loop(CommandNumber, UserName, UpdaterPid) ->
    receive
        {username, NewName} -> client_loop(CommandNumber, NewName, UpdaterPid)
    after 0 ->
        case CommandNumber of
            0 -> io:format("Gracias por jugar.~n");
            _ -> Data = string:strip(io:get_line(UserName ++ "$ "), right, $\n),
                 case Data of
                    "help" -> print_help(),
                              client_loop(CommandNumber, UserName, UpdaterPid);
                    "quit" -> init:stop(); %%TODO
                    ""     -> client_loop(CommandNumber, UserName, UpdaterPid);
                    %% FIXME: el caso de BYE en el updater ¿No iria aca?
                    "BYE"  -> UpdaterPid ! {client, Data},
                              ok;
                    _      -> UpdaterPid ! {client, Data},
                              client_loop(CommandNumber + 1, UserName, UpdaterPid)
                 end
        end
    end.


%% Print_table: pretty-print de una tabla de juego
%%
print_table(Table) ->
    F  = fun(X) -> 
            case X of
                0 -> " ";
                1 -> "X";
                2 -> "O";
                _ -> ""
            end
         end,
    [A1, B1, C1, A2, B2, C2, A3, B3, C3] = lists:map(F, lists:flatten(Table)),
    S1 = "    1   2   3 ~n",
    S2 = "              ~n",
    S3 = "   ---+---+---~n",
    S4 = "a   " ++ A1 ++ " ¦ " ++ B1 ++ " ¦ " ++ C1 ++ " ~n",
    S5 = "b   " ++ A2 ++ " ¦ " ++ B2 ++ " ¦ " ++ C2 ++ " ~n",
    S6 = "c   " ++ A3 ++ " ¦ " ++ B3 ++ " ¦ " ++ C3 ++ " ~n",
    io:format("~n" ++ S1 ++ S2 ++ S4 ++ S3 ++ S5 ++ S3 ++ S6 ++ "~n~n", []).


lsg_loop(Sock) ->
    receive
        {tcp, Sock, Data} ->
            case Data of
                "LSG end" -> ok;
                _         -> lists:foreach(fun(X) -> io:format(string:centre(X, 22), []) end, string:tokens(Data, " ")),
                             io:format("~n", []),
                             lsg_loop(Sock)
            end
    end.
    

%% Print_help
%%
print_help() ->
    io:format("~nComandos permitidos: ~n"),
    io:format("CON name - Conecta al servidor con el nombre deseado ~n"),
    io:format("NEW - Crea un nuevo juego ~n"),
    io:format("ACC juegoid - Acepta el juego identificado por juegoid ~n"),
    io:format("LSG - Lista los juegos disponibles. Estos son los que estan en" 
              ++ " desarollo y los que estan esperando un contrincante ~n"),
    io:format("PLA juegoid jugada - Realiza una jugada" 
              ++ " en el juego identificado por juegoid"
              ++ ". En la jugada debe indicarse primero la fila"
              ++ " (con una letra minuscula de la a a la c), y luego la columna"
              ++ " (con un numero del 1 al 3). Por ej.: \"PLA a3\" ~n"),
    io:format("OBS juegoid - Pide observar un juego ~n"),
    io:format("LEA juegoid - Deja de observar un juego ~n"),
    io:format("BYE - Termina la conexion. Abandona todos los juegos en los"
              ++ " que se este participando ~n ~n").
 
