-module(client).
-compile(export_all).

%%TODO: agregar forma de conectarse a otro servidor (ej: si se cae el sv al que estoy conectado).
%%TODO: repensar toda la logica de las jugadas


%% Client: inicializa el proceso de cliente
%%  spawnea un escucha del servidor y continua a procesar comandos locales
start(Host,Port) ->
    {ok,Sock} = gen_tcp:connect(Host, Port, [list, {packet,4}]),
    io:format("~n>> Bienvenido al server de TaTeTi! <<~n"),
    io:format(">> Escriba \"help\" para una lista de comandos disponibles <<~n~n"),
    UpdaterPid = spawn(?MODULE,updater,[Sock, self()]),
    gen_tcp:controlling_process(Sock,UpdaterPid),
    client_loop(1,"",UpdaterPid).

%% Updater: procesa mensajes entrantes
%%  llegan a el, si son para el los procesa, sino los reenvia para el loop original
updater(Sock,CliLoopPid) ->
    receive
        %% Ante una respuesta del servidor la procesamos aca
        {tcp, Sock, Data} -> 
            case string:tokens(Data," ") of
                ["CON"|T] -> ok;
                ["LSG"|T] -> ok;
                ["NEW"|T] -> ok;
                ["ACC"|T] -> ok;
                ["PLA"|T] -> ok;
                ["OBS"|T] -> ok;
                ["LEA"|T] -> ok;
                ["BYE"|T] -> ok;
                ["UPD"|T] ->
                    case T of
                        ["acc",GId] -> io:format("Partida #~p: Aceptada. Es su turno.~n",[GId]);
                        ["pla",GId] -> io:format("Partida #~p: Es su turno.~n",[GId]),
                                       receive
                                           {tcp,Sock,Table} -> print_table(Table);
                                           _                -> io:format("ERROR: [updater] al recibir tabla.~n")
                                       end;
                        ["obs",GId] -> io:format("Partida #~p: Observando.~n",[GId]),
                                       receive
                                           {tcp,Sock,Table} -> print_table(Table);
                                           _                -> io:format("ERROR: [updater] al recibir tabla.~n")
                                       end
                    end;
                _         -> io:format("ERROR: [updater] al recibir paquete.~n")
            end;
        %% Si llega un comando de la consola, redirigirlo al servidor
        {client,Message} -> ok = gen_tcp:send(Sock,Message);
        %% Si se cae el servidor, informarle al cliente
        {tcp_closed,_}   -> io:format("~n>> SERVIDOR CAIDO <<~n",[]);
        Default          -> io:format("ERROR [updater] al recibir mensaje \"~p\".~n",[Default])
    end,
    updater(Sock,CliLoopPid).

%% Client_loop: recibe comandos de la consola
%%  imprime la ayuda o redirecciona a updater para ser enviados al server
client_loop(CommandNumber,UserName,UpdaterPid) ->
    receive
        {username, UserName} -> client_loop(CommandNumber, UserName, UpdaterPid)
    after 0 ->
        case CommandNumber of
            0 -> io:format("Gracias por jugar.~n");
            _ -> Data = string:strip(io:get_line(UserName ++ "$ "), right, $\n),
                 case Data of
                    "help" -> print_help(),
                              client_loop(CommandNumber,UserName,UpdaterPid);
                    "quit" -> ok; %%TODO
                    ""     -> client_loop(CommandNumber,UserName,UpdaterPid);
                    _      -> UpdaterPid ! {client,Data},
                              client_loop(CommandNumber+1,UserName,UpdaterPid)
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
 
