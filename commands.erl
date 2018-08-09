-compile(export_all).
-include("game.erl").
-include_lib("stdlib/include/qlc.hrl").


%% CON
%% Hace un llamado a la base de datos para saber si ya existe
%% un usuario con su mismo nombre, en caso contrario lo registra.
cmd_con(UName, PSocket) ->
    io:format("PCOMMAND received ~p~n",[UName]),
    case exists_username(UName) of
        false -> add_username(UName, PSocket),
                 "valid "++UName;
        _     -> "error"
    end.

%% LSG
%% Devuelve todas las partidas listadas en la base de datos
%% de la forma "GameId Usuario1 Usuario2"
cmd_lsg() ->
    GamesList  = list_games(),
    lists:map(fun({ _, X, Y, Z, _, _}) -> erlang:integer_to_list(X) ++ " " 
                                                    ++ Y ++ " " 
                                                    ++ Z end, GamesList).
    
%% NEW
%% Dado un usuario, crea una partida a su nombre con un id
%% unico. Deja al jugador2 vacio, en espera a que se una.
cmd_new(UName) -> 
    F = fun() -> 
          Handle = qlc:q([P#game.gameid || P <- mnesia:table(game)]),
          qlc:e(Handle)
        end,
    Ids = mnesia:activity(transaction, F),
    case Ids of
        [] -> 
            create_game(1, UName),
            "1";
        _  ->
            ID = lists:max(Ids) + 1,
            create_game(ID, UName),
            integer_to_list(ID)
    end.

%% ACC
%% Dados un juego y un usuario, intenta unirse en la posicion
%% del jugador2 si esta esta vacia.
cmd_acc(GameID, UName) ->
    GID = element(1,string:to_integer(GameID)),
    if 
        not(is_integer(GID)) -> "error_notint";
        true ->
            case game_fill_room(GID, UName) of
                valid -> get_user_psock(element(1,get_game_players(GID))) ! {pcommand, "UPD acc " ++ integer_to_list(GID)}, %FIXME
                        "success";
                error -> "error_gfr"
            end
    end.
