-compile(export_all).
-include("game.erl").
-include_lib("stdlib/include/qlc.hrl").


%% CON
%% Hace un llamado a la base de datos para saber si ya existe
%% un usuario con su mismo nombre, en caso contrario lo registra.
cmd_con(UName, PSocket) ->
    case user_exists(UName) of
        false -> case psock_get_user(PSocket) of
                    [] -> user_add(UName, PSocket),
                          "valid " ++ UName;
                    _  -> "error"
                 end;
        _     -> "error"
    end.

%% LSG
%% Devuelve todas las partidas listadas en la base de datos
%% de la forma "GameId Usuario1 Usuario2"
cmd_lsg() ->
    GamesList  = game_list_all(),
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
            game_create(1, UName),
            "1";
        _  ->
            ID = lists:max(Ids) + 1,
            game_create(ID, UName),
            integer_to_list(ID)
    end.

%% ACC
%% Dados un juego y un usuario, intenta unirse en la posicion
%% del jugador2 si esta esta vacia.
cmd_acc(GameID, UName) ->
    GID = element(1, string:to_integer(GameID)),
    if 
        not(is_integer(GID)) -> "error_notint";
        true ->
            case game_fill_room(GID, UName) of
                valid -> "success " ++ GameID;
                error -> "error_gfr"
            end
    end.


%% PLA
%%
cmd_pla(GameID, UName, Play) ->
    T = game_get_table(GameID),
    case T of
        nil -> io:format("Error en cmd_pla~n~n", []),
                 "error";
        _   -> MakePlay = table_make_play(GameID, Play),
               case MakePlay of
                   game_over -> "error"; %%FIXME
                   error     -> "error";
                   NewTable  -> SetTable = game_set_table(GameID, NewTable, UName),
                                if 
                                  SetTable -> "success " ++ integer_to_list(GameID) ++ game_status(GameID, UName);
                                  true     -> "error"
                                end
               end
    end.

%% OBS
%% Dado un juego y un usuario, agrega el usuario a
%% la lista de observadores del juego.
cmd_obs(GameID, UName) ->
    game_add_observer(GameID, UName).


%% LEA
%%
cmd_lea(GameID, UName) ->
    game_delete_observer(GameID, UName).


%% BYE
%%
cmd_bye(UName) ->
    List = user_get_opponents_psock(UName) ++ user_get_observers_psock(UName),
    user_delete_all(UName),
    user_delete(UName),
    List.
