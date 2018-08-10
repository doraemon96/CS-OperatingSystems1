-compile(export_all).

-record(game, {gameid, 
               user1,
               user2 = "*waiting*", 
               table = [[0,0,0],[0,0,0],[0,0,0]], 
               observers=[]}).
-record(user, {name, psock}).

%% *************************************************************** %%
%% ***** Funciones para agregar o quitar de la base de datos ***** %%
%% *************************************************************** %%

%% user_add
%% Agrega un usuario a la base de datos si es posible
user_add(UName, PSock) ->
    F = fun() ->
            mnesia:write(#user{name=UName, psock=PSock})
        end,
    mnesia:activity(transaction, F).

%% user_exists
%% True si el usuario existe, caso contrario, False
user_exists(UName) ->
    F = fun() ->
            case mnesia:read({user, UName}) of 
                [] -> 
                    false;
                _  -> 
                    true
            end
        end,
    mnesia:activity(transaction, F). 

%% user_delete
%% Elimina un usuario de la base de datos
user_delete(UName) ->
    F = fun() ->
            mnesia:delete({user, UName})
        end,
    mnesia:activity(transaction, F).

user_get_psock(UName) ->
    F = fun() ->
            case mnesia:read({user, UName}) of
                [T] -> erlang:element(3, T);
                _   -> nil
            end
        end,
    mnesia:activity(transaction, F).


%% ***************************************** %%
%% ***** Funciones de logica del juego ***** %%
%% ***************************************** %%

%% game_create
%% Crea un nuevo juego en la tabla de mnesia con un gameId unico
%% y deja vacia la posicion del segundo jugador.
game_create(GameId, UName) ->
    F = fun() ->
            mnesia:write(#game{gameid=GameId,
                               user1=UName})
        end,
    mnesia:activity(transaction, F).

%% game_list_all
%% Hace una query en mnesia por todas las tablas de juego.
game_list_all() ->
    F = fun() -> 
          Handle = qlc:q([P || P <- mnesia:table(game)]),
          qlc:e(Handle)
        end,
    mnesia:activity(transaction, F).

game_fill_room(GameID,UName) ->
    F = fun() -> R = mnesia:read({game, GameID}),
                 case R of
                     []  -> error;
                     [G] -> case element(4,G) of
                                "*waiting*" -> mnesia:write(G#game{user2=UName}),
                                               valid;
                                _           -> error
                            end
                 end
        end,
    mnesia:activity(transaction, F). 


%% delete_game
%% Elimina todos los registros en mnesia con el gameId dado.
game_delete(GameID) ->
    F = fun() ->
            mnesia:delete({game, GameID})
        end,
    mnesia:activity(transaction, F).

%% user_delete_all
%% Elimina los juegos en los que participa el usuario y lo quita
%% de la lista de observadores de todos los juegos.
user_delete_all(UName) ->
    %% Lo elimina de todas las partidas en las que participa
    F = fun() -> 
            Handle = qlc:q([P#game.gameid || P <- mnesia:table(game), (P#game.user1 == UName) or (P#game.user2 == UName)]),
            qlc:e(Handle)
        end,
    L = mnesia:activity(transaction, F),
    lists:foreach(fun(X) -> game_delete(X) end, L),
    %% Lo elimina de todas las listas de observadores
    %% TODO?: No usar match_object
    G = fun() -> 
            R = mnesia:match_object({game, '_', '_', '_', '_', '_'}),
            lists:foreach(fun(X) -> mnesia:write(X#game{observers = lists:delete(UName, element(6, X))}) end, R)
        end,
    M = mnesia:activity(transaction, G),
    ok.

%% inform_opponents
user_get_opponents_psock(UName) ->
    F1 = fun() ->
            Handle = qlc:q([P#game.user2 || P <- mnesia:table(game), (P#game.user1 == UName), (P#game.user2 =/= "*waiting*")]),
            qlc:e(Handle)
         end,
    L1 = mnesia:activity(transaction, F1),
    F2 = fun() ->
            Handle = qlc:q([P#game.user1 || P <- mnesia:table(game), (P#game.user2 == UName), (P#game.user1 =/= "*waiting*")]),
            qlc:e(Handle)
         end,
    L2 = mnesia:activity(transaction, F2),
    lists:map(fun(X) -> user_get_psock(X) end, L1 ++ L2).

%% gets a game's opponent
game_get_opponent(GameId, UName) ->
    Players = game_get_players(GameId),
    case element(1, Players) == UName of
        true -> element(2, Players);
        _    -> element(1, Players)
    end.


%% game_get_table
%% Devuelve el tablero de un juego.
game_get_table(GameId) ->
    F = fun() ->
            case mnesia:read({game, GameId}) of
                [T] -> erlang:element(5, T);
                _   -> nil
            end
        end,
    mnesia:activity(transaction, F).

%% game_get_players
%% Devuelve una tupla de jugadores.
game_get_players(GameId) ->
    F = fun() ->
            case mnesia:read({game, GameId}) of
                [T] -> 
                    U1 = erlang:element(3, T),
                    U2 = erlang:element(4, T),
                    {U1, U2};
                _   -> 
                    []
            end
        end,
    mnesia:activity(transaction, F).

%% game_set_table 
%% Cambia el tablero segun una jugada y devuelve true si la jugada fue
%% posible, y false si no.
game_set_table(GameId, Table, UName) ->
    OldTable   = game_get_table(GameId),
    IsTurn     = game_is_turn(GameId, UName),
    if IsTurn -> 
        case table_impossible(OldTable, Table) of
            true  -> false;
            false -> F = fun() ->
                            [R] = mnesia:wread({game, GameId}), 
                            mnesia:write(R#game{table = Table})
                            %% PCmd ! {table, Table} antes pasabamos esto por mensaje,
                            %% ahora devolvemos la tabla vieja si no se pudo, la nueva caso contrario
                         end,
                     mnesia:activity(transaction, F),
                     true
        end;
        true -> false
    end.

%% game_is_turn
%% devuelve true si le toca al jugador llamante,
%% false caso contrario.
game_is_turn(GameID, UserName) ->
    {U1, U2} = game_get_players(GameID),
    OldTable = game_get_table(GameID),
    case (lists:foldl(fun(X, Sum) -> if X == 0 -> 1 + Sum; true -> Sum end end, 0, lists:flatten(OldTable))) rem 2 of
        0 -> UserName == U2;
        _ -> UserName == U1
    end.

%% table_impossible
%% Booleano que idica si NO se puede hacer la jugada.
table_impossible(TableIn, TableOut) ->
    table_checkmultimove(TableIn, TableOut) or table_checksuperpos(TableIn, TableOut).

table_checkmultimove(TableIn, TableOut) ->
    Zip = lists:zip(lists:flatten(TableIn), lists:flatten(TableOut)),
    F   = fun(X) -> if
                      (element(1,X) /= element(2,X)) -> 1;
                      true                           -> 0
                    end
          end,
    (lists:sum(lists:map(F, Zip))) /= 1.

table_checksuperpos(TableIn, TableOut) ->
    Zip = lists:zip(lists:flatten(TableIn), lists:flatten(TableOut)),
    Fea = lists:map(fun(X) -> if 
                                (element(1,X) /= 0) and (element(2,X) /= element(1,X)) -> false;
                                true                                                   -> true
                              end
                    end,
                    Zip),
    not lists:all(fun(X) -> X end, Fea).

%% game_is_won
%% Muestra si el juego fue ganado
game_is_won(Table) ->
    table_checkrow(Table) or table_checkcol(Table) or table_checkdiagonal(Table).

table_checkrow(Table) ->
    Fea = lists:map(fun(X) -> ((lists:nth(1,X)) == lists:nth(2,X)) 
                          and ((lists:nth(2,X)) == lists:nth(3,X)) 
                          and (lists:nth(1,X) /= 0) %evita falsos positivos cuando el juego recien comienza
                    end, Table), 
    lists:any(fun(X) -> X end, Fea).

table_checkcol(Table) ->
    Zip = lists:zip3(lists:nth(1,Table), lists:nth(2,Table), lists:nth(3,Table)),
    Fea = lists:map(fun(X) -> (element(1,X) == element(2,X)) 
                          and (element(2,X) == element(3,X))  
                          and (element(1,X) /= 0)
                    end, Zip),
    lists:any(fun(X) -> X end, Fea).

table_checkdiagonal(Table) ->
    Diag1 = (lists:nth(1, lists:nth(1,Table)) == lists:nth(2, lists:nth(2,Table))) 
        and (lists:nth(2, lists:nth(2,Table)) == lists:nth(3, lists:nth(3,Table)))
        and (lists:nth(1, lists:nth(1,Table)) /= 0),
    Diag2 = (lists:nth(3, lists:nth(1,Table)) == lists:nth(2, lists:nth(2,Table))) 
        and (lists:nth(2, lists:nth(2,Table)) == lists:nth(1, lists:nth(3,Table)))
        and (lists:nth(3, lists:nth(1,Table)) /= 0),
    Diag1 or Diag2.

%% game_is_tie
%% Devuelve true si se ocuparon
%% todos los espacios del tablero.
game_is_tie(Table) ->
    F = fun(X) -> X == 0 end,
    not lists:any(fun(X) -> X end, (lists:map(F, lists:flatten(Table)))).

%% game_is_over
%% Determina si el juego acabó.
game_is_over(Table) ->
    game_is_tie(Table) or game_is_won(Table).

%% game_status
%% Indica si el juego sigue en curso
%% o quien gano en caso contrario
game_status(GameID, UName) ->
    Table = game_get_table(GameID),
    case game_is_tie(Table) of
        true -> " tie";
        _    -> case game_is_won(Table) of 
                    true -> 
                        IsTurn = game_is_turn(GameID, UName), 
                        if IsTurn -> " won " ++ game_get_opponent(GameID, UName);
                           true   -> " won " ++ UName
                        end;
                    _    -> " continue"
                end
    end. 

%% table_make_play
%% Guarda la jugada en la tabla.
table_make_play(UserName, GameId, Play) ->
    OldTable = game_get_table(GameId),
    {U1, U2} = game_get_players(GameId),
    Num      = case (lists:foldl(fun(X, Sum) -> if X == 0 -> 1 + Sum; true -> Sum end end, 0, lists:flatten(OldTable))) rem 2 of
                   0 -> 2;
                   _ -> 1
               end, 
    [H | T]  = Play,
    IsGameOver = game_is_over(OldTable),
    case IsGameOver of
        true -> game_over;
        _    ->
            case H of
                97  -> 
                    X2 = get_number(T),
                    B  = (X2 < 4) and (X2 > 0),
                    if B -> L2 = lists:nth(2, OldTable),
                            L3 = lists:nth(3, OldTable),
                            L1 = set_nth_list(lists:nth(1, OldTable), X2, Num),
                            [L1, L2, L3];
                       true -> error
                    end;
                98  -> 
                    X2 = get_number(T),
                    B  = (X2 < 4) and (X2 > 0),
                    if B -> L1 = lists:nth(1, OldTable),
                            L3 = lists:nth(3, OldTable),
                            L2 = set_nth_list(lists:nth(2, OldTable), X2, Num),
                            [L1, L2, L3];
                       true -> error
                    end;
                99  -> 
                    X2 = get_number(T),
                    B  = (X2 < 4) and (X2 > 0),
                    if B -> L1 = lists:nth(1, OldTable),
                            L2 = lists:nth(2, OldTable),
                            L3 = set_nth_list(lists:nth(3, OldTable), X2, Num),
                            [L1, L2, L3];
                       true -> error
                    end;
                _ -> 
                    error
            end
    end.

%% Funcion auxiliar de table_make_play, para setear
%% la posicion indicada.
set_nth_list([H | T], Number, Element) ->
    case Number of
        1 -> 
            [Element] ++ T;
        _ -> 
            [H] ++ set_nth_list(T, Number - 1, Element)
    end.
       
%% Funcion auxiliar de table_make_play, para ver que la jugada
%% tenga el formato indicado.
get_number(Tail) ->
    case Tail of
        []      -> error;
        [H | T] ->
            try erlang:list_to_integer([H]) of  
                X -> 
                    if T == [] -> X;
                       true -> error
                    end
            catch
                _:_ -> error
            end
    end.    
