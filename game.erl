-compile(export_all).

-record(game, {gameid, 
               user1, 
               sock1,
               user2 = "*waiting*", 
               sock2,
               table = [[0,0,0],[0,0,0],[0,0,0]], 
               observers=[]}).
-record(user, {name, empty}).

%% *************************************************************** %%
%% ***** Funciones para agregar o quitar de la base de datos ***** %%
%% *************************************************************** %%

%% add_username
%% Agrega un usuario a la base de datos si es posible
add_username(UName) ->
    F = fun() ->
            mnesia:write(#user{name=UName})
        end,
    mnesia:activity(transaction, F).

%% exists_username
%% True si el usuario existe, caso contrario, False
exists_username(UName) ->
    F = fun() ->
            case mnesia:read({user, UName}) of 
                [] -> 
                    false;
                _  -> 
                    true
            end
        end,
    mnesia:activity(transaction, F). 

%% delete_username
%% Elimina un usuario de la base de datos
delete_username(UName) ->
    F = fun() ->
            mnesia:delete({user, UName})
        end,
    mnesia:activity(transaction, F).


%% ***************************************** %%
%% ***** Funciones de logica del juego ***** %%
%% ***************************************** %%
