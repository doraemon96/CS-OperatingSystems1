-compile(export_all).
-include("game.erl").
-include_lib("stdlib/include/qlc.hrl").


%% CON
%% Hace un llamado a la base de datos para saber si ya existe
%% un usuario con su mismo nombre, en caso contrario lo registra.
cmd_con(UserName) ->
    case exists_username(UserName) of
        false -> add_username(UserName),
                 "valid "++UserName;
        _     -> "error"
    end.

cmd_lsg(CmdId) -> "ok".

cmd_new(PlayerID) -> "ok".

cmd_acc(GameID, PlayerID) -> "ok".
