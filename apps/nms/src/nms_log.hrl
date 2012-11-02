-ifndef(_LOG_H).
-define(_LOG_H, true).

-compile([{parse_transform, lager_transform}]).

-define(DBG(F, A), lager:debug("(~w:~b) ~p " ++ F ++ "~n", [?MODULE, ?LINE, self() | A])).
-define(INFO(F, A), lager:info("(~w:~b) ~p " ++ F ++ "~n", [?MODULE, ?LINE, self() | A])).
-define(ERR(F, A), lager:error("(~w:~b) ~p " ++ F ++ "~n", [?MODULE, ?LINE, self() | A])).
-define(WARN(F, A), lager:warning("(~w:~b) ~p " ++ F ++ "~n", [?MODULE, ?LINE, self() | A])).

-endif.  % _LOG_H
