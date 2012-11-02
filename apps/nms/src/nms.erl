-module(nms).

-include("nms_log.hrl").

-export([start/0, rl/0]).

-spec start() -> ok.
start() ->
    [application:start(App) || App <- [compiler, syntax_tools, lager, gproc, jiffy, ibrowse, ?MODULE]],
    ok.


%% @doc Reload whole application
-spec rl() -> ok.
rl() ->
    {ok, App} = application:get_application(?MODULE),
    rl(App).

-spec rl(App :: atom()) -> ok.
rl(App) ->
  {ok, Keys} = application:get_all_key(App),
  Modules =
    case lists:keysearch(modules, 1, Keys) of
      {value, {modules, Mods}} -> Mods;
      _ -> []
    end,
  Reload = fun(Module) ->
               code:purge(Module),
               code:load_file(Module),
               Module
           end,
  Res = [Reload(Mod) || Mod <- Modules],
  ?INFO("Reloaded: ~p", [Res]),
  ok.
