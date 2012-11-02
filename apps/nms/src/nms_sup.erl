-module(nms_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_worker/1,
         start_writer/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SUP_WORKER, nms_worker_sup).
-define(SUP_WRITER, nms_writer_sup).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Args) ->
    supervisor:start_child(?SUP_WORKER, [Args]).

start_writer(Args) ->
    supervisor:start_child(?SUP_WRITER, [Args]).

init([nms_worker = ModName]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    AChild =
        [
         {ModName, {ModName, start_link, []},
          Restart, Shutdown, Type, [ModName]}
        ],
    {ok, {SupFlags, AChild}};

init([nms_writer = ModName]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    AChild =
        [
         {ModName, {ModName, start_link, []},
          Restart, Shutdown, Type, [ModName]}
        ],
    {ok, {SupFlags, AChild}};

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Children =
        [
         %% Supervisor for workers
         {?SUP_WORKER,
          {supervisor, start_link,
           [{local, ?SUP_WORKER}, ?MODULE, [nms_worker]]},
          Restart, Shutdown, supervisor, [?MODULE]},

         %% Supervisor for writers
         {?SUP_WRITER,
          {supervisor, start_link,
           [{local, ?SUP_WRITER}, ?MODULE, [nms_writer]]},
          Restart, Shutdown, supervisor, [?MODULE]},

         %% Supervisor for controller
         {nms_ctl, {nms_ctl, start_link, []},
          Restart, Shutdown, worker, [nms_ctl]}
        ],

    {ok, { SupFlags, Children} }.

