-module(nms_ctl).

-include("nms_log.hrl").

-behaviour(gen_server).

-export([start_link/0]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         file_chunk/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

file_chunk(FileName, ImagePartNumber, Data) ->
    gen_server:cast(?SERVER, {chunk, FileName, ImagePartNumber, Data}).

%%% gen_server callbacks

init([]) ->
    {ok, Servers} = application:get_env(nms, servers),
    {ok, ParallelNum} = application:get_env(nms, parallel_num),
    [[nms_sup:start_worker(Server) || Server <- Servers] || _ <- lists:seq(1, ParallelNum)],
    ?DBG("Servers: ~p", [Servers]),
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    ?WARN("Unhandled call:~n~p", [Request]),
    Error = {unknown_call, Request},
    {stop, Error, {error, Error}, State}.

handle_cast({chunk, FileName, ImagePartNumber, Data}, State) ->
    WPid =
        case gproc:lookup_local_name(FileName) of
            Pid when is_pid(Pid) ->
                Pid;
            _ ->
                {ok, Pid} = nms_sup:start_writer(FileName),
                Pid
        end,
    nms_writer:store(WPid, ImagePartNumber, Data),
    {noreply, State};
handle_cast(_Msg, State) ->
    ?WARN("Unhandled cast: ~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?WARN("Unhandled info:~n~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ?WARN("Terminated: ~p", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
