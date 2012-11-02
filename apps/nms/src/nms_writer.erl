-module(nms_writer).

-include("nms_log.hrl").

-behaviour(gen_server).

-export([start_link/1]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-export([
         store/3
        ]).

-record(state, {
          filename             :: string(),
          parts_num            :: integer(),
          chunks = dict:new()  :: dict()
         }).

%%% API
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

store(Pid, ImagePartNumber, Data) ->
    gen_server:cast(Pid, {store, ImagePartNumber, Data}).

%%% gen_server callbacks

init(FileName) ->
    gproc:add_local_name(FileName),
    {ok, PicsPartsNum} = application:get_env(nms, pics_parts_num),
    {ok, #state{filename = FileName, parts_num = PicsPartsNum}}.

handle_call(Request, _From, State) ->
    ?WARN("Unhandled call:~n~p", [Request]),
    Error = {unknown_call, Request},
    {stop, Error, {error, Error}, State}.

handle_cast({store, ImagePartNumber, Data},
            #state{filename = FileName, chunks = Chunks, parts_num = PartsNum} = State) ->
    NewChunks = dict:store(ImagePartNumber, Data, Chunks),
    ?DBG("Store ~p of ~p", [ImagePartNumber, FileName]),
    Size = dict:size(NewChunks),
    if
        Size == PartsNum ->
            %% Write data
            ChList = dict:to_list(NewChunks),
            SortedData = lists:keysort(1, ChList),
            FileBin = << <<D/binary>> || {_, D} <- SortedData >>,
            ok = file:write_file(FileName, FileBin),
            ?INFO("File ~p completed", [FileName]),
            {stop, normal, State};
        true ->
            {noreply, State#state{chunks = NewChunks}}
    end;
handle_cast(_Msg, State) ->
    ?WARN("Unhandled cast: ~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?WARN("Unhandled info:~n~p", [_Info]),
    {noreply, State}.

terminate(normal, _State) ->
    ?DBG("Terminated with normal", []),
    ok;
terminate(_Reason, _State) ->
    ?WARN("Terminated: ~p", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
