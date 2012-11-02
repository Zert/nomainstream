-module(nms_worker).

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

-record(state, {
          server :: string()
         }).

%%% API
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%% gen_server callbacks

init(Server) ->
    gen_server:cast(self(), do_request),
    {ok, #state{server = Server}}.

handle_call(Request, _From, State) ->
    ?WARN("Unhandled call:~n~p", [Request]),
    Error = {unknown_call, Request},
    {stop, Error, {error, Error}, State}.

handle_cast(do_request, #state{server = Server} = State) ->
    case ibrowse:send_req(Server, [], get, <<>>, [{response_format, binary}]) of
        {ok, "200", _, Body} when size(Body) > 0 ->
            Json = jiffy:decode(Body),
            {Content} = Json,
            ImageName = proplists:get_value(<<"imageName">>, Content),
            SizeOfPartInBytes = proplists:get_value(<<"sizeOfPartInBytes">>, Content),
            SizeOfImageInBytes = proplists:get_value(<<"sizeOfImageInBytes">>, Content),
            ImagePartNumber = proplists:get_value(<<"imagePartNumber">>, Content),
            Base64Data = proplists:get_value(<<"base64Data">>, Content),
            Data = base64:decode(Base64Data),
            nms_ctl:file_chunk(ImageName, ImagePartNumber, Data),
            ?DBG("Body received: ~p, ~p, ~p, ~p", [ImageName, SizeOfPartInBytes, SizeOfImageInBytes, ImagePartNumber]);
        _ ->
            pass
    end,
    gen_server:cast(self(), do_request),
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
