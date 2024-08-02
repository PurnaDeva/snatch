-module(snatch_router).
-compile([warnings_as_errors]).

-behaviour(snatch).

-export([init/1, handle_info/2, terminate/2]).

-spec init([pid() | atom()]) -> {ok, pid() | atom()}.
init([undefined]) ->
    erlang:error(badarg);
init([Name]) when is_atom(Name) ->
    LocatedPID = whereis(Name),
    error_logger:info_msg("Located PID: ~p from Name:~p~n", [LocatedPID, Name]),
    {ok, LocatedPID};
init([PID]) when is_pid(PID) ->
    {ok, PID}.

-spec handle_info(Info :: term(), pid() | atom()) ->
      {noreply, pid() | atom()}.
handle_info(Info, PID) ->
    error_logger:info_msg("Handle Info. PID: ~p. Info:~p~n", [PID, Info]),
    
    case is_pid(PID) of
        true -> PID ! Info;
        false ->
            case whereis(PID) of
                undefined ->
                    error_logger:error_msg("Error: ~p is not a valid PID.~n", [PID]);
                LocatedPID when is_pid(LocatedPID) ->
                    LocatedPID ! Info;
                _ ->
                    error_logger:error_msg("Error: ~p is not a valid PID.~n", [PID])
            end
    end,
    
    {noreply, PID}.

-spec terminate(Reason :: any(), pid() | atom()) -> ok.
terminate(_Reason, _PID) ->
    ok.
