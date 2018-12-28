%%%-------------------------------------------------------------------
%%% @author berg
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Дек. 2018 13:05
%%%-------------------------------------------------------------------
-module(actuator).
-author("berg").

-export([gen/0,
         loop/1]).

-record(state, {id, cx_pid, name, fanin_pids_max, fanin_pids_tmp,
                exo_pid, input_signals = []}).

gen() ->
    spawn(?MODULE, loop, [#state{}]).

loop(State) ->
    receive
        Msg ->
            case handle_cast(Msg, State) of
                {noreply, NewState} ->
                    loop(NewState);
                {stop, _Reason} ->
                    ok
            end
    end.


handle_cast({ExoSelfPid, {Id, CxPid, Name, FanInPids}}, State) ->
    {noreply, State#state{id = Id, cx_pid = CxPid, name = Name,
                          fanin_pids_max = FanInPids, exo_pid = ExoSelfPid,
                          fanin_pids_tmp = FanInPids}};

handle_cast({FromPid, forward, Input}, State = #state{fanin_pids_tmp = [FromPid],
                                                      fanin_pids_max = AllFanInPids,
                                                      input_signals = InputAcc,
                                                      cx_pid = CxPid}) ->
    pts(lists:reverse([Input|InputAcc])),
    CxPid! {self(), sync},
    {noreply, State#state{fanin_pids_tmp = AllFanInPids,
                          input_signals = []}};

handle_cast({FromPid, forward, Input}, State = #state{fanin_pids_tmp = FanInPids,
                                                      input_signals = InputAcc}) ->
    {noreply, State#state{fanin_pids_tmp = FanInPids -- [FromPid],
                          input_signals = [Input|InputAcc]}};

handle_cast(terminate, _State) ->
    {stop, normal};

handle_cast(_Request, State) ->
    {noreply, State}.


pts(Result) ->
    io:format("actuator:pts() -> ~p~n",[Result]).