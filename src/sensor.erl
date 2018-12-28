%%%-------------------------------------------------------------------
%%% @author berg
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Дек. 2018 12:48
%%%-------------------------------------------------------------------
-module(sensor).
-author("berg").

-export([gen/0,
         loop/1]).

-record(state, {id, cx_pid, name, vl, fanout_pids, exo_pid}).

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

%%%-------------------------------------------------------------------
%%% Handling messages
%%%-------------------------------------------------------------------

handle_cast({ExoSelfPid, {Id, CxPid, SensorName, VL, FanoutPids}}, State) ->
    {noreply, State#state{id = Id, cx_pid = CxPid, vl = VL,
                          fanout_pids = FanoutPids, exo_pid = ExoSelfPid,
                          name = SensorName}};

handle_cast({CxPid, sync}, State = #state{cx_pid = CxPid,
                                          fanout_pids = OutPids,
                                          vl = VL}) ->
    SensoryVector = rng(VL),
    [Pid! {self(), forward, SensoryVector} || Pid <- OutPids],
    {noreply, State};

handle_cast(terminate, _State) ->
    {stop, normal};

handle_cast(_Request, State) ->
    {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


rng(VL) ->
    R = fun(_) -> rand:uniform() end,
    lists:map(R, lists:seq(1,VL)).
