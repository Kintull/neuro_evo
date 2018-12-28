%%%-------------------------------------------------------------------
%%% @author berg
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Дек. 2018 13:17
%%%-------------------------------------------------------------------
-module(neuron).
-author("berg").

-export([gen/0,
         loop/1]).

-record(state, {id, cx_pid, input_pid_ps_max, input_pid_ps_tmp,
                output_pids, exo_pid, af, signal = 0}).

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

%%--------------------------------------------------------------------
handle_cast({ExoSelfPid, {Id, CxPid, AF, InputPidPs, OutputPIds}}, State) ->
    {noreply, State#state{id = Id, cx_pid = CxPid, af = AF,
                          input_pid_ps_max = InputPidPs,
                          input_pid_ps_tmp = InputPidPs,
                          output_pids = OutputPIds,
                          exo_pid = ExoSelfPid}};

%%--------------------------------------------------------------------
handle_cast({InputPid, forward, Input}, State = #state{input_pid_ps_tmp = [InputPidP],
                                                       input_pid_ps_max = InputPidPsMax,
                                                       output_pids = OutPids,
                                                       signal = SignalAcc,
                                                       af = AF}) ->
    {InputPid, Weight} = InputPidP,
    Result = dot(Input, Weight),
    Output = neuron:AF(SignalAcc + Result),
    [Pid! {self(), forward, [Output]} || Pid <- OutPids],
    {noreply, State#state{signal = 0, input_pid_ps_tmp = InputPidPsMax}};

%%--------------------------------------------------------------------
handle_cast({InputPid, forward, Input}, State = #state{input_pid_ps_tmp = InputPidPs,
                                                       input_pid_ps_max = InputPidPsMax,
                                                       output_pids = OutPids,
                                                       signal = SignalAcc,
                                                       af = AF}) when length(InputPidPs) == 2 ->
    InputPidP = {InputPid, Weights} = lists:keyfind(InputPid, 1, InputPidPs),

    {NewSignal,NewInputPidTmp} =
        case lists:keyfind(bias, 1, InputPidPs) of
            false ->
                Result = dot(Input, Weights),
                {SignalAcc + Result, InputPidPs -- [InputPidP]};
            {bias, Bias} ->
                Result = dot(Input, Weights) + Bias,
                Output = math:AF(SignalAcc + Result),
                [Pid! {self(), forward, [Output]} || Pid <- OutPids],
                {0, InputPidPsMax}
        end,
    {noreply, State#state{signal = NewSignal, input_pid_ps_tmp = NewInputPidTmp}};

%%--------------------------------------------------------------------
handle_cast({InputPid, forward, Input}, State = #state{input_pid_ps_tmp = InputPidPs,
                                                       signal = SignalAcc}) ->
    InputPidP = {InputPid, Weights} = lists:keyfind(InputPid, 1, InputPidPs),
    Result = dot(Input, Weights),
    {noreply, State#state{signal = SignalAcc + Result,
                          input_pid_ps_tmp = InputPidPs -- [InputPidP]}};

%%--------------------------------------------------------------------
handle_cast({CxPid, get_backup}, State = #state{id = Id,
                                                cx_pid = CxPid,
                                                input_pid_ps_max = MInputPidPs}) ->
    CxPid ! {self(), Id, MInputPidPs},
    {noreply, State};

%%--------------------------------------------------------------------
handle_cast(terminate, _State) ->
    {stop, normal};

%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.



dot(Inputs,Weights) ->
    IWList = lists:zip(Inputs,Weights),
    R = fun({I,W},Acc) -> I*W + Acc end,
    lists:foldl(R, 0, IWList).