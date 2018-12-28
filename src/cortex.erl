%%%-------------------------------------------------------------------
%%% @author berg
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Дек. 2018 12:06
%%%-------------------------------------------------------------------
-module(cortex).
-author("berg").

-export([gen/0,
         loop/1]).

-record(state, {id, exo_self,s_pids, a_pids_max, a_pids_temp, n_pids, backup, steps}).

%%%-------------------------------------------------------------------
%%% Initialization
%%%-------------------------------------------------------------------

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
handle_cast({ExoSelfPid, {Id, SPids, APIds, NPIds}, Steps}, #state{id = undefined}) ->

    [SPid ! {self(),sync} || SPid <- SPids],
    {noreply, #state{id = Id, s_pids = SPids, a_pids_max = APIds,
                     a_pids_temp = APIds,
                     n_pids = NPIds, exo_self = ExoSelfPid,
                     steps = Steps}};

%%--------------------------------------------------------------------
handle_cast({APId, sync}, State = #state{a_pids_temp = [APId],
                                         exo_self = ExoPid,
                                         n_pids = NPids,
                                         a_pids_max = APids,
                                         s_pids = SPids,
                                         steps = 0}) ->
    NeuronIdsWeights = get_backup(NPids),
    ExoPid ! {self(), backup, NeuronIdsWeights},

    %% terminate processes
    [Pid!terminate || Pid <- NPids ++ SPids ++ APids],
    {noreply, State};

%%--------------------------------------------------------------------
handle_cast({APId, sync}, State = #state{a_pids_temp = [APId],
                                         a_pids_max = MAPids,
                                         s_pids = SPids,
                                         steps = Steps}) ->
    [SPid ! {self(),sync} || SPid <- SPids],
    {noreply, State#state{a_pids_temp = MAPids, steps = Steps - 1}};

%%--------------------------------------------------------------------
handle_cast({APId, sync}, State = #state{a_pids_temp = APids}) ->
    {noreply, State#state{a_pids_temp = APids -- [APId]}};

%%--------------------------------------------------------------------
handle_cast(terminate, _State) ->
    {stop, normal};

%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.



%%%-------------------------------------------------------------------
%%% Helping functions
%%%-------------------------------------------------------------------

get_backup(NPids) ->
    F = fun(Pid) ->
                Pid! {self(), get_backup},
                receive {Pid, NId, WeightTuples} ->
                        {NId, WeightTuples}
                end
        end,
    [F(Pid) || Pid <- NPids].