%%%-------------------------------------------------------------------
%%% @author berg
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Дек. 2018 15:37
%%%-------------------------------------------------------------------
-module(exoself).
-author("berg").

-export([gen/1,
         init/2]).

-include_lib("records.hrl").
-define(id_pid_table, id_pid_table).

-record(state, {genotype, filename}).


%%%-------------------------------------------------------------------
gen(FileName) ->
    spawn(?MODULE, init, [FileName, #state{}]).

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
handle_cast({CxPid, backup, NeuronIdsNWeigths}, State = #state{genotype = Genotype,
                                                               filename = FileName}) ->
    [Cx|_] = Genotype,
    CxPid = ets:lookup_element(?id_pid_table, Cx#cortex.id, 2),
    NewGenotype = update_genotype(Genotype, NeuronIdsNWeigths),

    {ok, File} = file:open(FileName, write),
    [io:format(File, "~p.~n",[G]) || G <- NewGenotype],
    file:close(File),

    io:format("Finished updating to file:~p~n",[FileName]),
    {noreply, State#state{genotype = NewGenotype}}.


init(FilePath, State) ->
    {ok, Genotype} = file:consult(FilePath),
    ets:new(?id_pid_table, [named_table]),
    [Cx|CerebralUnits] = Genotype,
    SIds = Cx#cortex.sensor_ids,
    AIds = Cx#cortex.actuator_ids,
    NIds = Cx#cortex.nids,

    start_cerebral_units(cortex, [Cx#cortex.id]),
    start_cerebral_units(sensor, SIds),
    start_cerebral_units(actuator, AIds),
    start_cerebral_units(neuron, NIds),

    init_cerebral_units(CerebralUnits),
    init_cortex(Cx),

    loop(State#state{genotype = Genotype, filename = FilePath}).

start_cerebral_units(UnitType, [Id|Ids]) ->
    Pid = UnitType:gen(),
    ets:insert(?id_pid_table, {Id, Pid}),
    ets:insert(?id_pid_table, {Pid, Id}),
    start_cerebral_units(UnitType, Ids);
start_cerebral_units(_UnitType, []) ->
    true.

init_cerebral_units([R|Records]) when is_record(R, sensor) ->
    SId = R#sensor.id,
    SPId = ets:lookup_element(?id_pid_table, SId, 2),
    CxPid = ets:lookup_element(?id_pid_table, R#sensor.cx_id, 2),
    SName = R#sensor.name,
    FanoutIds = R#sensor.fanout_ids,
    FanoutPids = [ets:lookup_element(?id_pid_table, Id, 2) || Id <- FanoutIds],
    SPId! {self(), {SId, CxPid, SName, R#sensor.vl, FanoutPids}},
    init_cerebral_units(Records);

init_cerebral_units([R|Records]) when is_record(R, actuator) ->
    AId = R#actuator.id,
    APId = ets:lookup_element(?id_pid_table,AId,2),
    CxPId = ets:lookup_element(?id_pid_table,R#actuator.cx_id,2),
    AName = R#actuator.name,
    FaninIds = R#actuator.fanin_ids,
    FaninPIds = [ets:lookup_element(?id_pid_table,Id,2) || Id <- FaninIds],
    APId ! {self(),{AId,CxPId,AName,FaninPIds}},
    init_cerebral_units(Records);

init_cerebral_units([R|Records]) when is_record(R,neuron) ->
    NId = R#neuron.id,
    NPId = ets:lookup_element(?id_pid_table,NId,2),
    CxPId = ets:lookup_element(?id_pid_table,R#neuron.cx_id,2),
    AFName = R#neuron.af,
    InputIdPs = R#neuron.input_idps,
    OutputIds = R#neuron.output_ids,
    InputPIdPs = convert_idps2pidps(InputIdPs),
    OutputPIds = [ets:lookup_element(?id_pid_table,Id,2) || Id <- OutputIds],
    NPId ! {self(),{NId,CxPId,AFName,InputPIdPs,OutputPIds}},
    init_cerebral_units(Records);

init_cerebral_units([])->
    ok.

convert_idps2pidps(A)->
    convert_idps2pidps(A, []).
convert_idps2pidps([{bias, Bias}], Acc)->
    lists:reverse([{bias, Bias} | Acc]);
convert_idps2pidps([{Id,Weights} | FaninIdPs], Acc)->
    IdPs = {ets:lookup_element(?id_pid_table, Id, 2), Weights},
    convert_idps2pidps(FaninIdPs, [IdPs| Acc]).

init_cortex(Cx) ->
    Cx_Id = Cx#cortex.id,
    Cx_PId = ets:lookup_element(?id_pid_table,Cx_Id,2),
    SIds = Cx#cortex.sensor_ids,
    AIds = Cx#cortex.actuator_ids,
    NIds = Cx#cortex.nids,
    SPIds = [ets:lookup_element(?id_pid_table,SId,2) || SId <- SIds],
    APIds = [ets:lookup_element(?id_pid_table,AId,2) || AId <- AIds],
    NPIds = [ets:lookup_element(?id_pid_table,NId,2) || NId <- NIds],
    Cx_PId ! {self(),{Cx_Id,SPIds,APIds,NPIds},1}.

update_genotype(Genotype, [{NId, PIdPs} | WeightPs]) ->
    N = lists:keyfind(NId, 2, Genotype),
    io:format("PIdsPs:~p~n",[PIdPs]),
    UpdatedInputIdPs = convert_idps2pidps(PIdPs),
    NewN = N#neuron{input_idps = UpdatedInputIdPs},
    NewGenotype = lists:keyreplace(NId, 2, Genotype, NewN),
%%    io:format("N:~p~n NewN:~p~n Genotype:~p~n NewGenotype:~p~n",
%%        [N, NewN, Genotype, NewGenotype]),
    update_genotype(Genotype, WeightPs);
update_genotype(Genotype, []) ->
    Genotype.

