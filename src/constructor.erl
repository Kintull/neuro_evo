%%%-------------------------------------------------------------------
%%% @author berg
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Дек. 2018 19:38
%%%-------------------------------------------------------------------
-module(constructor).
-author("berg").

-include_lib("records.hrl").

-compile(export_all).

%% API
-export([]).

construct_genotype(SName, AName, HidLayerDens) ->
    construct_genotype(ffnn, SName, AName, HidLayerDens).

construct_genotype(FileName, SName, AName, HidLayerDens) ->
    S = create_sensor(SName),
    A = create_actuator(AName),
    Out_VL = A#actuator.vl,
    LayerDens = lists:append(HidLayerDens, [Out_VL]),
    CxId = {cortex, generate_id()},

    Neurons = create_neuro_layers(CxId,S,A,LayerDens),

    InputLayer = hd(Neurons),
    OutputLayer = lists:last(Neurons),
    FL_NIds = [N#neuron.id || N <- InputLayer],
    LL_NIds = [N#neuron.id || N <- OutputLayer],
    NIds = [N#neuron.id || N <- lists:flatten(Neurons)],
    Sensor = S#sensor{cx_id = CxId, fanout_ids = FL_NIds},
    Actuator = A#actuator{cx_id = CxId, fanin_ids = LL_NIds},
    Cortex = create_cortex(CxId,[S#sensor.id], [A#actuator.id], NIds),
    Genotype = lists:flatten([Cortex,Sensor,Actuator|Neurons]),

    {ok,F} = file:open(FileName, write),
    [io:format(F, "~p.~n",[L])||L<-Genotype],
    file:close(F).

create_sensor(SName) ->
    case SName of
        rng ->
            #sensor{id = {sensor, generate_id()}, name = rgn, vl =2};
        _ ->
            exit("bad sensor name")
    end.

create_actuator(AName) ->
    case AName of
        pts ->
            #actuator{id = {actuator, generate_id()}, name = pts, vl =1};
        _ ->
            exit("bad actuator name")
    end.
%%--------------------------------------------------------------
create_neuro_layers(CxId, Sensor, Actuator, LayerDens) ->
    InputIdPs = [{Sensor#sensor.id, Sensor#sensor.vl}],
    TotLayers = length(LayerDens),
    [FLNeurons | TailDens] = LayerDens,
    NIds = [{neuron, {1, Id}} || Id <- generate_ids(FLNeurons, [])],
    create_neuro_layers(CxId, Actuator#actuator.id, 1, TotLayers, InputIdPs, NIds, TailDens, []).

%%--------------------------------------------------------------
create_neuro_layers(CxId, AId, LayerIndex, TotLayers, InputIdPs,
                    NIds, [NextLD|LDs], Acc) ->

    OutputNIds = [{neuron, {LayerIndex + 1, Id}} || Id <- generate_ids(NextLD,[])],
    LayerNs = create_neuro_layer(CxId, InputIdPs, NIds, OutputNIds, []),
    NextInputIdPs = [{NId, 1} || NId <- NIds],
    create_neuro_layers(CxId, AId, LayerIndex + 1, TotLayers, NextInputIdPs, OutputNIds, LDs, [LayerNs | Acc]);
%%--------------------------------------------------------------
create_neuro_layers(CxId, AId, _LayerIndex, _TotLayers, InputIdPs,
                    NIds, [], Acc) ->

    OutputIds = [AId],
    LayerNs = create_neuro_layer(CxId, InputIdPs, NIds, OutputIds, []),
    lists:reverse([LayerNs|Acc]).

%%--------------------------------------------------------------
create_neuro_layer(CxId, InputIdPs, [Id|NIds], OutputIds, Acc) ->
    Neuron = create_neuron(InputIdPs, Id, CxId, OutputIds),
    create_neuro_layer(CxId, InputIdPs, NIds, OutputIds, [Neuron|Acc]);
%%--------------------------------------------------------------
create_neuro_layer(_CxId, _InputIdPs, [], _OutputIds, Acc) ->
    Acc.


%%--------------------------------------------------------------
create_neuron(InputIdPs, Id, CxId, OutputIds) ->
    ProperInputIdPs = create_neural_input(InputIdPs, []),
    #neuron{id = Id, cx_id = CxId, af = tanh, input_idps = ProperInputIdPs,
        output_ids = OutputIds}.

%%--------------------------------------------------------------
create_neural_input([{InputId, InputVL} | InputIdPs], Acc) ->
    Weights = create_neural_weights(InputVL, []),
    create_neural_input(InputIdPs,[{InputId, Weights} | Acc]);
%%--------------------------------------------------------------
create_neural_input([], Acc) ->
    lists:reverse([{bias, rand:uniform()-0.5}|Acc]).


create_neural_weights(0,Acc) ->
    Acc;
create_neural_weights(Index,Acc) ->
    W = rand:uniform()-0.5,
    create_neural_weights(Index-1, [W|Acc]).


generate_ids(0,Acc) ->
    Acc;
generate_ids(Index,Acc) ->
    Id = generate_id(),
    generate_ids(Index-1,[Id|Acc]).

generate_id() ->
    [H|T] = binary:split(binary:replace(list_to_binary(os:cmd("uuid -v4")), <<"\n">>, <<"">>), <<"-">>),
    H.


create_cortex(CxId, SIds, AIds, NIds) ->
    #cortex{id = CxId, sensor_ids = SIds, actuator_ids = AIds, nids = NIds}.



