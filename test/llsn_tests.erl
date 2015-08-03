%% Erlang support for LLSN - Allyst's data interchange format.
%% LLSN specification http://allyst.org/opensource/llsn/
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Library General Public License for more details.
%%
%% Full license: https://github.com/allyst/erlang-llsn/blob/master/LICENSE
%%
%% copyright (C) 2015 Allyst Inc. http://allyst.com
%% author Taras Halturin <halturin@allyst.com>

-module(llsn_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/llsn.hrl").

-compile(export_all).


llsn_encode_signed_numbers_test() ->
    [
        fun() ->
            {BinNum, _} = llsn:encode_NUMBER(N),
            {DecNum, _} = llsn:decode_NUMBER(BinNum),
            ?assert(N =:= DecNum)
        end
        || N <- get_signed_numbers()
    ].

llsn_encode_unsigned_numbers_test() ->
    [
        fun() ->
            {BinNum, _} = llsn:encode_UNUMBER(N),
            {DecNum, _} = llsn:decode_UNUMBER(BinNum),
            ?assert(N =:= DecNum)
        end
        || N <- get_unsigned_numbers()
    ].

llsn_random_signed_NUMBER(0) ->
    ok;
llsn_random_signed_NUMBER(N) ->
    Num = llsn_gen:random_signed_number(),
    {BinNum, _} = llsn:encode_NUMBER(Num),
    {DecNum, _} = llsn:decode_NUMBER(BinNum),
    ?assert(Num =:= DecNum),
    llsn_random_signed_NUMBER(N-1).

llsn_1K_random_signed_NUMBER_test() ->
    llsn_random_signed_NUMBER(1000).


llsn_random_unsigned_NUMBER(0) ->
    ok;

llsn_random_unsigned_NUMBER(N) ->
    Num = llsn_gen:random_unsigned_number(),
    {BinNum, _} = llsn:encode_UNUMBER(Num),
    {DecNum, _} = llsn:decode_UNUMBER(BinNum),
    ?assert(Num =:= DecNum),
    llsn_random_unsigned_NUMBER(N-1).

llsn_1K_random_unsigned_NUMBER_test() ->
    llsn_random_unsigned_NUMBER(1000).

llsn_1K_random_DATE_test() ->
    ok.

llsn_1K_random_FLOAT_test() ->
    ok.

llsn_encodeComplexStruct_test() ->
    Value = get_exampleMainValue(),
    Declaration = get_exampleMainDeclaration(),
    ValueBin = get_exampleMainValueEncoded(),
    Bin = llsn:encode(Value,Declaration,4),
    ?assert(Bin =:= ValueBin),
    ok.

llsn_encodeEtalonEncodeDecodeEtalon_Threshold_test() ->
    Etalon      = get_exampleMainValue(),
    Declaration = get_exampleMainDeclaration(),
    ValueBin    = llsn:encode(Etalon,Declaration,4),
    Value       = llsn:decode(ValueBin),

    Etalon15    = element(15, Etalon),
    Etalon15_1  = Etalon15#llsn_file{origin = null},
    EtalonFixed  = setelement(15, Etalon, Etalon15_1),

    Value15     = element(15, Value),
    Value15_1   = Value15#llsn_file{origin = null},
    ValueFixed  = setelement(15, Value, Value15_1),

    ?assert(EtalonFixed =:= ValueFixed),
    ok.

llsn_encodeEtalonEncodeDecodeEtalon_NoThreshold_test() ->
    Etalon      = get_exampleMainValue(),
    Declaration = get_exampleMainDeclaration(),
    ValueBin    = llsn:encode(Etalon,Declaration),
    Value       = llsn:decode(ValueBin),

    Etalon15    = element(15, Etalon),
    Etalon15_1  = Etalon15#llsn_file{origin = null},
    EtalonFixed  = setelement(15, Etalon, Etalon15_1),

    Value15     = element(15, Value),
    Value15_1   = Value15#llsn_file{origin = null},
    ValueFixed  = setelement(15, Value, Value15_1),

    ?assert(EtalonFixed =:= ValueFixed).

llsn_encodeComplexStruct(0) ->
    ok;

llsn_encodeComplexStruct(N) ->
    ok = llsn_encodeComplexStruct_test(),
    llsn_encodeComplexStruct(N-1).

llsn_1K_encodeComplexStruct_test() ->
    llsn_encodeComplexStruct(1000).



llsn_decodeComplexStruct_test() ->
    ValueBin = get_exampleMainValueEncoded(),
    Value = llsn:decode(ValueBin),
    % we have to remove 'origin' from 'llsn_file'
    F0 = element(15, Value),
    F1 = F0#llsn_file{origin = null},
    Value1 = setelement(15, Value, F1),

    MainValue = get_exampleMainValue(),
    FF0 = element(15, MainValue),
    FF1 = FF0#llsn_file{origin = null},
    MainValue1 = setelement(15, MainValue, FF1),

    ?assert(Value1 =:= MainValue1).


llsn_decodeComplexStructSlowStream_test() ->
    ValueBin = get_exampleMainValueEncoded(),
    Value = slow_stream(ValueBin, null),
    % we have to remove 'origin' from 'llsn_file'
    F0 = element(15, Value),
    io:format("~p ~n", [F0]),
    F1 = F0#llsn_file{origin = null},
    Value1 = setelement(15, Value, F1),

    MainValue = get_exampleMainValue(),
    FF0 = element(15, MainValue),
    FF1 = FF0#llsn_file{origin = null},
    MainValue1 = setelement(15, MainValue, FF1),

    ?assert(Value1 =:= MainValue1).

llsn_decodeComplexStructSlowStreamNoThreshold_test() ->
    Value0 = get_exampleMainValue(),
    Declaration = get_exampleMainDeclaration(),
    ValueBin    = llsn:encode(Value0,Declaration),
    Value1 = slow_stream(ValueBin, null),
    % we have to remove 'origin' from 'llsn_file'
    F0 = element(15, Value0),
    F0_1 = F0#llsn_file{origin = null},
    Value0_1 = setelement(15, Value0, F0_1),

    FF0 = element(15, Value1),
    FF1 = FF0#llsn_file{origin = null},
    Value1_1 = setelement(15, Value1, FF1),

    ?assert(Value0_1 =:= Value1_1).

llsn_decodeEtalonDecodeEncodeEtalon_test() ->
    EtalonBin   = get_exampleMainValueEncoded(),
    Value       = llsn:decode(EtalonBin),
    Declaration = get_exampleMainDeclaration(),
    ValueBin    = llsn:encode(Value, Declaration, 4),

    ?assert(EtalonBin =:= ValueBin).


receive_frame(Bin) ->

    receive
        {frame, N, _Size, Frame, UserData } ->
            ?assert(UserData =:= [userdata]),
            receive_frame(<<Bin/binary,Frame/binary>>);

        {done, N, _Size, Frame, UserData} ->
            ?assert(UserData =:= [userdata]),
            <<Bin/binary,Frame/binary>>

    after 1000 -> ?assert("timeout")
    end.

llsn_encodeComplexStruct_with_Framing_test() ->
    Value = get_exampleMainValue(),
    Declaration = get_exampleMainDeclaration(),
    ok    = llsn:encode(Value, Declaration, self(), 50, [userdata]),
    ValueBin = receive_frame(<<>>),
    ValueBinNoFrame   =  llsn:encode(Value, Declaration),
    ?assert(ValueBin =:= ValueBinNoFrame).



llsn_decodeComplexStruct_with_Framing_test() ->
    ok.


%% =============================================================================
%% helpers
%% =============================================================================

% нужно еще включить в тест массив структур, где структура имеет количество полей > 8.
% а то я в коде опираюсь на nullflag != null как на факт уже закодированной структуры
% в первом проходе. хотя если количество полей > 8 то текущий алгоритм в том же Go будет
% считать, что мы уже на втором проходе и пора считать nullflag byte хотя это будет
% лишь первый проход.
%
% А еще нужно включить string, blob, file, которые не попадают под threshold, чтобы были
% закодированы прям в теле пакета, а не в хвосте(tail), размещенных где-нибудь во вложенных 
% структурах с количеством элементов этих структур >8 чтобы заодно покрыть кейс предыдущего
% абзаца

get_exampleMainValueEncoded() ->
    <<16, 4, 19, 1, 33, 254, 12, 131, 120, 9, 3, 7, 1, 0, 1, 2, 6, 224, 47, 239, 220, 3, 128, 146, 6, 7, 223, 71, 195, 137, 234, 96, 0, 249, 8, 2, 1, 0, 247, 9, 5, 8, 2, 1, 0, 247, 64, 0, 64, 0, 64, 0, 64, 0, 246, 10, 4, 32, 8, 2, 1, 23, 8, 2, 1, 24, 247, 0, 25, 0, 22, 2, 1, 21, 247, 64, 26, 10, 10, 223, 10, 5, 208, 8, 2, 1, 27, 247, 64, 28, 64, 4, 160, 64, 29, 0, 30, 2, 1, 31, 247, 4, 13, 5, 75, 12, 108, 108, 115, 110, 116, 101, 115, 116, 102, 105, 108, 101, 250, 9, 34, 1, 191, 192, 65, 63, 128, 64, 223, 224, 0, 160, 1, 159, 255, 192, 32, 0, 239, 240, 0, 0, 208, 0, 1, 207, 255, 255, 224, 16, 0, 0, 247, 248, 0, 0, 0, 232, 0, 0, 1, 231, 255, 255, 255, 240, 8, 0, 0, 0, 251, 252, 0, 0, 0, 0, 244, 0, 0, 0, 1, 243, 255, 255, 255, 255, 248, 4, 0, 0, 0, 0, 253, 254, 0, 0, 0, 0, 0, 250, 0, 0, 0, 0, 1, 249, 255, 255, 255, 255, 255, 252, 2, 0, 0, 0, 0, 0, 254, 255, 0, 0, 0, 0, 0, 0, 253, 0, 0, 0, 0, 0, 1, 252, 255, 255, 255, 255, 255, 255, 254, 1, 0, 0, 0, 0, 0, 0, 255, 255, 128, 0, 0, 0, 0, 0, 0, 254, 128, 0, 0, 0, 0, 0, 1, 254, 127, 255, 255, 255, 255, 255, 255, 255, 0, 128, 0, 0, 0, 0, 0, 0, 255, 128, 0, 0, 0, 0, 0, 0, 1, 255, 127, 255, 255, 255, 255, 255, 255, 255, 9, 17, 12, 127, 128, 128, 191, 255, 192, 64, 0, 223, 255, 255, 224, 32, 0, 0, 239, 255, 255, 255, 240, 16, 0, 0, 0, 247, 255, 255, 255, 255, 248, 8, 0, 0, 0, 0, 251, 255, 255, 255, 255, 255, 252, 4, 0, 0, 0, 0, 0, 253, 255, 255, 255, 255, 255, 255, 254, 2, 0, 0, 0, 0, 0, 0, 254, 255, 255, 255, 255, 255, 255, 255, 255, 1, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 10, 5, 168, 10, 3, 0, 12, 131, 120, 131, 120, 131, 120, 4, 224, 131, 120, 72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 46, 32, 228, 189, 160, 229, 165, 189, 228, 184, 150, 231, 149, 140, 46, 32, 217, 133, 216, 177, 216, 173, 216, 168, 216, 167, 32, 216, 168, 216, 167, 217, 132, 216, 185, 216, 167, 217, 132, 217, 133, 46, 32, 227, 129, 147, 227, 130, 147, 227, 129, 171, 227, 129, 161, 227, 129, 175, 228, 184, 150, 231, 149, 140, 46, 32, 206, 147, 206, 181, 206, 185, 206, 172, 32, 206, 163, 206, 191, 207, 133, 32, 206, 154, 207, 140, 207, 131, 206, 188, 206, 181, 46, 32, 215, 148, 215, 162, 215, 156, 215, 144, 32, 215, 149, 215, 149, 215, 162, 215, 156, 215, 152, 46, 32, 208, 159, 209, 128, 208, 184, 208, 178, 208, 181, 209, 130, 32, 208, 156, 208, 184, 209, 128, 46, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 7, 7, 7, 84, 104, 105, 115, 32, 105, 115, 32, 100, 101, 109, 111, 32, 102, 105, 108, 101, 46, 32, 84, 104, 105, 115, 32, 105, 115, 32, 100, 101, 109, 111, 32, 102, 105, 108, 101, 46, 32, 84, 104, 105, 115, 32, 105, 115, 32, 100, 101, 109, 111, 32, 102, 105, 108, 101, 46, 32, 84, 104, 105, 115, 32, 105, 115, 32, 100, 101, 109, 111, 32, 102, 105, 108, 101, 46>>.

get_exampleMainValue() ->
    TmpFile = "/tmp/llsntestfile",
    ContentFile = "This is demo file. This is demo file. This is demo file. This is demo file.",
    file:write_file(TmpFile, ContentFile),

   {33, null, 888,                      %% Field1, Field2, Field3
    [true, false, true],                %% Field4
    3.141596,                           %% Field5
    "Hello World. 你好世界. مرحبا بالعالم. こんにちは世界. Γειά Σου Κόσμε. העלא וועלט. Привет Мир.",
    {{2015, 4, 15},{16, 56, 39, 678},{0,0}},            %% Field7 = 4 Apr, 2015 16:56:39.678 +0000
    null,                                               %% Field8
    {0, null},                                          %% Field9
    [{0, null}, {0, null}, {0, null}, {0, null}, {0, null}],        %% Field10
    null,                                               %% Field11
    [ {23, {24, null}}, {25, {22, {21, null}}}, null, {26, null}],   %% Field12

    %% two dimensional array with null values
    %% [null, null, [null,null,VALUE,null,VALUE], null,  null, null, null, null, [null,VALUE,null,VALUE], null]
    [null, null,                                        %%
        [null, null, {27, null} , null, {28, null}],    %%
     null, null, null, null, null,                      %%
        [null, {29, null} ,null, {30, {31, null}}],      %%
     null],                                                  %% Field13

    <<8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 7, 7, 7>>,          %% Field14
    ?LLSN_FILE(TmpFile),             %% Field15
    null,                                               %% Field16
    get_signed_numbers(),                               %% Field17
    get_unsigned_numbers(),                             %% Field18
    [null, [888, 888, 888], null, [null, null, null, 888], null]
    }.

get_exampleMainDeclaration() ->
    {
        ?LLSN_TYPE_NUMBER,
        ?LLSN_TYPE_NUMBER,
        ?LLSN_TYPE_UNUMBER,
        {?LLSN_TYPE_ARRAY, ?LLSN_TYPE_BOOL},
        ?LLSN_TYPE_FLOAT,
        ?LLSN_TYPE_STRING,
        ?LLSN_TYPE_DATE,
        ?LLSN_TYPE_DATE,
        {?LLSN_TYPE_STRUCT, {?LLSN_TYPE_NUMBER, {?LLSN_TYPE_POINTER, [8] }}},
        {?LLSN_TYPE_ARRAY, {?LLSN_TYPE_STRUCT, {?LLSN_TYPE_NUMBER, {?LLSN_TYPE_POINTER, [8] }}}},
        {?LLSN_TYPE_ARRAY, {?LLSN_TYPE_STRUCT, {?LLSN_TYPE_NUMBER, {?LLSN_TYPE_POINTER, [8] }}}},
        {?LLSN_TYPE_ARRAYN, {?LLSN_TYPE_STRUCT, {?LLSN_TYPE_NUMBER, {?LLSN_TYPE_POINTER, [8] }}}},
        {?LLSN_TYPE_ARRAYN, {?LLSN_TYPE_ARRAYN, {?LLSN_TYPE_STRUCT, {?LLSN_TYPE_NUMBER, {?LLSN_TYPE_POINTER, [8] }}}}},
        ?LLSN_TYPE_BLOB,
        ?LLSN_TYPE_FILE,
        ?LLSN_TYPE_FILE,
        {?LLSN_TYPE_ARRAY, ?LLSN_TYPE_NUMBER},
        {?LLSN_TYPE_ARRAY, ?LLSN_TYPE_UNUMBER},
        {?LLSN_TYPE_ARRAYN, {?LLSN_TYPE_ARRAYN, ?LLSN_TYPE_UNUMBER}}
    }.

%%check for correct number encoding.
get_signed_numbers() ->
    [
    -64, -63, 63, 64, % 2,1,1,2 bytes
    -8192, -8191, 8191, 8192, % 3,2,2,3 bytes
    -1048576, -1048575, 1048575, 1048576, % 4,3,3,4
    -134217728, -134217727, 134217727, 134217728, % 5,4,4,5
    -17179869184, -17179869183, 17179869183, 17179869184, % 6,5,5,6
    -2199023255552, -2199023255551, 2199023255551, 2199023255552, % 7,6,6,7
    -281474976710656, -281474976710655, 281474976710655, 281474976710656, % 8,7,7,8
    -36028797018963968, -36028797018963967, 36028797018963967, 36028797018963968, % 9,8,8,9
    -9223372036854775807, 9223372036854775807 % 9,9
    ].

get_unsigned_numbers() ->
    [
    127, 128, % 1,2 bytes
    16383, 16384, % 2,3
    2097151, 2097152, % 3,4
    268435455, 268435456, % 4,5
    34359738367, 34359738368, % 5,6
    4398046511103, 4398046511104, % 6,7
    562949953421311, 562949953421312, % 7,8
    72057594037927935, 72057594037927936, % 8,9
    18446744073709551615 % 9 (<<255,255,255,255,255,255,255,255,255>>)
    ].

slow_stream(<<Bin:3/binary-unit:8, Tail/binary>>, null) ->
    case llsn:decode(Bin) of
        {parted, X} ->
            slow_stream(Tail, X);
        Value ->
            Value
    end;

slow_stream(<<Bin:1/binary-unit:8, Tail/binary>>, Opts) ->
    case llsn:decode(continue, Opts, Bin) of
        {parted, X} ->
            slow_stream(Tail, X);
        Value ->
            Value
    end.


