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

-module(llsn_gen).

-include_lib("include/llsn.hrl").

-compile(export_all).


% packet generation opts
-record (gopts, {folder,   % полный путь, куда складывать сгенерированные файлы
                 struct,   % декларативная структура, по которой генерируется пакет.
                           % нужна для отработки POINTER типов.
                 stack,
                 etype   % обычные элементы пакуются [{type, val}], элементы массива просто [val]
                }).

% struct generation opts
-record (gsopts, {maxlength,
                  maxdepth,
                  stack}).


%% LLSN generation limits

%% длина генерируемой строки формируется через указание минимальной длины в 1
%% бай и верхней границы. Верхняя граница задается рандомно через степень 2ки.
%% (от 1 до 16, т.е. - 2, 4, 8, 16 ... 65536). Этим лимитом мы задем максимальную
%% степень двойки. Генерация строк длиной в 65К для тестов только в исключительных
%% случаях. для бытовых тестов достаточно 8
-define (LLSN_GEN_STRING_LIMIT, 4).

%% для блоба общий лимит в 4Гб. этого слишком много для бытовых тестов.
%% 1024 - достаточно
-define (LLSN_GEN_BLOB_LIMIT, 6).

%% генерация файлов также должна быть в разумных пределах. 1Мб будет достаточно.
-define (LLSN_GEN_FILE_SIZELIMIT, 20).
%% генерация имени файла. поскольку генерим юникодные имена, то лимит на
%% количество символов 128 (в байтах - 256). лимит задается в степени двойки
-define (LLSN_GEN_FILE_NAMELIMIT, 7).
%% куда складывать оные
-define (LLSN_GEN_FILE_FOLDER , "/tmp/allyst/gen").
%% лимит на кол-во генерируемых элементов массива. степень двойки
-define (LLSN_GEN_ARRAY_LIMIT, 8).

%% ограничиваем разнообразие типов в генераторе структуры
random_allowed_types() ->
        [
            ?LLSN_TYPE_NUMBER
            % , ?LLSN_TYPE_FLOAT
            , ?LLSN_TYPE_STRING
            , ?LLSN_TYPE_STRUCT
            , ?LLSN_TYPE_BOOL
            , ?LLSN_TYPE_BLOB
            % , ?LLSN_TYPE_FILE
            % , ?LLSN_TYPE_DATE
            , ?LLSN_TYPE_ARRAY
            % , ?LLSN_TYPE_POINTER
        ].

random_signed_number() ->
    NL = 1 bsl crypto:rand_uniform(0,4), % 1byte, 2, 4, 8 num len
    <<Value:NL/big-signed-integer-unit:8>> = crypto:rand_bytes(NL),
    Value.

random_unsigned_number() ->
    NL = 1 bsl crypto:rand_uniform(0,4), % 1byte, 2, 4, 8 num len
    <<Value:NL/big-unsigned-integer-unit:8>> = crypto:rand_bytes(NL),
    Value.


random_string() ->
    % генерим список рандомной длины от 1 до 32767,
    % повышаем ентропию через вложенную генерацию длины разряда (степень двойки),
    % в противном случае значения гуляют в основном в середине диапазона
    Seq = lists:seq(1, crypto:rand_uniform(1,
                1 bsl crypto:rand_uniform(1, ?LLSN_GEN_STRING_LIMIT) ) ),
    % берем диапазон только русских букв.
    % [ crypto:rand_uniform(16#410, 16#44f) || _X  <- Seq].
    [ crypto:rand_uniform(16#00, 16#7f) || _X  <- Seq].

random_float() ->
    NL = 1 bsl crypto:rand_uniform(0,4), % 1byte, 2, 4, 8 num len
    NL1 = 1 bsl crypto:rand_uniform(0,4), % 1byte, 2, 4, 8 num len
    <<A:NL/big-integer-unit:8>> = crypto:rand_bytes(NL),
    <<B:NL1/big-integer-unit:8>> = crypto:rand_bytes(NL1),
    A/(B+0.000001). % div by zero

random_bool() ->
    case crypto:rand_uniform(0,2) of
        0 ->
            false;
        1 ->
            true
    end.

random_blob() ->
    crypto:rand_bytes(crypto:rand_uniform(1, ?LLSN_GEN_BLOB_LIMIT)).

random_date() ->
    % fixme later.
    {D,T} = calendar:universal_time(),
    L = calendar:local_time(),
    {_,{TZ_H,TZ_M,_}} = calendar:time_difference({D,T}, L),
    {TH,TM,TS} = T,
    TMS = crypto:rand_uniform(0,999),
    {D,{TH,TM,TS, TMS},{TZ_H,TZ_M}}.



random_file(Folder) ->
    Seq = lists:seq(1, crypto:rand_uniform(1, 1 bsl crypto:rand_uniform(1, ?LLSN_GEN_FILE_NAMELIMIT))),
    FileName = [ crypto:rand_uniform(16#410, 16#44f) || _X  <- Seq],
    FileNameFull = Folder ++ FileName,

    % создаем этот файл и пишем туда всякий мусор
    {ok, File} = file:open(unicode:characters_to_binary(FileNameFull, utf8), [write, binary]),
    FileSize = crypto:rand_uniform(1, 1 bsl crypto:rand_uniform(1, ?LLSN_GEN_FILE_SIZELIMIT) ),
    RawData = crypto:rand_bytes(FileSize),
    ok = file:write(File, << <<"FILE">>/binary, RawData/binary>>),
    file:close(File),
    FileNameFull.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  GENERATE STRUCT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
random_struct() ->
    random_struct(10, 5).

random_struct(MaxLength, MaxDepth) ->
    Opts = #gsopts{maxlength = MaxLength,
            maxdepth = MaxDepth,
            stack = []},
    Length = crypto:rand_uniform(1, MaxLength+1),
    random_struct_acc([], Length, MaxDepth, Opts).

random_struct_acc(Struct, 0, _Depth, Opts) when length(Opts#gsopts.stack) == 0 ->
    list_to_tuple(Struct);

random_struct_acc(Struct, 0, Depth, Opts) ->
    [ {Parent, Type, Length} | T] = Opts#gsopts.stack,
    NOpts = Opts#gsopts{stack = T},
    if Type == ?LLSN_TYPE_ARRAY orelse Type == ?LLSN_TYPE_ARRAYN ->
            % если это массив, то Struct должен состоять из одного элемента
            [NStruct | _ ] = Struct;
        true ->
            NStruct = Struct
    end,
    random_struct_acc(lists:append(Parent, [{Type, NStruct}]), Length-1, Depth, NOpts);

random_struct_acc(Struct, Length, Depth, Opts) ->
    AllowedTypes = random_allowed_types(),

    Type = lists:nth(crypto:rand_uniform(1, length(AllowedTypes)+1), AllowedTypes),

    case Type of
        ?LLSN_TYPE_STRUCT ->
            if length(Opts#gsopts.stack) < Depth ->
                    Len = crypto:rand_uniform(1, Opts#gsopts.maxlength+1),
                    NOpts = Opts#gsopts{stack = [{Struct, ?LLSN_TYPE_STRUCT, Length} | Opts#gsopts.stack]},
                    random_struct_acc([], Len, Depth-1, NOpts);
                true ->
                    % исчерпали лимит на глубину. теперь только базовые типы. генерим заново
                    random_struct_acc(Struct, Length, Depth, Opts)
            end;

        ?LLSN_TYPE_ARRAY ->
            % рандомизируем - будет ли наш массив поддерживать NULL-значения
            HaveNull = crypto:rand_uniform(0,2),
            NType = if HaveNull == 0 ->
                            ?LLSN_TYPE_ARRAY;
                        true ->
                            ?LLSN_TYPE_ARRAYN
                    end,

            if length(Opts#gsopts.stack) < Depth ->
                    NOpts = Opts#gsopts{stack = [{Struct, NType, Length} | Opts#gsopts.stack]},
                    random_struct_acc([], 1, Depth-1, NOpts);
                ok;
            true ->
                    % исчерпали лимит на глубину. теперь только базовые типы. генерим заново
                    random_struct_acc(Struct, Length, Depth, Opts)
            end;

        ?LLSN_TYPE_POINTER ->
            % FIXME. запилить нормальную поддержку
            random_struct_acc(lists:append(Struct, [{Type,5,[5,5,5,5,5]}]), Length-1, Depth, Opts);

        _ ->
            % базовые типы
            random_struct_acc(lists:append(Struct, [Type]), Length-1, Depth, Opts)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  GENERATE PACKET %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen(Name) ->
    gen(Name, random_struct()).

gen(Name, Struct) ->
    % создаем каталог для сгенерированных файлов
    WD = ?LLSN_GEN_FILE_FOLDER ++ "/" ++ Name ++ "/",
    ok = filelib:ensure_dir(WD),
    Opts = #gopts{folder = WD, struct = Struct, stack = [], etype = ?LLSN_TYPE_STRUCT},
    random_packet(tuple_to_list(Struct), [], Opts).

random_packet([], Packet, Opts) when length(Opts#gopts.stack) == 0 ->
    list_to_tuple(Packet);

random_packet([], Packet, Opts) ->
    [{ParentStruct, Type, EType, ParentPacket}|Stack] = Opts#gopts.stack,
    NOpts = Opts#gopts{stack = Stack, etype = EType},
    case Type of
        ?LLSN_TYPE_ARRAY->
            random_packet(ParentStruct, lists:append(ParentPacket, [Packet]), NOpts);
        ?LLSN_TYPE_ARRAYN->
            random_packet(ParentStruct, lists:append(ParentPacket, [Packet]), NOpts);
        _ ->
            random_packet(ParentStruct, lists:append(ParentPacket, [list_to_tuple(Packet)]), NOpts)
    end;


random_packet([Type | Struct], Packet, Opts) ->
    NullValue = if Opts#gopts.etype == ?LLSN_TYPE_ARRAY; Opts#gopts.etype == ?LLSN_TYPE_STRUCT ->
            false;
        true ->
            X = crypto:rand_uniform(0,5), % вероятность генерации NULL = 20%
            if X == 1 ->
                true;
            true ->
                false
            end
    end,

    case Type of
        {?LLSN_TYPE_STRUCT, Elements} ->
            NOpts = Opts#gopts{stack = [ {Struct, ?LLSN_TYPE_STRUCT, Opts#gopts.etype, Packet} | Opts#gopts.stack],
                    etype = ?LLSN_TYPE_STRUCT },
            random_packet(Elements, [], NOpts);

        {?LLSN_TYPE_ARRAY, ArrayOf} ->
            NStruct = [ArrayOf || _N <- lists:seq(1, crypto:rand_uniform(1, 10) )],

            NOpts = Opts#gopts{stack = [ {Struct, ?LLSN_TYPE_ARRAY, Opts#gopts.etype, Packet} | Opts#gopts.stack],
                    etype = ?LLSN_TYPE_ARRAY },
            random_packet(NStruct, [], NOpts);

        {?LLSN_TYPE_ARRAYN, ArrayOf} ->
            NStruct = lists:map(
                    fun(_) ->
                            XX = crypto:rand_uniform(0,5),
                            if XX == 0 ->
                                    ?LLSN_NULL;
                                true ->
                                    ArrayOf
                            end
                    end
                    , lists:seq(1, crypto:rand_uniform(1,10))),

            NOpts = Opts#gopts{stack = [ {Struct, ?LLSN_TYPE_ARRAYN, Opts#gopts.etype, Packet} | Opts#gopts.stack],
                    etype = ?LLSN_TYPE_ARRAYN },
            random_packet(NStruct, [], NOpts);


        {?LLSN_TYPE_POINTER, _PonterLen, _Ponter} ->
            % FIXME
            ok;

        ?LLSN_TYPE_NUMBER ->
            NPacket = if NullValue ->
                            lists:append(Packet, [ ?LLSN_NULL ]);
                        true ->
                            lists:append(Packet, [ random_signed_number() ])
                      end,
            random_packet(Struct, NPacket, Opts);

        ?LLSN_TYPE_FLOAT ->
            NPacket = if NullValue ->
                            lists:append(Packet, [ ?LLSN_NULL ]);
                        true ->
                            lists:append(Packet, [ random_float() ])
                      end,
            random_packet(Struct, NPacket, Opts);

        ?LLSN_TYPE_STRING ->
            NPacket = if NullValue ->
                            lists:append(Packet, [ ?LLSN_NULL ]);
                        true ->
                            lists:append(Packet, [ random_string() ])
                      end,
            random_packet(Struct, NPacket, Opts);

        ?LLSN_TYPE_BLOB ->
            NPacket = if NullValue ->
                            lists:append(Packet, [ ?LLSN_NULL ]);
                        true ->
                            lists:append(Packet, [ random_blob() ])
                      end,
            random_packet(Struct, NPacket, Opts);

        ?LLSN_TYPE_DATE ->
            NPacket = if NullValue ->
                            lists:append(Packet, [ {?LLSN_TYPE_DATE_NULL, ?LLSN_NULL} ]);
                        true ->
                            lists:append(Packet, [ {Type, random_date()} ])
                      end,
            random_packet(Struct, NPacket, Opts);

        ?LLSN_TYPE_FILE ->
            NPacket = if NullValue ->
                            lists:append(Packet, [ {?LLSN_TYPE_FILE_NULL, ?LLSN_NULL} ]);
                        true ->
                            lists:append(Packet, [ {Type, random_file(Opts#gopts.folder)} ])
                      end,
            random_packet(Struct, NPacket, Opts);

        ?LLSN_TYPE_BOOL ->
            NPacket = if NullValue ->
                            lists:append(Packet, [ ?LLSN_NULL ]);
                        true ->
                            lists:append(Packet, [ random_bool() ])
                      end,
            random_packet(Struct, NPacket, Opts);

        ?LLSN_NULL ->
            random_packet(Struct, lists:append(Packet, [?LLSN_NULL]), Opts)

    end.

