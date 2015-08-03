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

-module(llsn).

-include("llsn.hrl").

-export([encode/2, encode/3, encode/4, encode/5]).

-export([encode_NUMBER/1, encode_UNUMBER/1]).

-export([decode/1, decode/3]).

-export([decode_NUMBER/1, decode_UNUMBER/1, encode_POINTER/2]).
-export ([decode_DATE/1, encode_DATE/1]).


-export ([encode_nullflag_create/1]).

% Tree of the types. Uses for encode and decode data.
-record(typestree, {
    type        :: non_neg_integer(),
    next,       % next 'typestree' item
    prev,       %
    child,      %
    parent,     %
    length      :: non_neg_integer() % length of array/struct
}).

% encode options
-record(eopts, {

    threshold   :: non_neg_integer(), % threshold for the huge data (string, blob, file).
    pid         :: pid(), % send frames of the encoded data to the PID
    frame       :: non_neg_integer(), % frame number
    framesize   :: non_neg_integer(), % size of encoded data
    framelimit  :: non_neg_integer(), % frame size limit. default value
    userdata    :: list(), % using on encoding frames to help identify by PID
    tail        :: list(), % tail packed items exceeds the 'threshold'
    stack       :: list(), % needs for incapsulated structs/arrays
    struct      :: tuple(), % needs for POINTER type processing
    nullflag    :: list(),
    chunk       :: tuple(), % file_chunk struct. for encoding/decoding file purposes
    tt          % tree of the types
}).

% file chunk.
-record(chunkFile, {fileSize :: non_neg_integer(),
                    f, % llsn_file
                    fd % file descriptor
               }).

%% =============================================================================
%% Encoding
%% =============================================================================
% with no framing and default threshold

encode(Packet, Struct) when is_tuple(Packet) and is_tuple(Struct) ->
    Options = #eopts{
        threshold       = ?LLSN_DEFAULT_THRESHOLD,
        framelimit      = 0,
        frame           = 1,
        tail            = [],
        stack           = [],
        nullflag        = ?LLSN_NULL,
        chunk           = ?LLSN_NULL,
        tt              = typesTree(new)
    },
    encode_ext(Packet, Struct, Options).

% with framing and default threshold
encode(Packet, Struct, PID) when is_pid(PID)
        and is_tuple(Packet) and is_tuple(Struct) ->
    Options = #eopts{
        threshold       = ?LLSN_DEFAULT_THRESHOLD,
        framelimit      = ?LLSN_DEFAULT_FRAME_LIMIT,
        framesize       = 0,
        frame           = 1,
        tail            = [],
        stack           = [],
        pid             = PID,
        nullflag        = ?LLSN_NULL,
        chunk           = ?LLSN_NULL,
        tt              = typesTree(new)
    },
    encode_ext(Packet, Struct, Options);


% no framing and custom threshold
encode(Packet, Struct, Threshold) when is_integer(Threshold)
        and is_tuple(Packet) and is_tuple(Struct) ->
    Options = #eopts{
        threshold       = Threshold,
        framelimit      = 0,
        framesize       = 0,
        frame           = 1,
        tail            = [],
        stack           = [],
        nullflag        = ?LLSN_NULL,
        chunk           = ?LLSN_NULL,
        tt              = typesTree(new)
    },
    encode_ext(Packet, Struct, Options).

encode(Packet, Struct, PID, UserData) when is_pid(PID)
        and is_tuple(Packet) and is_tuple(Struct)
        and is_list(UserData) ->
    Options = #eopts{
        threshold       = ?LLSN_DEFAULT_THRESHOLD,
        framelimit      = ?LLSN_DEFAULT_FRAME_LIMIT,
        framesize       = 0,
        frame           = 1,
        tail            = [],
        stack           = [],
        pid             = PID,
        userdata        = UserData,
        nullflag        = ?LLSN_NULL,
        chunk           = ?LLSN_NULL,
        tt              = typesTree(new)
    },
    encode_ext(Packet, Struct, Options);

% framing and custom framelimit
encode(Packet, Struct, PID, FrameLimit) when is_pid(PID)
        and is_integer(FrameLimit)
        and is_tuple(Packet) and is_tuple(Struct) ->
    Options = #eopts{
        threshold       = ?LLSN_DEFAULT_THRESHOLD,
        framelimit      = FrameLimit,
        framesize       = 0,
        frame           = 1,
        tail            = [],
        stack           = [],
        pid             = PID,
        nullflag        = ?LLSN_NULL,
        chunk           = ?LLSN_NULL,
        tt              = typesTree(new)
    },
    encode_ext(Packet, Struct, Options).

% framing and custom framelimit
encode(Packet, Struct, PID, FrameLimit, UserData) when is_pid(PID)
        and is_integer(FrameLimit) and is_tuple(Packet)
        and is_tuple(Struct) and is_list(UserData)->
    Options = #eopts{
        threshold       = ?LLSN_DEFAULT_THRESHOLD,
        framelimit      = FrameLimit,
        framesize       = 0,
        frame           = 1,
        tail            = [],
        stack           = [],
        pid             = PID,
        userdata        = UserData,
        nullflag        = ?LLSN_NULL,
        chunk           = ?LLSN_NULL,
        tt              = typesTree(new)
    },
    encode_ext(Packet, Struct, Options).


% Start encoding.
encode_ext(Packet, Struct, #eopts{threshold = Threshold} = Opts) ->
    P   = tuple_to_list(Packet),
    Len = length(P),
    Version = 1,
    {LenBin, LenBinLen} = encode_UNUMBER(Len),
    Bin = <<Version:4/big-unsigned-integer, Threshold:12/big-unsigned-integer, LenBin/binary>>,

    Opts1 = Opts#eopts{
            framesize    = 2 + LenBinLen, % first 2 bytes for the version and threshold
            struct       = Struct
            },

    encode_ext(P, tuple_to_list(Struct), Bin, Opts1).

% stack processing is done. work with tail...
encode_ext([], Struct, Bin, Opts) when Opts#eopts.stack == [] ->
    case Opts#eopts.tail of
        [] ->
            % finished
            if is_pid(Opts#eopts.pid) ->
                Opts#eopts.pid ! {done, Opts#eopts.frame, Opts#eopts.framesize, Bin, Opts#eopts.userdata};
            true ->
                Bin
            end;
        [{file, #llsn_file{origin = FileOrigin} = File } | Tail] ->
            {ok, FD}        = file:open(FileOrigin, [read, binary]),
            Chunk           = #chunkFile{fd = FD},
            {Bin1, Opts1}   = encode_FILE(File, Bin, Opts#eopts{chunk = Chunk, tail = Tail}),
            encode_ext([], Struct, Bin1, Opts1);

        [{data, Data} | Tail] ->
            {Bin1, Opts1}   = framing(Bin, Opts, Data, byte_size(Data)),
            Opts2           = Opts1#eopts{tail = Tail},
            encode_ext([], Struct, Bin1, Opts2)
    end;

% stack processing...
encode_ext([], _, Bin, Opts) ->
    [{Packet, Struct, NullFlag} | Stack] = Opts#eopts.stack,
    TT    = typesTree(parent, Opts#eopts.tt),
    NOpts = Opts#eopts{stack = Stack, nullflag = NullFlag,
                       tt = typesTree(next, TT)},
    encode_ext(Packet, Struct, Bin, NOpts);

encode_ext([Value | Packet], {?LLSN_TYPE_POINTER, Pointer}, Bin, Opts) ->
    PStruct = encode_POINTER(Opts#eopts.struct, Pointer),
    encode_ext([Value|Packet], PStruct, Bin, Opts);

encode_ext([Value | Packet], [{?LLSN_TYPE_POINTER, Pointer} | Struct], Bin, Opts) ->
    PStruct = encode_POINTER(Opts#eopts.struct, Pointer),
    encode_ext([Value|Packet], [PStruct|Struct], Bin, Opts);


% process null value
encode_ext([?LLSN_NULL|Packet], TypeStruct, Bin, Opts)
                            when Opts#eopts.nullflag == ?LLSN_NULL ->
    case TypeStruct of
        [Type|Struct] ->
            pass;
        Type ->
            Struct = Type
    end,

    TT = Opts#eopts.tt,
    case TT#typestree.type of
        ?LLSN_TYPE_UNDEFINED ->
            case Type of
                {Type1, _} ->
                    Type2 = ?LLSN_TYPE_UNDEFINED_NULL - Type1;
                Type1 ->
                    Type2 = ?LLSN_TYPE_UNDEFINED_NULL - Type
            end,

            {Bin1, Opts1} = framing(Bin, Opts, <<Type2/big-unsigned-integer>>, 1),

            TT1 = Opts1#eopts.tt#typestree{type = Type1},
            Opts2 = Opts1#eopts{tt = typesTree(next, TT1)},

            encode_ext(Packet, Struct, Bin1, Opts2);
        _ ->
            TT1 = typesTree(next, TT),
            encode_ext(Packet, Struct, Bin, Opts#eopts{tt = TT1})
    end;

encode_ext([?LLSN_NULL|Packet], TypeStruct, Bin, Opts) ->

    case TypeStruct of
        [_|Struct] ->
            pass;
        Struct ->
            pass
    end,

    {BinNullFlag, BinNullFlagSize, NullFlag} = encode_nullflag(Opts#eopts.nullflag),

    TT = Opts#eopts.tt,
    Opts1 = Opts#eopts{tt = typesTree(next, TT), nullflag = NullFlag},

    if BinNullFlag == <<>> ->
        encode_ext(Packet, Struct, Bin, Opts1);
    true ->
        {Bin1, Opts2} = framing(Bin, Opts1, BinNullFlag, BinNullFlagSize),
        encode_ext(Packet, Struct, Bin1, Opts2)
    end;

% main encoding process routine
encode_ext([Value|Packet], TypeStruct, Bin, Opts) ->
    {BinNullFlag, BinNullFlagSize, NullFlag} = encode_nullflag(Opts#eopts.nullflag),

    case TypeStruct of
        [Type|Struct] ->
            pass;
        Type ->
            Struct = Type
    end,

    case Opts#eopts.tt#typestree.type of
        ?LLSN_TYPE_UNDEFINED when is_tuple(Type) ->
            % array and struct are presents as a tuple. for example, array of
            % numbers looks like {?LLSN_TYPE_ARRAY, ?LLSN_TYPE_NUMBER} and
            % for the struct it looks {?LLSN_TYPE_STRUCT, [?LLSN_TYPE_UNUMBER, ?LLSN_TYPE_STRING]}
            {Type1, _} = Type,
            BinType = <<Type1/big-unsigned-integer>>,
            BinTypeLen = 1,
            TT = Opts#eopts.tt#typestree{type = Type1};

        ?LLSN_TYPE_UNDEFINED  ->
            BinType = <<Type/big-unsigned-integer>>,
            BinTypeLen = 1,
            TT = Opts#eopts.tt#typestree{type = Type};
        _ ->
            BinType = <<>>, BinTypeLen = 0,
            TT = Opts#eopts.tt
    end,

    case Type of
        ?LLSN_TYPE_NUMBER ->
            {BinValue, BinValueLen} = encode_NUMBER(Value),
            TT1     = typesTree(next, TT),
            Opts1   = Opts#eopts{tt = TT1, nullflag = NullFlag},
            Bin1    = <<BinNullFlag/binary, BinType/binary, BinValue/binary>>,
            Bin1Len = BinNullFlagSize + BinTypeLen + BinValueLen,
            {Bin2, Opts2} = framing(Bin, Opts1, Bin1, Bin1Len),
            encode_ext(Packet, Struct, Bin2, Opts2);

        ?LLSN_TYPE_UNUMBER ->
            {BinValue, BinValueLen} = encode_UNUMBER(Value),
            TT1     = typesTree(next, TT),
            Opts1   = Opts#eopts{tt = TT1, nullflag = NullFlag},
            Bin1    = <<BinNullFlag/binary, BinType/binary, BinValue/binary>>,
            Bin1Len = BinNullFlagSize + BinTypeLen + BinValueLen,
            {Bin2, Opts2} = framing(Bin, Opts1, Bin1, Bin1Len),
            encode_ext(Packet, Struct, Bin2, Opts2);

        ?LLSN_TYPE_FLOAT ->
            {BinValue, BinValueLen} = encode_FLOAT(Value),
            TT1     = typesTree(next, TT),
            Opts1   = Opts#eopts{tt = TT1, nullflag = NullFlag},
            Bin1    = <<BinNullFlag/binary, BinType/binary, BinValue/binary>>,
            Bin1Len = BinNullFlagSize + BinTypeLen + BinValueLen,
            {Bin2, Opts2} = framing(Bin, Opts1, Bin1, Bin1Len),
            encode_ext(Packet, Struct, Bin2, Opts2);

        ?LLSN_TYPE_STRING ->
            {BinValue, BinValueLen, Opts1} = encode_STRING(Value, Opts),
            TT1     = typesTree(next, TT),
            Opts2   = Opts1#eopts{tt = TT1, nullflag = NullFlag},
            Bin1    = <<BinNullFlag/binary, BinType/binary, BinValue/binary>>,
            Bin1Len = BinNullFlagSize + BinTypeLen + BinValueLen,
            {Bin2, Opts3} = framing(Bin, Opts2, Bin1, Bin1Len),
            encode_ext(Packet, Struct, Bin2, Opts3);

        ?LLSN_TYPE_BOOL ->
            {BinValue, BinValueLen} = encode_BOOL(Value),
            TT1     = typesTree(next, TT),
            Opts1   = Opts#eopts{tt = TT1, nullflag = NullFlag},
            Bin1    = <<BinNullFlag/binary, BinType/binary, BinValue/binary>>,
            Bin1Len = BinNullFlagSize + BinTypeLen + BinValueLen,
            {Bin2, Opts2} = framing(Bin, Opts1, Bin1, Bin1Len),
            encode_ext(Packet, Struct, Bin2, Opts2);

        ?LLSN_TYPE_BLOB ->
            {BinValue, BinValueLen, Opts1} = encode_BLOB(Value, Opts),
            TT1     = typesTree(next, TT),
            Opts2   = Opts1#eopts{tt = TT1, nullflag = NullFlag},
            Bin1    = <<BinNullFlag/binary, BinType/binary, BinValue/binary>>,
            Bin1Len = BinNullFlagSize + BinTypeLen + BinValueLen,
            {Bin2, Opts3} = framing(Bin, Opts2, Bin1, Bin1Len),
            encode_ext(Packet, Struct, Bin2, Opts3);

        ?LLSN_TYPE_DATE ->
            {BinValue, BinValueLen} = encode_DATE(Value),
            TT1     = typesTree(next, TT),
            Opts1   = Opts#eopts{tt = TT1, nullflag = NullFlag},
            Bin1    = <<BinNullFlag/binary, BinType/binary, BinValue/binary>>,
            Bin1Len = BinNullFlagSize + BinTypeLen + BinValueLen,
            {Bin2, Opts2} = framing(Bin, Opts1, Bin1, Bin1Len),
            encode_ext(Packet, Struct, Bin2, Opts2);

        ?LLSN_TYPE_FILE ->
            TT1     = typesTree(next, TT),
            Opts1   = Opts#eopts{tt = TT1, nullflag = NullFlag},
            Bin1    = <<BinNullFlag/binary, BinType/binary>>,
            Bin1Len = BinNullFlagSize + BinTypeLen,
            {Bin2, Opts2} = framing(Bin, Opts1, Bin1, Bin1Len),
            {Bin3, Opts3} = encode_FILE(Value, Bin2, Opts2),
            encode_ext(Packet, Struct, Bin3, Opts3);

        {?LLSN_TYPE_STRUCT, ValueStruct} ->
            StructList  = tuple_to_list(ValueStruct),
            StructLen   = length(StructList),

            case TT#typestree.length of
                ?LLSN_NULL ->
                    TT1 = typesTree(child, TT#typestree{length = StructLen}),
                    {StructLenBin, StructLenBinLen} = encode_UNUMBER(StructLen);
                _ ->
                    % the length of struct is already encoded early
                    TT1 = typesTree(child, TT),
                    {StructLenBin, StructLenBinLen} = {<<>>, 0}
            end,

            if TT1#typestree.type == ?LLSN_TYPE_UNDEFINED ->
                NullFlagNew = ?LLSN_NULL;
            true ->
                NullFlagNew = encode_nullflag_create(tuple_to_list(Value))
            end,

            Bin1    = <<BinNullFlag/binary, BinType/binary, StructLenBin/binary>>,
            Bin1Len = BinNullFlagSize + BinTypeLen + StructLenBinLen,

            Opts1 = Opts#eopts{stack = [{Packet, Struct, NullFlag}| Opts#eopts.stack],
                                  tt = TT1,
                            nullflag = NullFlagNew},

            {Bin2, Opts2} = framing(Bin, Opts1, Bin1, Bin1Len),

            encode_ext(tuple_to_list(Value), StructList, Bin2, Opts2);


        {Array, ArrayOf} when Array == ?LLSN_TYPE_ARRAY; % array or array with null values
                                 Array == ?LLSN_TYPE_ARRAYN ->
            ArrayLen    = length(Value),
            TT1         = typesTree(child, TT#typestree{length = ArrayLen}),
            TT2         = TT1#typestree{next = self},
            {ArrayLenBin, ArrayLenBinLen} = encode_UNUMBER(ArrayLen),

            if Array == ?LLSN_TYPE_ARRAYN ->
                NullFlagNew = encode_nullflag_create(Value);
            true ->
                NullFlagNew = ?LLSN_NULL
            end,

            Bin1    = <<BinNullFlag/binary, BinType/binary, ArrayLenBin/binary>>,
            Bin1Len = BinNullFlagSize + BinTypeLen + ArrayLenBinLen,

            Opts1 = Opts#eopts{stack = [{Packet, Struct, NullFlag}| Opts#eopts.stack],
                                  tt = TT2,
                            nullflag = NullFlagNew},

            {Bin2, Opts2} = framing(Bin, Opts1, Bin1, Bin1Len),

            encode_ext(Value, ArrayOf, Bin2, Opts2)

    end.


encode_NUMBER(Value) when 0 > Value ->
    NValue = Value * -1,
    encode_number(Value, NValue);

encode_NUMBER(Value) ->
    encode_number(Value, Value).

% 2^7 - 1
encode_number(Value, NValue) when NValue band 16#3f == NValue ->
    { <<16#0:1/big-unsigned-integer,Value:7/big-signed-integer>>, 1 };

% 2^14 -1
encode_number(Value, NValue) when NValue band 16#1fff == NValue ->
    { <<16#2:2/big-unsigned-integer,Value:14/big-signed-integer>>, 2 };

% 2^21 -1
encode_number(Value, NValue) when NValue band 16#fffff == NValue ->
    { <<16#6:3/big-unsigned-integer,Value:21/big-signed-integer>>, 3 };

% 2^28 -1
encode_number(Value, NValue) when NValue band 16#7ffffff == NValue ->
    { <<16#e:4/big-unsigned-integer,Value:28/big-signed-integer>>, 4};

% 2^35 -1
encode_number(Value, NValue) when NValue band 16#3ffffffff == NValue ->
    { <<16#1e:5/big-unsigned-integer,Value:35/big-signed-integer>>, 5 };

% 2^42 -1
encode_number(Value, NValue) when NValue band 16#1ffffffffff == NValue ->
    { <<16#3e:6/big-unsigned-integer,Value:42/big-signed-integer>>, 6 };

% 2^49 -1
encode_number(Value, NValue) when NValue band 16#ffffffffffff == NValue ->
    { <<16#7e:7/big-unsigned-integer,Value:49/big-signed-integer>>, 7 };

% 2^56 -1
encode_number(Value, NValue) when NValue band 16#7fffffffffffff == NValue ->
    { <<16#fe:8/big-unsigned-integer,Value:56/big-signed-integer>>, 8 };

% def
encode_number(Value, NValue) ->
    { <<16#ff:8/big-unsigned-integer,Value:64/big-signed-integer>>, 9 }.

% 2^7 - 1
encode_UNUMBER(Value) when Value band 16#7f == Value ->
    { <<16#0:1/big-unsigned-integer,Value:7/big-unsigned-integer>>, 1 };

% 2^14 -1
encode_UNUMBER(Value) when Value band 16#3fff == Value ->
    { <<16#2:2/big-unsigned-integer,Value:14/big-unsigned-integer>>, 2 };

% 2^21 -1
encode_UNUMBER(Value) when Value band 16#1fffff == Value ->
    { <<16#6:3/big-unsigned-integer,Value:21/big-unsigned-integer>>, 3 };

% 2^28 -1
encode_UNUMBER(Value) when Value band 16#fffffff == Value ->
    { <<16#e:4/big-unsigned-integer,Value:28/big-unsigned-integer>>, 4};

% 2^35 -1
encode_UNUMBER(Value) when Value band 16#7ffffffff == Value ->
    { <<16#1e:5/big-unsigned-integer,Value:35/big-unsigned-integer>>, 5 };

% 2^42 -1
encode_UNUMBER(Value) when Value band 16#3ffffffffff == Value ->
    { <<16#3e:6/big-unsigned-integer,Value:42/big-unsigned-integer>>, 6 };

% 2^49 -1
encode_UNUMBER(Value) when Value band 16#1ffffffffffff == Value ->
    { <<16#7e:7/big-unsigned-integer,Value:49/big-unsigned-integer>>, 7 };

% 2^56 -1
encode_UNUMBER(Value) when Value band 16#ffffffffffffff == Value ->
    { <<16#fe:8/big-unsigned-integer,Value:56/big-unsigned-integer>>, 8 };

% def
encode_UNUMBER(Value) ->
    { <<16#ff:8/big-unsigned-integer,Value:64/big-unsigned-integer>>, 9 }.


encode_float(Value, N, Pow) ->
    V  = Value * Pow,
    TV = trunc(V),
    if TV == V ->
            {N, TV};
        true ->
            encode_float(Value, N+1, Pow*10)
    end.

encode_FLOAT(Value) ->
    {P,M}    = encode_float(Value, 1, 10),
    {BP, PL} = encode_UNUMBER(P),
    {BM, ML} = encode_NUMBER(M),
    {<<BP/binary,BM/binary>>, PL+ML}.


% FYI: how to get a GMT offset
% calendar:time_difference(calendar:universal_time(), calendar:local_time()).
% {_, {HourOffset, MinOffset, _}} = {0,{4,0,0}}

encode_DATE({{Year, Month, Day},
             {Hour, Min, Sec, MSec},
             {OffsetHour, OffsetMin}}) ->
    { <<Year:16/big-integer,
        Month:4/big-unsigned-integer,
        Day:5/big-unsigned-integer,
        Hour:5/big-unsigned-integer,
        Min:6/big-unsigned-integer,
        Sec:6/big-unsigned-integer,
        MSec:10/big-unsigned-integer,
        OffsetHour:6/big-integer,
        OffsetMin:6/big-unsigned-integer>>, 8 }.

encode_BOOL(true) -> {<<1:8/big-unsigned-integer>>, 1};
encode_BOOL(_)    -> {<<0:8/big-unsigned-integer>>, 1}.

encode_POINTER([S|_], [0]) ->
    S;
encode_POINTER(Struct, [0]) ->
    Struct;
encode_POINTER(Struct, [C|Coord]) ->
    case C of
        0 when is_tuple(Struct) ->
            {_, Elements} = Struct,
            encode_POINTER(Elements, Coord);
        0 ->
            [{_, Elements}|_] = Struct,
            encode_POINTER(Elements, Coord);
        N when is_tuple(Struct) ->
            [_|S] = tuple_to_list(Struct),
            encode_POINTER(S, [N-1|Coord]);
        N ->
            [_|S] = Struct,
            encode_POINTER(S, [N-1|Coord])
    end.

encode_STRING(Value, Opts) ->
    BinValue = unicode:characters_to_binary(Value, utf8),
    if byte_size(BinValue) > ?LLSN_TYPE_STRING_LIMIT ->
        throw("The limit of string length is exceeded");
    true ->
        encode_BLOB(BinValue, Opts)
    end.

encode_BLOB(Value, _) when byte_size(Value) > ?LLSN_TYPE_BLOB_LIMIT ->
    throw("The limit of blob length is exceeded");

encode_BLOB(Value, Opts) ->
    ValueSize = byte_size(Value),

    {ValueSizeBin, ValueSizeBinLen} = encode_UNUMBER(ValueSize),

    if ValueSize > Opts#eopts.threshold, Opts#eopts.threshold > 0 ->
            Opts1 = Opts#eopts{tail = lists:append(Opts#eopts.tail, [ {data, Value} ])},
            {<<ValueSizeBin/binary>>, ValueSizeBinLen, Opts1};
    true ->
            {<<ValueSizeBin/binary, Value/binary>>,
                ValueSizeBinLen + ValueSize, Opts}
    end.

encode_FILE(#llsn_file{name = FileName, origin = FileOrigin} = Value, Bin, Opts)
                        when Opts#eopts.chunk == ?LLSN_NULL ->

    FileSize    = filelib:file_size(FileOrigin),
    {FileSizeBin, FileSizeBinLen} = encode_UNUMBER(FileSize),

    FileNameBin     = unicode:characters_to_binary(FileName, utf8),
    FileNameBinSize = byte_size(FileNameBin),
    {FileNameBinSizeBin, FileNameBinSizeBinLen} = encode_UNUMBER(FileNameBinSize),

    Bin1        = <<FileSizeBin/binary, FileNameBinSizeBin/binary, FileNameBin/binary>>,
    Bin1Len     = FileSizeBinLen + FileNameBinSizeBinLen + FileNameBinSize,

    {Bin2, Opts1} = framing(Bin, Opts, Bin1, Bin1Len),

    if FileSize > Opts1#eopts.threshold, Opts1#eopts.threshold > 0 ->
        Opts2       = Opts1#eopts{tail = lists:append(Opts1#eopts.tail, [ {file, Value} ])},
        {Bin2, Opts2};
    true ->
        {ok, FD} = file:open(FileOrigin, [read, binary]),

        Chunk = #chunkFile{
            fileSize = FileSize,
            fd       = FD
        },

        encode_FILE(Value, Bin2, Opts1#eopts{chunk = Chunk})
    end;

encode_FILE(Value, Bin, #eopts{chunk = Chunk} = Opts) ->
    case file:read(Chunk#chunkFile.fd, ?LLSN_DEFAULT_BUFFER_SIZE) of
        {ok, Data} ->
            DataSize        = byte_size(Data),
            {Bin1, Opts1}   = framing(Bin, Opts, Data, DataSize),
            encode_FILE(Value, Bin1, Opts1);
        eof ->
            file:close(Chunk#chunkFile.fd),
            {Bin, Opts};
        {error, Reason} ->
            throw("Error file reading: " ++ Reason)
    end.

%% =============================================================================
%% Decoding
%% =============================================================================

% decode options
-record(dopts, {threshold :: non_neg_integer(),
                tail,
                stack,
                tt, % typestree
                nullflag,
                chunk, % uses for partial data of string,blob,file
                dir
               }).


% Support version 1
decode(<<V:4/big-unsigned-integer, Threshold:12/big-unsigned-integer,
                Data/binary>>) when V == 1 ->
    case decode_UNUMBER(Data) of
        {parted, _} ->
            {malformed, Data};

        {N, Data1} ->
            Opts = #dopts{threshold = Threshold,
                    stack     = [],
                    tail      = [],
                    tt        = typesTree(new),
                    chunk     = null,
                    dir       = ?LLSN_DEFAULT_DIR},
            decode_ext([], Data1, N, Opts)
    end;

% unsupported version of LLSN
decode(<<V:4/big-unsigned-integer, _:12/big-unsigned-integer,
                Data/binary>>) when V > 1 ->
    {unsupported, Data};


decode(Data) ->
    {malformed, Data}.

decode(continue, {Value, Data, N, Opts}, NextData) ->
    decode_ext(Value, <<Data/binary,NextData/binary>>, N, Opts).


% stack processing is done. tail processing
decode_ext(Value, Data, 0, Opts) when Opts#dopts.stack == [] ->
    case Opts#dopts.tail of
        [] ->
            % done
            list_to_tuple(lists:reverse(Value));
        [{string, [X|Y], Len} | Tail] ->
            case Data of
                <<BinStrValue:Len/binary-unit:8, DataTail/binary>> ->
                    StrValue = unicode:characters_to_list(BinStrValue, utf8),
                    NOpts = Opts#dopts{tail = Tail},
                    % the first level of Value is reversed. fix the first element
                    % of the XY list
                    NewValue = tail_replacexy([length(Value) - X + 1|Y], Value, StrValue),
                    decode_ext(NewValue, DataTail, 0, NOpts);
                _ ->
                    {parted, {Value, Data, 0, Opts}}
            end;

        [{blob, [X|Y], Len} | Tail] ->

            case Data of
                <<BinValue:Len/binary-unit:8, DataTail/binary>> ->
                    NOpts = Opts#dopts{tail = Tail},
                    NewValue = tail_replacexy([length(Value) - X + 1|Y], Value, BinValue),
                    decode_ext(NewValue, DataTail, 0, NOpts);
                _ ->
                    {parted, {Value, Data, 0, Opts}}
            end;

        [{file, [X|Y], Chunk} | Tail] ->
            case decode_FILE(0, Data, Opts#dopts{tail = Tail, threshold = 0, chunk = Chunk}) of
                {parted, DataTail, NOpts} ->
                    NewTail = [{file, [X|Y], NOpts#dopts.chunk} | Tail],
                    {parted, {Value, DataTail, 0, NOpts#dopts{tail = NewTail}}};

                {FileValue, DataTail, NOpts} ->
                    NewValue = tail_replacexy([length(Value) - X + 1|Y], Value, FileValue),
                    decode_ext(NewValue, DataTail, 0, NOpts)
            end
    end;

% stack processing
decode_ext(Value, Data, 0, Opts) ->
    [{StackValue, StackN, NullFlag} | StackT] = Opts#dopts.stack,

    TT      =   typesTree(parent, Opts#dopts.tt),

    NValue  =   case TT#typestree.type of
                    ?LLSN_TYPE_STRUCT ->
                        [list_to_tuple(lists:reverse(Value)) | StackValue];
                    _ ->
                        [lists:reverse(Value) | StackValue]
                end,

    NOpts   =   Opts#dopts{stack = StackT,
                           tt    = typesTree(next, TT),
                           nullflag = NullFlag},

    decode_ext(NValue, Data, StackN, NOpts);


decode_ext(Value, Data, N, Opts) ->
    case decode_nullflag(Data, N, Opts) of
        % null value. skip it.
        {true, Data1, Opts1} ->
            T1 = Opts1#dopts.tt,
            Opts2   = Opts1#dopts{tt = typesTree(next, T1)},
            decode_ext([?LLSN_NULL|Value], Data1, N-1, Opts2);

        % Not enough data to decode packet.
        parted ->
            {parted, {Value, Data, N, Opts}};

        {_, <<>>, _} ->
            {parted, {Value, Data, N, Opts}};

        % decode value
        {false, Data1, Opts1} ->
            T = Opts1#dopts.tt,
            if T#typestree.type == ?LLSN_TYPE_UNDEFINED ->
                <<Type:8/big-unsigned-integer, Data2/binary>> = Data1,
                Opts2   = Opts1#dopts{tt = T#typestree{type = Type}};
            true ->
                Type    = T#typestree.type,
                Data2   = Data1,
                Opts2   = Opts1
            end,

            TT = typesTree(next, Opts2#dopts.tt),

            case Type of
                ?LLSN_TYPE_NUMBER ->
                    case decode_NUMBER(Data2) of
                        {parted, _} ->
                            {parted, {Value, Data, N, Opts}};
                        {NValue, Data3} ->
                            decode_ext([NValue|Value], Data3, N-1, Opts2#dopts{tt = TT})
                    end;

                ?LLSN_TYPE_UNUMBER ->
                    case decode_UNUMBER(Data2) of
                        {parted, _} ->
                            {parted, {Value, Data, N, Opts}};
                        {NValue, Data3} ->
                            decode_ext([NValue|Value], Data3, N-1, Opts2#dopts{tt = TT})
                    end;

                ?LLSN_TYPE_FLOAT ->
                    case decode_FLOAT(Data2) of
                        {parted, _} ->
                            {parted, {Value, Data, N, Opts}};
                        {NValue, Data3} ->
                            decode_ext([NValue|Value], Data3, N-1, Opts2#dopts{tt = TT})
                    end;

                ?LLSN_TYPE_STRING ->
                    case decode_STRING(length(Value)+1, Data2, Opts2) of
                        {parted, _, _} ->
                            {parted, {Value, Data, N, Opts}};
                        {tail, Data3, Opts3} ->
                            % FIXME. tail processing
                            decode_ext([tail|Value], Data3, N-1, Opts3#dopts{tt = TT});
                        {NValue, Data3, Opts3} ->
                            decode_ext([NValue|Value], Data3, N-1, Opts3#dopts{tt = TT})
                    end;

                ?LLSN_TYPE_DATE ->
                    case decode_DATE(Data2) of
                        {parted, _} ->
                            {parted, {Value, Data, N, Opts}};
                        {NValue, Data3} ->
                            decode_ext([NValue|Value], Data3, N-1, Opts2#dopts{tt = TT})
                    end;

                ?LLSN_TYPE_BOOL ->
                    case decode_BOOL(Data2) of
                        {parted, _} ->
                            {parted, {Value, Data, N, Opts}};
                        {NValue, Data3} ->
                            decode_ext([NValue|Value], Data3, N-1, Opts2#dopts{tt = TT})
                    end;

                ?LLSN_TYPE_BLOB ->
                    case decode_BLOB(length(Value)+1, Data2, Opts2) of
                        {parted, _, _} ->
                            {parted, {Value, Data, N, Opts}};
                        {tail, Data3, Opts3} ->
                            % FIXME. tail processing
                            decode_ext([tail|Value], Data3, N-1, Opts3#dopts{tt = TT});
                        {NValue, Data3, Opts3} ->
                            decode_ext([NValue|Value], Data3, N-1, Opts3#dopts{tt = TT})
                    end;

                ?LLSN_TYPE_FILE ->
                    case decode_FILE(length(Value)+1, Data2, Opts2) of
                        {parted, _, _} ->
                            {parted, {Value, Data2, N, Opts2}};
                        {tail, Data3, Opts3} ->
                            % FIXME. tail processing
                            decode_ext([tail|Value], Data3, N-1, Opts3#dopts{tt = TT});
                        {NValue, Data3, Opts3} ->
                            decode_ext([NValue|Value], Data3, N-1, Opts3#dopts{tt = TT})
                    end;

                ?LLSN_TYPE_STRUCT ->
                    case decode_STRUCT(Value, N, Data2, Opts2) of
                        parted ->
                            {parted, {Value, Data, N, Opts}};

                        {Data3, NN, Opts3} ->
                            decode_ext([], Data3, NN, Opts3)
                    end;

                ?LLSN_TYPE_ARRAY ->
                    case decode_UNUMBER(Data2) of
                        {parted, _} ->
                            {parted, {Value, Data, N, Opts}};
                        {NN, Data3} ->
                            T0 = Opts2#dopts.tt#typestree{length = NN},
                            T1 = typesTree(child, T0),
                            T2 = T1#typestree{next = self},
                            NOpts = Opts2#dopts{stack = [{Value, N-1, Opts2#dopts.nullflag} | Opts2#dopts.stack],
                                                tt    = T2,
                                                nullflag = ?LLSN_NULL},
                            decode_ext([], Data3, NN, NOpts)
                    end;

                ?LLSN_TYPE_ARRAYN ->
                    case decode_UNUMBER(Data2) of
                        {parted, _} ->
                            {parted, {Value, Data, N, Opts}};
                        {NN, Data3} ->
                            T0 = Opts2#dopts.tt#typestree{length = NN},
                            T1 = typesTree(child, T0),
                            T2 = T1#typestree{next = self},
                            NOpts = Opts2#dopts{stack = [{Value, N-1, Opts2#dopts.nullflag} | Opts2#dopts.stack],
                                                tt = T2,
                                                nullflag = 0},
                            decode_ext([], Data3, NN, NOpts)
                    end;

                Null when Null > ?LLSN_NULL_TYPES  ->
                    T0   = Opts2#dopts.tt,
                    T1 = typesTree(next, T0#typestree{type = ?LLSN_TYPE_UNDEFINED_NULL - Null}),
                    decode_ext([?LLSN_NULL|Value], Data2, N-1, Opts2#dopts{tt = T1})

            end



    end.


%% =============================================================================
%% Numbers
%% =============================================================================
decode_NUMBER(Data) ->
    % decode signed number
    decode_NUMBER(signed, Data).

decode_UNUMBER(Data) ->
    % decode unsigned number
    decode_NUMBER(unsigned, Data).

decode_NUMBER(unsigned, <<2#0:1/big-unsigned-integer,        Num:7/big-unsigned-integer,  Tail/binary>>) -> {Num, Tail};
decode_NUMBER(signed,   <<2#0:1/big-unsigned-integer,        Num:7/big-signed-integer,    Tail/binary>>) -> {Num, Tail};

decode_NUMBER(unsigned, <<2#10:2/big-unsigned-integer,       Num:14/big-unsigned-integer, Tail/binary>>) -> {Num, Tail};
decode_NUMBER(signed,   <<2#10:2/big-unsigned-integer,       Num:14/big-signed-integer,   Tail/binary>>) -> {Num, Tail};

decode_NUMBER(unsigned, <<2#110:3/big-unsigned-integer,      Num:21/big-unsigned-integer, Tail/binary>>) -> {Num, Tail};
decode_NUMBER(signed,   <<2#110:3/big-unsigned-integer,      Num:21/big-signed-integer,   Tail/binary>>) -> {Num, Tail};

decode_NUMBER(unsigned, <<2#1110:4/big-unsigned-integer,     Num:28/big-unsigned-integer, Tail/binary>>) -> {Num, Tail};
decode_NUMBER(signed,   <<2#1110:4/big-unsigned-integer,     Num:28/big-signed-integer,   Tail/binary>>) -> {Num, Tail};

decode_NUMBER(unsigned, <<2#11110:5/big-unsigned-integer,    Num:35/big-unsigned-integer, Tail/binary>>) -> {Num, Tail};
decode_NUMBER(signed,   <<2#11110:5/big-unsigned-integer,    Num:35/big-signed-integer,   Tail/binary>>) -> {Num, Tail};

decode_NUMBER(unsigned, <<2#111110:6/big-unsigned-integer,   Num:42/big-unsigned-integer, Tail/binary>>) -> {Num, Tail};
decode_NUMBER(signed,   <<2#111110:6/big-unsigned-integer,   Num:42/big-signed-integer,   Tail/binary>>) -> {Num, Tail};

decode_NUMBER(unsigned, <<2#1111110:7/big-unsigned-integer,  Num:49/big-unsigned-integer, Tail/binary>>) -> {Num, Tail};
decode_NUMBER(signed,   <<2#1111110:7/big-unsigned-integer,  Num:49/big-signed-integer,   Tail/binary>>) -> {Num, Tail};

decode_NUMBER(unsigned, <<2#11111110:8/big-unsigned-integer, Num:56/big-unsigned-integer, Tail/binary>>) -> {Num, Tail};
decode_NUMBER(signed,   <<2#11111110:8/big-unsigned-integer, Num:56/big-signed-integer,   Tail/binary>>) -> {Num, Tail};

decode_NUMBER(unsigned, <<2#11111111:8/big-unsigned-integer, Num:64/big-unsigned-integer, Tail/binary>>) -> {Num, Tail};
decode_NUMBER(signed,   <<2#11111111:8/big-unsigned-integer, Num:64/big-signed-integer,   Tail/binary>>) -> {Num, Tail};

decode_NUMBER(_,   Data) -> {parted, Data}.

%% =============================================================================
%% Floats
%% =============================================================================
decode_FLOAT(Data) ->
    case decode_UNUMBER(Data) of
        {parted, _} ->
            {parted, Data};
        {Pow, Tail1} ->
            case decode_NUMBER(Tail1) of
                {parted, _} ->
                    {parted, Data};
                {N, Tail2} ->
                    {N/(math:pow(10,Pow)), Tail2}
            end
    end.

%% =============================================================================
%% Strings
%% =============================================================================
decode_STRING(L, Data, Opts) ->
    case decode_UNUMBER(Data) of
        {parted, _}     ->
            {parted, Data, Opts};
        {Len, DataTail} ->
            if Len > Opts#dopts.threshold, Opts#dopts.threshold > 0 ->
                    XY = tail_getxy(L, Opts, []),
                    Opts1 = Opts#dopts{tail = lists:append(Opts#dopts.tail, [ {string, XY, Len} ])},
                    {tail, DataTail, Opts1};
                true ->
                    case DataTail of
                        <<BinStrValue:Len/binary-unit:8, DataTail1/binary>> ->
                            StrValue = unicode:characters_to_list(BinStrValue, utf8),
                            {StrValue, DataTail1, Opts};
                        _ ->
                            {parted, Data, Opts}
                    end
            end
    end.

%% =============================================================================
%% Blobs
%% =============================================================================
decode_BLOB(L, Data, Opts) ->
    case decode_UNUMBER(Data) of
        {parted, _} ->
            {parted, Data, Opts};
        {Len, DataTail} ->
            if Len > Opts#dopts.threshold, Opts#dopts.threshold > 0 ->
                    XY = tail_getxy(L, Opts, []),
                    Opts1 = Opts#dopts{tail = lists:append(Opts#dopts.tail, [ {blob, XY, Len} ])},
                    {tail, DataTail, Opts1};
                true ->
                    case DataTail of
                        <<Value:Len/binary-unit:8, DataTail1/binary>> ->
                            {Value, DataTail1, Opts};
                        _ ->
                            {parted, Data, Opts}
                    end
            end
    end.

decode_FILE(L, Data, Opts) when Opts#dopts.chunk == null ->
    case decode_UNUMBER(Data) of
        {parted, _} ->
            {parted, Data, Opts};
        {FileSize, DataTail} ->
            case decode_UNUMBER(DataTail) of
                {parted, _} ->
                    {parted, Data, Opts};
                {FileNameSize, DataTail1} ->
                    case DataTail1 of
                        <<FileNameBin:FileNameSize/binary-unit:8, DataTail2/binary>> ->
                            TmpFileName = lists:flatten(io_lib:format(?LLSN_DEFAULT_FILEPREFIX"-~p.~p.~p",
                                                                      tuple_to_list(now()))),
                            TmpFile     = Opts#dopts.dir ++ TmpFileName,
                            FileName    = unicode:characters_to_list(FileNameBin, utf8),

                            File = #llsn_file{
                                name    = FileName,
                                origin  = TmpFile
                            },

                            if FileSize > Opts#dopts.threshold, Opts#dopts.threshold > 0 ->
                                Chunk = #chunkFile{
                                    fileSize = FileSize,
                                    f        = File,
                                    fd       = null
                                },
                                XY = tail_getxy(L, Opts, []),
                                Opts1 = Opts#dopts{tail = lists:append(Opts#dopts.tail, [ {file, XY, Chunk} ])},
                                {tail, DataTail2, Opts1};

                            true ->
                                {ok, FD} = file:open(TmpFile, [write, binary]),
                                Chunk = #chunkFile{
                                    fileSize = FileSize,
                                    f        = File,
                                    fd       = FD
                                },
                                decode_FILE(L, DataTail2, Opts#dopts{chunk = Chunk})
                            end;
                        _ ->
                            {parted, Data, Opts}
                    end

            end
    end;

decode_FILE(L, Data, Opts) when Opts#dopts.chunk#chunkFile.fd == null ->

    Chunk   = Opts#dopts.chunk,
    File    = Chunk#chunkFile.f,
    {ok, FD}    = file:open(File#llsn_file.origin, [write, binary]),

    decode_FILE(L, Data, Opts#dopts{chunk = Chunk#chunkFile{fd = FD}} );

decode_FILE(_L, Data, Opts) ->
    Chunk   = Opts#dopts.chunk,
    DSize   = size(Data),
    if DSize < Chunk#chunkFile.fileSize ->
        file:write(Chunk#chunkFile.fd, Data),
        FSize = Chunk#chunkFile.fileSize - DSize,
        Chunk1 = Chunk#chunkFile {fileSize = FSize},

        {parted, <<>>, Opts#dopts{chunk = Chunk1}};
    true ->
        Len = Chunk#chunkFile.fileSize,
        <<FileData:Len/binary-unit:8, Tail/binary>> = Data,
        file:write(Chunk#chunkFile.fd, FileData),
        file:close(Chunk#chunkFile.fd),

        {Chunk#chunkFile.f, Tail, Opts#dopts{chunk = null}}
    end.


%% =============================================================================
%% Dates
%% =============================================================================
% 2B: year. (-32767..32768), sign for AC/BC
%   :4b month (1..12)
%   :5b day of month (1..31)
%   :5b hour (0..23)
%   :6b min (0..59)
%   :6b sec (0..59)
%   :10 msec (0..999)
%   :6b hours offset (signed)
%   :6b min offset (unsigned)
%   -- :48bit
% --
% 8B total
decode_DATE(<<Year:16/big-signed-integer,
                Month:4/big-unsigned-integer,
                Day:5/big-unsigned-integer,
                Hour:5/big-unsigned-integer,
                Min:6/big-unsigned-integer,
                Sec:6/big-unsigned-integer,
                MSec:10/big-unsigned-integer,
                OffsetHour:6/big-integer,
                OffsetMin:6/big-unsigned-integer, DataTail/binary>>) ->
    {{{Year, Month, Day}, {Hour, Min, Sec, MSec}, {OffsetHour, OffsetMin}},
     DataTail};
decode_DATE(Data) ->
    {parted, Data}.

%% =============================================================================
%% Booleans
%% =============================================================================
decode_BOOL(<<0:8/big-unsigned-integer, DataTail/binary>>) -> {false, DataTail};
decode_BOOL(<<1:8/big-unsigned-integer, DataTail/binary>>) -> {true, DataTail};
decode_BOOL(Data)                                          -> {parted, Data}.


%% =============================================================================
%% Structs
%% =============================================================================
decode_STRUCT(Value, N, Data, Opts) when Opts#dopts.tt#typestree.length == ?LLSN_NULL ->

    case decode_UNUMBER(Data) of
        {parted, _} ->
            parted;
        {Len, Data1} ->
            T = Opts#dopts.tt,
            T1 = T#typestree{length = Len},
            % Opts1 = Opts#dopts{tt = T1},
            T2 = typesTree(child, T1),
            NOpts = Opts#dopts{stack = [{Value, N-1, Opts#dopts.nullflag } | Opts#dopts.stack],
                         tt  = T2,
                    nullflag = ?LLSN_NULL},
            {Data1, Len, NOpts}
    end;

decode_STRUCT(Value, N, Data, Opts) ->
    T1 = typesTree(child, Opts#dopts.tt),
    NOpts = Opts#dopts{stack = [{Value, N-1, Opts#dopts.nullflag } | Opts#dopts.stack],
                         tt  = T1,
                    nullflag = 0},
    {Data, Opts#dopts.tt#typestree.length, NOpts}.

%% =============================================================================
%% Helpers
%% =============================================================================

typesTree(new) ->
    #typestree{
        type     = ?LLSN_TYPE_UNDEFINED,

        next     = ?LLSN_NULL,
        prev     = ?LLSN_NULL,

        child    = ?LLSN_NULL,
        parent   = ?LLSN_NULL,

        % nullflag = ?LLSN_NULL,
        length   = ?LLSN_NULL }. % set it true when struct is decoded (all field types are defined)

typesTree(next, Current) when Current#typestree.next == self ->
    Current;

typesTree(next, Current) when Current#typestree.next == ?LLSN_NULL->
    T = typesTree(new),
    T#typestree{prev     = Current,
                parent   = Current#typestree.parent};

typesTree(next, Current) ->
    T = Current#typestree.next,
    NextPrev = Current#typestree{next = ?LLSN_NULL, parent = ?LLSN_NULL},
    T#typestree{prev     = NextPrev,
                parent   = Current#typestree.parent};

typesTree(child, Current) when Current#typestree.child == ?LLSN_NULL ->
    T = typesTree(new),
    T#typestree{parent = Current#typestree{child = ?LLSN_NULL}};

typesTree(child, Current) ->
    T = Current#typestree.child,
    T#typestree{parent = Current#typestree{child = ?LLSN_NULL}};

typesTree(parent, Current) when Current#typestree.prev == ? LLSN_NULL ->
    T = Current#typestree.parent,
    T#typestree{child = Current#typestree{parent = ?LLSN_NULL}};

typesTree(parent, Current) ->
    T = Current#typestree.prev,
    ParentNext = Current#typestree{prev = ?LLSN_NULL, parent = ?LLSN_NULL},
    typesTree(parent, T#typestree{next   = ParentNext,
                                  parent = Current#typestree.parent}).



decode_nullflag(Data, _N, Opts) when Opts#dopts.tt#typestree.parent == ?LLSN_NULL ->
    {false, Data, Opts};

decode_nullflag(Data, N, Opts) when Opts#dopts.nullflag /= ?LLSN_NULL ->
    T   = Opts#dopts.tt,
    Parent = T#typestree.parent,
    Pos = (8 - ((Parent#typestree.length - N) rem 8)),
    case Data of
        <<NF:8/big-unsigned-integer, Data1/binary>> when Pos == 8 ->
            Opts1   = Opts#dopts{nullflag = NF},
            if NF band (1 bsl (Pos - 1)) == 0 ->
                {false, Data1, Opts1};
            true ->
                {true, Data1, Opts1}
            end;

        <<>>  when Pos == 8 ->
            parted;

        _ ->
            if Opts#dopts.nullflag band (1 bsl (Pos - 1)) == 0 ->
                {false, Data, Opts};
            true ->
                {true, Data, Opts}
            end
    end;

decode_nullflag(Data, _N, Opts) ->
    {false, Data, Opts}.


tail_getxy(N, Opts, XY) when Opts#dopts.stack == [] ->
    [N | XY];

tail_getxy(N, Opts, XY) ->
    [{StackValue, _, _} | StackT] = Opts#dopts.stack,
    Opts1 = Opts#dopts{stack = StackT},
    tail_getxy(length(StackValue) + 1 ,Opts1, [N | XY]).

tail_replacexy([X|Y], Value, NewElement) when Y == [], is_tuple(Value) ->
    setelement(X, Value, NewElement);

tail_replacexy([X|Y], Value, NewElement) when Y == [], is_list(Value) ->
    setelement_l(X, Value, NewElement);


tail_replacexy([X|Y], Value, NewElement) ->
    [H|T] = Value,
    [NewElement|T].

setelement_l(1, [_|Rest], New) -> [New|Rest];
setelement_l(I, [E|Rest], New) -> [E|setelement_l(I-1, Rest, New)].

% we have to respect the frame limits and slice the packet if it exceeds the limit
framing(Bin, #eopts{framesize = FrameSize,
                      frame     = FrameNumber,
                      framelimit= FrameLimit,
                      pid       = PID,
                      userdata  = UserData} = Opts, Value, ValueLen)
        when is_pid(PID), FrameSize + ValueLen >= FrameLimit ->

    Space = FrameLimit - FrameSize,
    <<ValueHead:Space/binary-unit:8, ValueTail/binary>> = Value,
    Frame = <<Bin/binary, ValueHead/binary>>,
    % send it to the PID
    erlang:send(
        PID,
        {frame, FrameNumber, FrameSize + Space, Frame, UserData}
        ),
    % process tail to the new frame
    framing(<<>>, Opts#eopts{framesize   = 0,
                             frame = FrameNumber + 1 },
            ValueTail, ValueLen - Space);

framing(Bin, Opts, Value, ValueLen) ->
    { <<Bin/binary, Value/binary>>, Opts#eopts{framesize = Opts#eopts.framesize + ValueLen} }.

% encoding NULL flag routines
encode_nullflag({[Flag | FlagTail], N}) when N rem 8 == 0 ->
    % every 8 bytes should have NULL-flag byte
    {<<Flag:8/big-integer>>, 1, {FlagTail,  N + 1} };

encode_nullflag(?LLSN_NULL) ->
    {<<>>, 0, ?LLSN_NULL};

encode_nullflag({FlagList, N}) ->
    {<<>>, 0, {FlagList, N + 1}}.


encode_nullflag_create(ValueList) ->
    {ReversedNullFlags, _} = lists:foldl(
        fun(Element, Acc) ->
                case Acc of
                    {[ByteFlag|FlagsList], N} ->
                        Pos = N rem 8,
                        if Element == null -> BOR = 1 bsl (7 - Pos);
                            true -> BOR = 0 end,

                        if Pos > 0 ->  {[ByteFlag bor BOR |FlagsList], N + 1};
                            true ->  {[0 bor BOR |[ByteFlag |FlagsList]], N + 1} end;
                    _ ->
                        % first item
                        if Element == null -> {[1 bsl 7], 1};
                            true -> {[0], 1} end
                end
        end,
        [], ValueList ),
    {lists:reverse(ReversedNullFlags),0}.