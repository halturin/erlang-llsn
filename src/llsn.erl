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
%% Full license: https://github.com/allyst/go-llsn/blob/master/LICENSE
%%
%% copyright (C) 2014 Allyst Inc. http://allyst.com
%% author Taras Halturin <halturin@allyst.com>

-module(llsn).

-include("llsn.hrl").

-export([encode/2, encode/3, encode/4, encode/5]).

-export([encode_NUMBER/1]).

%% =============================================================================
%% Encoding
%% =============================================================================
% with no framing and default threshold

encode(Packet, Struct) when is_tuple(Packet) and is_tuple(Struct) ->
    Options = #options{
        threshold = ?LLSN_DEFAULT_THRESHOLD,
        framesize = ?LLSN_DEFAULT_FRAME_SIZE,
        frame = 1,
        binsize = 0,
        tail = [],
        stack = [],
        tt = typestree(new)
    },
    encode_ext(Packet, Struct, Options).

% with framing and default threshold
encode(Packet, Struct, PID) when is_pid(PID)
        and is_tuple(Packet) and is_tuple(Struct) ->
    Options = #options{
        threshold = ?LLSN_DEFAULT_THRESHOLD,
        framesize = ?LLSN_DEFAULT_FRAME_SIZE,
        frame = 1,
        binsize = 0,
        tail = [],
        stack = [],
        pid = PID,
        tt = typestree(new)
    },
    encode_ext(Packet, Struct, Options);


% no framing and custom threshold
encode(Packet, Struct, Threshold) when is_integer(Threshold)
        and is_tuple(Packet) and is_tuple(Struct) ->
    Options = #options{
        threshold = Threshold,
        framesize = ?LLSN_DEFAULT_FRAME_SIZE,
        frame = 1,
        binsize = 0,
        tail = [],
        stack = [],
        tt = typestree(new)
    },
    encode_ext(Packet, Struct, Options).

encode(Packet, Struct, PID, UserData) when is_pid(PID)
        and is_tuple(Packet) and is_tuple(Struct)
        and is_list(UserData) ->
    Options = #options{
        threshold = ?LLSN_DEFAULT_THRESHOLD,
        framesize = ?LLSN_DEFAULT_FRAME_SIZE,
        frame = 1,
        binsize = 0,
        tail = [],
        stack = [],
        pid = PID,
        userdata = UserData,
        tt = typestree(new)
    },
    encode_ext(Packet, Struct, Options);

% framing and custom framelimit
encode(Packet, Struct, PID, FrameLimit) when is_pid(PID)
        and is_integer(FrameLimit)
        and is_tuple(Packet) and is_tuple(Struct) ->
    Options = #options{
        threshold = ?LLSN_DEFAULT_THRESHOLD,
        framesize = FrameLimit,
        frame = 1,
        binsize = 0,
        tail = [],
        stack = [],
        pid = PID,
        tt = typestree(new)
    },
    encode_ext(Packet, Struct, Options).

% framing and custom framelimit
encode(Packet, Struct, PID, FrameLimit, UserData) when is_pid(PID)
        and is_integer(FrameLimit) and is_tuple(Packet) 
        and is_tuple(Struct) and is_list(UserData)->
    Options = #options{
        threshold = ?LLSN_DEFAULT_THRESHOLD,
        framesize = FrameLimit,
        frame = 1,
        binsize = 0,
        tail = [],
        stack = [],
        pid = PID,
        userdata = UserData,
        tt = typestree(new)
    },
    encode_ext(Packet, Struct, Options).


encode_ext(Packet, Struct, #options{threshold = Threshold} = Options) ->

    P   = tuple_to_list(Packet),
    Len = length(P),
    {LenBin, LenBinLen} = encode_UNUMBER(Len),
    Bin = <<Threshold:16/big-integer, LenBin/binary>>,

    Opts = #options{
            framesize    = 2 + LenBinLen, % first 2 bytes for threshold + N bytes for the number of elements
            struct       = Struct
            },
    

    encode_struct(P, tuple_to_list(Struct), Bin, Opts).

framing(Bin, #options{framesize    = FrameSize,
                frame  = FrameNumber,
                pid          = PID,
                userdata = UserData} = Opts, Value, ValueLen)
        when is_pid(PID), FrameSize + ValueLen >= ?LLSN_DEFAULT_FRAME_SIZE  ->
    Space = ?LLSN_DEFAULT_FRAME_SIZE - FrameSize,
    <<ValueHead:Space/binary-unit:8, ValueTail/binary>> = Value,
    Frame = <<Bin/binary, ValueHead/binary>>,
    % send it to the PID
    erlang:send(
        PID,
        {frame, FrameNumber, FrameSize + Space, Frame, UserData}
        ),
    % process tail to the new frame
    framing(<<>>, Opts#options{framesize   = 0,
                             frame = FrameNumber + 1 },
            ValueTail, ValueLen - Space);
framing(Bin, Opts, Value, ValueLen) ->
    { <<Bin/binary, Value/binary>>, Opts#options{framesize = Opts#options.framesize + ValueLen} }.




encode_struct(Value, Struct, Bin, Options) ->
    ok.








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


typestree(new) ->
    #typestree{ 
        type     = ?LLSN_NULL,
        length   = ?LLSN_NULL,

        parent   = ?LLSN_NULL,
        child    = ?LLSN_NULL,
        prev     = ?LLSN_NULL,
        next     = ?LLSN_NULL }.

typestree(append, Parent, Type) ->
    ok;

typestree(addchild, Parent, Type) ->
    ok.

typestree(type, Type) ->
    ok.