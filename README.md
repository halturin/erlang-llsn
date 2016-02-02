erlang-LLSN
======

**__draft__**


This is Erlang implementation of LLSN binary format.
Here is specifiaction: http://allyst.org/opensource/llsn/

Basics
------
...

```Erlang
   Struct = {?LLSN_TYPE_NUMBER, ?LLSN_TYPE_STRING},
   Packet = {12345, "Hello World!"},
   Bin = llsn:encode(Packet, Struct).
```

...

```Erlang
   Struct = {
             ?LLSN_TYPE_NUMBER, ?LLSN_TYPE_STRING, 
             {?LLSN_TYPE_ARRAY, ?LLSN_TYPE_NUMBER}
            },
   Packet = {12345, "Hello World!", [1,2,3,4,5]},
   Bin = llsn:encode(Packet, Struct).
```

...

```Erlang
   Struct = {
             ?LLSN_TYPE_NUMBER, ?LLSN_TYPE_STRING,
             {?LLSN_TYPE_ARRAYN, ?LLSN_TYPE_NUMBER}
            },
   Packet = {12345, "Hello World!", [1,2,null,null,5]},
   Bin = llsn:encode(Packet, Struct).
```

...

```Erlang
   Struct = {
             ?LLSN_TYPE_NUMBER, ?LLSN_TYPE_STRING,
             {?LLSN_TYPE_ARRAYN, ?LLSN_TYPE_NUMBER},
             {?LLSN_TYPE_STRUCT,[?LLSN_TYPE_BOOL, ?LLSN_TYPE_DATE]}
            },
   Packet = {
             12345, "Hello World!",
             [1,2,null,null,5],
             {false, {{2015, 4, 15},{16, 56, 39, 678},{0,0}}}
            },
   Bin = llsn:encode(Packet, Struct).
```

Extended features of data encoding
-----------------------
###Tree-like data
...

C-example of tree like data structure

```C
typedef TreeStruct struct {
    int value,
    TreeStruct *child
}
```

In LLSN you have to use  LLSN_TYPE_POINTER. 

```Erlang
   Struct = {
             ?LLSN_TYPE_NUMBER, ?LLSN_TYPE_STRING,
             {?LLSN_TYPE_ARRAYN, ?LLSN_TYPE_NUMBER},
             {?LLSN_TYPE_STRUCT,[?LLSN_TYPE_BOOL, ?LLSN_TYPE_DATE]},
             {?LLSN_TYPE_STRUCT,[?LLSN_TYPE_NUMBER, {?LLSN_TYPE_POINTER,[3]}]}
            },
   Packet = {
             12345, "Hello World!",
             [1,2,null,null,5],
             {false, {{2015, 4, 15},{16, 56, 39, 678},{0,0}},
             {1,{2,{3,null}}}}
            },
   Bin = llsn:encode(Packet, Struct).
```


###Data framing

...

```Erlang
   Bin = llsn:encode(Packet, Struct, self()).
```

Default frame size has value 49152 bytes (as LLSN_DEFAULT_FRAME_LIMIT definition in .erl file). You
can specify custom value

```Erlang
   Bin = llsn:encode(Packet, Struct, self(), 128000).
```

... 3th argument is Pid of receiver process.

```Erlang
receive_frame(Bin) ->
    receive
        {frame, _N, _Size, Frame, UserData } ->
            ?assert(UserData =:= [userdata]),
            receive_frame(<<Bin/binary,Frame/binary>>);

        {done, _N, _Size, Frame, UserData} ->
            ?assert(UserData =:= [userdata]),
            <<Bin/binary,Frame/binary>>

    after 1000 -> ?assert("timeout")
    end.

...

demoframe() ->
...
    ok  = llsn:encode(Packet, Struct, self(), 50, [userdata]),
    Bin  = receive_frame(<<>>),
```

###Decode partitioned data

It's able to decode part of encoded data starting from the first frame.

```Erlang
    case decode(Bin) of
        {parted, State1} ->
            % itsn't entired packet
            ...
        Packet ->
            % complete packet
            ...
    end
    ...
```

To continue decoding you have to use State1 and the next part of binary data

```Erlang
    Value = decode(continue, State1, NextFrame)
```

It's useful in case of transfer big files. For example: array of Images.

License
--------

The MIT License (MIT)