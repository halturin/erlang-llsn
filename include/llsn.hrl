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


-define(LLSN_TYPE_UNDEFINED,    0).
-define(LLSN_TYPE_NUMBER,       1).
-define(LLSN_TYPE_FLOAT,        2).
-define(LLSN_TYPE_STRING,       3).
-define(LLSN_TYPE_BLOB,         4).
-define(LLSN_TYPE_FILE,         5).
-define(LLSN_TYPE_DATE,         6).
-define(LLSN_TYPE_BOOL,         7).
-define(LLSN_TYPE_STRUCT,       8).
-define(LLSN_TYPE_ARRAY,        9).


-define(LLSN_TYPE_ARRAYN,      10).
-define(LLSN_TYPE_POINTER,     11).
-define(LLSN_TYPE_UNUMBER,     12).

-define(LLSN_TYPE_UNDEFINED_NULL,    255).
-define(LLSN_TYPE_NUMBER_NULL,       254).
-define(LLSN_TYPE_FLOAT_NULL,        253).
-define(LLSN_TYPE_STRING_NULL,       252).
-define(LLSN_TYPE_BLOB_NULL,         251).
-define(LLSN_TYPE_FILE_NULL,         250).
-define(LLSN_TYPE_DATE_NULL,         249).
-define(LLSN_TYPE_BOOL_NULL,         248).
-define(LLSN_TYPE_STRUCT_NULL,       247).
-define(LLSN_TYPE_ARRAY_NULL,        246).
-define(LLSN_TYPE_ARRAYN_NULL,       245).
-define(LLSN_TYPE_POINTER_NULL,      244).
-define(LLSN_TYPE_UNUMBER_NULL,      243).

-define(LLSN_NULL_TYPES, 128).


-define(LLSN_NULL, null).
-define(LLSN_DEFAULT_THRESHOLD,        0).
-define(LLSN_DEFAULT_FRAME_SIZE,           49152). % 48K
-define(LLSN_TYPE_STRING_MAXBYTES,      10485760).
-define(LLSN_TYPE_BLOB_MAXBYTES,      4294967296).

% file decoding temporary directory
-define(LLSN_DEFAULT_DIR,           "/tmp/").
-define(LLSN_DEFAULT_FILEPREFIX,    "llsn").


-record(llsn_date_d, {
    year    :: integer(),
    month   :: non_neg_integer(),
    day     :: non_neg_integer()
    }).

-record(llsn_date_t, {
    hour    :: non_neg_integer(),
    min     :: non_neg_integer(),
    sec     :: non_neg_integer(),
    msec    :: non_neg_integer() %% up to 999
    }).

-record(llsn_date_tz, {
    hour    :: integer(),
    min     :: non_neg_integer()
    }).

-record (llsn_date, {
    date,
    time,
    zone
    }).

-record(llsn_file, {
    name    :: string(),   %% file name
    origin  :: string()    %% original file name with location
    }).

-define (LLSN_FILE(File),          #llsn_file{name = filename:basename(File), origin = File}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% for development purposes only
%%
-define(DBG(Format),                lager:log(debug, self(), "~p:~p: " ++ Format, [?MODULE, ?LINE])).
-define(DBG(Format, Data),          lager:log(debug, self(), "~p:~p: " ++ Format, [?MODULE, ?LINE | Data])).

-define(LOG(Format),                lager:log(info, self(), "~p:~p: " ++ Format, [?MODULE, ?LINE])).
-define(LOG(Format, Data),          lager:log(info, self(), "~p:~p: " ++ Format, [?MODULE, ?LINE | Data])).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
