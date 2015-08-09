%% Erlang support for LLSN - Allyst's data interchange format.
%% LLSN specification http://allyst.org/opensource/llsn/
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% copyright (C) 2015 Allyst Inc. http://allyst.com
%% author Taras Halturin <halturin@allyst.com>


-define(LLSN_TYPE_UNDEFINED,        0).
-define(LLSN_TYPE_NUMBER,           1).
-define(LLSN_TYPE_FLOAT,            2).
-define(LLSN_TYPE_STRING,           3).
-define(LLSN_TYPE_BLOB,             4).
-define(LLSN_TYPE_FILE,             5).
-define(LLSN_TYPE_DATE,             6).
-define(LLSN_TYPE_BOOL,             7).
-define(LLSN_TYPE_STRUCT,           8).
-define(LLSN_TYPE_ARRAY,            9).


-define(LLSN_TYPE_ARRAYN,           10).
-define(LLSN_TYPE_POINTER,          11).
-define(LLSN_TYPE_UNUMBER,          12).

-define(LLSN_TYPE_UNDEFINED_NULL,   255).
-define(LLSN_TYPE_NUMBER_NULL,      254).
-define(LLSN_TYPE_FLOAT_NULL,       253).
-define(LLSN_TYPE_STRING_NULL,      252).
-define(LLSN_TYPE_BLOB_NULL,        251).
-define(LLSN_TYPE_FILE_NULL,        250).
-define(LLSN_TYPE_DATE_NULL,        249).
-define(LLSN_TYPE_BOOL_NULL,        248).
-define(LLSN_TYPE_STRUCT_NULL,      247).
-define(LLSN_TYPE_ARRAY_NULL,       246).
-define(LLSN_TYPE_ARRAYN_NULL,      245).
-define(LLSN_TYPE_POINTER_NULL,     244).
-define(LLSN_TYPE_UNUMBER_NULL,     243).
-define(LLSN_NULL_TYPES, 128).


-define(LLSN_NULL, null).
-define(LLSN_DEFAULT_THRESHOLD,     0). % value range: 0..4096
-define(LLSN_DEFAULT_FRAME_LIMIT,   49152). % 48K
-define(LLSN_TYPE_STRING_LIMIT,     10485760). % 1MB
-define(LLSN_TYPE_BLOB_LIMIT,       4294967296). % 4GB
-define(LLSN_DEFAULT_BUFFER_SIZE,   104857600). % 10MB. uses for file reading in encoding process

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
