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

%% this module for development purposes only

-module(llsn_helper).

-export([reload/0]).

-export([start/0, stop/0]).

%% includes
-include_lib("include/llsn.hrl").

-define(APPS, [ syntax_tools, compiler, goldrush, lager ]).


reload_m(M) ->
    code:purge(M),
    code:soft_purge(M),
    {module, M} = code:load_file(M),
    {ok, M}.


reload() ->
    Modules = [M || {M, P} <- code:all_loaded(), is_list(P) andalso string:str(P, "llsn") > 0],
    [reload_m(M) || M <- Modules].


start() ->
    ?LOG("Start apps: ~p", [?APPS]),
    application:load(lager),
    application:set_env(lager, handlers,
        [
            {lager_console_backend, info},
            {lager_file_backend, [{file, "./log/run.log"}, {level, debug}]}
        ]
    ),
    ok = ensure_started(?APPS),

    ok = sync:go(),
    sync:growl(none),
    ok.

stop() ->
    sync:stop(),
    ok = stop_apps(lists:reverse(?APPS)),
    ok.


ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
    case application:start(App) of
        ok -> ensure_started(Apps);
        {error, {already_started, App}} -> ensure_started(Apps)
    end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
    application:stop(App),
    stop_apps(Apps).