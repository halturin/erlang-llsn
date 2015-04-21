%%
%% Allyst ™  - Universal Integration Platform, http://allyst.com
%%
%% CONFIDENTIAL NOTICE:
%%
%% All information contained herein is, and remains the property
%% of Allyst Inc. and its suppliers, if any. The intellectual
%% and technical concepts contained herein are proprietary to
%% Allyst Inc. and its suppliers and may be covered by U.S.
%% and Foreign Patents, patents in process, and are protected by trade
%% secret or copyright law. Dissemination of this information
%% or reproduction of this material is strictly forbidden unless prior
%% written permission is obtained from Allyst Inc.
%%
%% @copyright 2015 Allyst Inc. http://allyst.com
%%
%% @author Taras Halturin <halturin@allyst.com>
%% @doc
%%      FIXME
%% @end
-module(llsn_helper).

-export([reload/0]).


%% includes
-include_lib("include/llsn.hrl").


reload_m(M) ->
    code:purge(M),
    code:soft_purge(M),
    {module, M} = code:load_file(M),
    {ok, M}.


reload() ->
    Modules = [M || {M, P} <- code:all_loaded(), is_list(P) andalso string:str(P, "proto") > 0],
    [reload_m(M) || M <- Modules].

