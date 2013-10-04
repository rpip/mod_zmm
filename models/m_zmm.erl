%% @author Mawuli Adzaku <mawuli@mawuli.me>
%% @copyright 2013 Mawuli Adzaku
%% Date: 23-09-2013
%% @doc Uses JSON data from ZMR as models.

%% Copyright 2013 Mawuli Adzaku
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(m_zmm).
-author("Mawuli Adzaku <mawuli@mawuli.me>").

-behaviour(gen_model).
%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    list/1
]).

-include_lib("zotonic.hrl").
-include("../include/mod_zmm.hrl").

%% return a property of the model
m_find_value(value, #m{}=M, Context) ->
    m_value(M, Context);

m_find_value(Key, #m{value=V} = _M, _Context) ->
    case lists:member(Key,record_info(fields, z_module)) of
        true ->  proplists:get_value(Key,V);
        false -> undefined
    end.     

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> []
m_to_list(#m{value=_V}, Context) ->
   list(Context).

%% @doc return a model
m_value(#m{value=V}, _Context) ->
    V.


%% @doc Fetch all modules from ZMR, and cache the return data
list(Context)->
    Structs = mochijson2:decode(mod_zmm:zmr_data()),
    Active = z_module_manager:active(Context),
    %AllModules = z_module_manager:all(Context),    
    AllModules = lists:map(fun({M,_Path}) ->
				   M end,
				   z_module_manager:scan(Context)),
    Status = z_module_manager:get_modules_status(Context),
    Status1 = lists:flatten(
                    [ 
                        [ {Module, atom_to_list(State)} || {Module, _, _Pid, _Date} <- Specs ] 
                        || {State, Specs} <- Status 
                    ]),
    Modules = [[{title, Title},
	       {repository, Repo},
	       {scm, SCM},
	       {is_active, lists:member(z_convert:to_atom(Title), Active)},
	       {is_installed, lists:member(z_convert:to_atom(Title), AllModules)},
               {status, proplists:get_value(z_convert:to_atom(Title), Status1)}]
	       || {struct,[{<<"title">>,Title}, {<<"repository">>,Repo}, {<<"scm">>,SCM}]} <- Structs],
    Modules.
