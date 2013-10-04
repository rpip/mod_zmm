%% @author Mawuli Adzaku <mawuli@mawuli.me>
%% @copyright 2013 Mawuli Adzaku
%% @doc Admin interface for the Zotonic module manager, and interacting with the modules repository.

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

-module(controller_admin_zmm).
-author("Mawuli Adzaku <mawuli@mawuli.me>").

-export([
    is_authorized/2,
    event/2,
    html/1
]).

-include_lib("controller_html_helper.hrl").
-include_lib("../include/mod_zmm.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_zmm, ReqData, Context).


html(Context) ->
    Vars = [
        {modules,  m_zmm:list(Context)}
    ],
    Html = z_template:render("modules_index.tpl", Vars, Context),
    z_context:output(Html, Context).


%% @doc Postback handler for 'install-module' event
event(#postback_notify{message="install-module"}, Context) ->
    Module = z_context:get_q("module", Context),
    case mod_zmm:install(Module, Context) of
	{error, {already_exists, Module}} ->
  	    z_render:growl_error(?__(Module ++ " is already installed.", Context), Context);
        {ok, Module} ->
	    z_render:growl("Installing " ++ Module, Context);
	_Error ->
	   z_render:growl_error(?__("Failed to install" ++ Module, Context), Context)   
    end;

%% @doc Refresh the cache: Download the latest modules from ZMR
event(#postback_notify{message="refresh"}, Context) ->
    mod_zmm:refresh_cache(),
    z_render:growl("Module list updated", Context).

