%% @author Mawuli Adzaku <mawuli@mawuli.me>
%% @copyright 2013 Mawuli Adzaku
%% Date: 2013-08-30
%% @doc Reinstall a module

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

-module(action_zmm_module_reinstall).
-author("Mawuli Adzaku <mawuli@mawuli.me").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Module = proplists:get_value(module, Args),
    Postback = {module_reinstall, Module},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc 'module_reinstall' postback event handler
%% @spec event(Event, Context1) -> Context2
event(#postback{message={module_reinstall, Module}, trigger=_TriggerId}, Context) ->
    Module1 = atom_to_list(Module),
    case z_acl:is_allowed(use, mod_admin_modules, Context) of
	true ->
	        case mod_zmm:reinstall(Module, Context) of
		    nop ->
		        z_render:growl_error(?__("ERROR: manage schema not defined for " ++ Module1 , Context), Context);
		    _HasSchema ->
    	    	    	z_render:growl("Reinstalling " ++ Module1, Context)
	        end;
      false ->
            z_render:growl_error("You are not allowed to reinstall modules.", Context)
    end.
