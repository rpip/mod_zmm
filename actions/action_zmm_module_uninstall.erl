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

-module(action_zmm_module_uninstall).
-author("Mawuli Adzaku <mawuli@mawuli.me").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Module = proplists:get_value(module, Args),
    Postback = {module_uninstall, Module},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc 'module_uninstall' postback event handler
-spec event(Event, Context1) -> Context2 when
      Event :: #postback{},
      Context1 :: #context{},
      Context2 :: #context{}.
event(#postback{message={module_uninstall, Module}, trigger=_TriggerId}, Context) ->
    Module1 = atom_to_list(Module),
    z_render:growl("Uninstalling " ++ Module1, Context),
    case z_acl:is_allowed(use, mod_admin_modules, Context) of
	true ->
	    case mod_zmm:uninstall(Module1, Context) of
		{error, {not_found, Module1}} ->
		    z_render:growl_error(?__("ERROR: " ++ Module1 ++ " does not exist on this site", Context), Context);
		ok ->
		    z_render:growl(?__(Module1 ++ " uninstalled", Context), Context)
	    end;
      false ->
            z_render:growl_error("You are not allowed to uninstall modules.", Context)
    end.
