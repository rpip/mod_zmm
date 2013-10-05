%% @author Mawuli Adzaku <mawuli@mawuli.me>
%% @copyright 2013 Mawuli Adzaku
%% Date: 2013-10-04
%% @doc Activate/dactivate a module. Modified version of the same action
%% found in mod_admin_modules.

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

-module(action_zmm_module_toggle).
-author("Mawuli Adzaku <mawuli@mawuli.me>").

%% zotonic header file
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).


render_action(TriggerId, TargetId, Args, Context) ->
    Module = proplists:get_value(module, Args),
    StatusId = proplists:get_value(status_id, Args),
    Postback = {module_toggle, Module, StatusId},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc Toggle the module status from/to activate/deactivate
%% @spec event(Event, Context1) -> Context2
event(#postback{message={module_toggle, Module, _StatusId}, trigger=TriggerId}, Context) ->
    Module1 = z_convert:to_atom(Module),
    case z_acl:is_allowed(use, mod_admin_modules, Context) of
        true ->
            Active = z_module_manager:active(Context),
            case lists:member(Module1, Active) of
                true ->
                    z_module_manager:deactivate(Module1, Context),
                    z_render:update(TriggerId, "Activate", Context);
                false ->
                    z_module_manager:activate(Module1, Context),
                    z_render:update(TriggerId, "Deactivate", Context)
            end;
        false ->
            z_render:growl_error("You are not allowed to activate or deactivate modules.", Context)
    end.
