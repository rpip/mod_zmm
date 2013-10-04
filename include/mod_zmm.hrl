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


%% file for caching json data from ZMR 
-define(ZMM_CACHE, filename:join([z_utils:lib_dir(priv), "modules", ".zmm"])).

%% Path to the Zotonic executable
-define(ZOTONIC_EXE,filename:join([os:getenv("ZOTONIC"),'bin', 'zotonic'])).

%% Zotonic Modules Index API
-define(zmm_api,"http://modules.zotonic.com/api/zmr/repositories").

%% module record
-record(z_module, {title, repository, scm, is_active, is_installed}).


