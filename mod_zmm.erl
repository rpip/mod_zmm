%% @author Mawuli Adzaku <mawuli@mawuli.me>
%% @copyright 2013 Mawuli Adzaku
%% Date: 21 Aug 2013
%% @doc Web admin interface for the Zotonic Module Manager

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

-module(mod_zmm).
-author('Mawuli Adzaku <mawuli@mawuli.me>').

%& Module metadata
-mod_title("Zotonic Module Manager").
-mod_description("Web interface for managing Zotonic modules").
%-mod_depends([]).
% zotonic/priv/modules path for the modules installed from there.
-mod_prio(500).
-export([observe_admin_menu/3]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").
-include_lib("include/mod_zmm.hrl").
-export([
    install/2,	
    exec_zmm/3,
    update/2,
    uninstall/2
]).

-export([cache_exists/0,
	zmr_data/0,
	refresh_cache/0,
	refresh_cache/1
	]).


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_zmm,
                parent=admin_modules,
                label=?__("Module manager", Context),
                url={admin_zmm},
                visiblecheck={acl, use, ?MODULE}}
     |Acc].


%% @doc Return path to the Zotonic module
-spec build_module_path(Module, Context) -> ModulePath when
      Module :: string(),
       Context :: #context{},      
      ModulePath :: string().						   
build_module_path(Module, _Context)->
    PrivDir = z_utils:lib_dir(priv),
    ModulePath = filename:join([PrivDir, "modules", Module]),
    ModulePath.


%% @doc Install a Zotonic module in the priv/modules directory
-spec install(Module, Context) -> Result when
       Module :: string(),
       Context :: #context{},      
       Result :: {error, {already_ecists, list()}}
	         | {ok, list()} | {error, list()}.
install(Module, Context) ->
    ModulePath = build_module_path(Module, Context),
    case filelib:is_file(ModulePath) of
        true ->
            {error, {already_exists, Module}};
        false ->
            Arg = z_utils:os_escape(Module),
            erlang:spawn(z_module_manager, exec_zmm, [install, Arg, Context]),
	    {ok, Module}
     end.
	    	
%% @doc Return path to the module manager script
-spec get_path_to_zmm() -> Path when
      Path :: string().
get_path_to_zmm() ->
    filename:join([os:getenv("ZOTONIC"),'bin', 'zmm']).

%% @doc Execute external shell command.
%% Command is run in a separate process/space.
-spec exec_zmm(Action, Arg, Context) -> Result when
    Action :: update | install,
    Arg :: string(),
    Context :: #context{},
    Result :: ok | {error, timeout}.
exec_zmm(install, Arg, Context) when is_list(Arg) ->
    Cmd = get_path_to_zmm() ++ " install " ++ Arg,
    cmd(Cmd, 30000, Context);
exec_zmm(update, Arg, Context) ->
    Cmd = get_path_to_zmm() ++ " update " ++ Arg,
    cmd(Cmd, 30000, Context).

%% @doc Run a shell command
-spec cmd(Cmd, Timeout, _Context) -> Result when
      Cmd :: string(),
      Timeout :: integer(),
      Result :: ok | {error, timeout}.
cmd(Cmd, Timeout, _Context) ->
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    try loop(Port,[], Timeout) of
       {done, _Data} ->	    
           ok
    catch
       throw:timeout ->
	    {error, timeout}
    end.
    
%% @doc Interacts with the Erlang opened for calling the ZMM CLI 	
-spec loop(Port, Data, Timeout) -> Result when
      Port :: port(),
      Data :: list(),
      Timeout :: integer(),
      Result :: {done, Data} | throw.
loop(Port, Data, Timeout) ->
    receive
	{Port, {data, NewData}} ->
	    loop(Port, Data++NewData, Timeout);
	{Port, {exit_status, 0}} ->
	    {done, Data};
	{Port, {exit_status, S}} ->
	    throw({cmd_failed, S})
	after Timeout ->
	    throw(timeout)
    end.

%% @doc Recursively delete directory
-spec del_dir(Path) -> ok when
      Path :: string().
del_dir(Path) ->
    case file:del_dir(Path) of
        {error, eexist} ->
            ok = del_files(Path),
            ok = file:del_dir(Path);
        ok ->
            ok
    end.

del_files(Path) ->
    {ok, Files} = file:list_dir(Path),
    ok = del_files(Path, Files).

del_files(_EmptyDir, []) ->
    ok;
del_files(Dir, [Filename | Rest]) ->
    Path = filename:join([Dir, Filename]),
    case filelib:is_dir(Path) of
        true ->
            ok = del_dir(Path);
        false ->
            ok = file:delete(Path)
    end,
    del_files(Dir, Rest).


%% @doc Update a Zotonic module by pulling the latest changes from the
%%    module's remote repostory
-spec update(Module, Context) -> Result when
      Module :: string(),
      Context :: #context{},
      Result :: {ok, list()}.
update(Module, Context) ->
    Arg = z_utils:os_escape(Module),
    spawn(z_module_manager, exec_zmm, [update, Arg, Context]),
    {ok, Module}.


%% @doc Deactivate and delete module from the priv/modules directory
-spec uninstall(Module, Context) -> Result when
      Module :: string(),
      Context :: #context{},
      Result :: {ok, list()} | {error, list()}.
uninstall(Module, Context) ->
    ModulePath = build_module_path(Module, Context),
    case filelib:is_dir(ModulePath) of
        true ->
	    case z_module_manager:whereis(Module, Context) of
		{ok, _Pid} ->
		    z_module_manager:deactivate(Module, Context);
		{error, not_running} ->
		    not_running
	    end,
	%% delete the module's directory
	del_dir(ModulePath),
	%% update Zotonic
	zotonic:update(),
	ok;
        false ->
	    {error, {not_found, Module}}
     end.

%% @doc Return cached file containing the downloaded json data of modules
%%      available on the Zotonic Module Repository.
-spec cache_exists() -> {true, Data :: string()} | {false, empty}.
cache_exists() ->
    case filelib:file_size(?ZMM_CACHE) of
	N when N > 0 ->
	    {ok, Data} = file:read_file(?ZMM_CACHE),
	    {true, Data};
	_Empty ->
	    {false, empty}
    end.

%% @doc Fetch modules from ZMR or the cache, if it exists.
-spec zmr_data() -> Data when
      Data :: string() | binary().
zmr_data() ->
    case mod_zmm:cache_exists() of
	{false, _} -> 
	    Body = refresh_cache(true),
	    Body;
	{true, Data} ->
	    Data
      end.

%% @doc Refresh the local ZMR moudle list cache
-spec refresh_cache() -> binary().
refresh_cache()->
    {ok, {{_HttpVer, 200, _State}, _Head, Body}} = httpc:request(?zmm_api),
    ok = file:write_file(?ZMM_CACHE, Body).
    
%% @doc Refresh the local zmm module list cache and return the `new` data
-spec refresh_cache(true) -> binary().
refresh_cache(true)->
    {ok, {{_HttpVer, 200, _State}, _Head, Body}} = httpc:request(?zmm_api),
    ok = file:write_file(?ZMM_CACHE, Body),
    Body.
