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
-mod_title("Zotonic Module Manager").
-mod_description("Web interface for managing Zotonic modules").
-mod_prio(500).
-mod_depends([mod_admin_modules]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").
-include_lib("include/mod_zmm.hrl").

-export([observe_admin_menu/3]).
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
            Module1 = z_utils:os_escape(Module),
            erlang:spawn(?MODULE, exec_zmm, [install, Module1, Context]),
	    {ok, Module}
     end.
	    	
%% @doc Execute external shell command.
%% Command is run in a separate process/space.
-spec exec_zmm(Action, Args, Context) -> Result when
    Action :: update | install,
    Args :: [string()],
    Context :: #context{},
    Result :: ok | {error, timeout}.
exec_zmm(install, Args, Context) when is_list(Args) ->
    cmd(["install", Args], Context);
exec_zmm(update, Args, Context) ->
    cmd(["update", Args], Context).

%% @doc Run a shell command
-spec cmd(Args, _Context) -> Result when
      Args :: [string()],
      Result :: ok | {error, timeout}.
cmd(Args, _Context) ->
    Args1 = ["modules"] ++ Args,
    Port = erlang:open_port({spawn_executable, ?ZOTONIC_EXE},
                            [stderr_to_stdout, exit_status, {args, Args1}]),
    try loop(Port,[], 3000) of
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
        {error, _NotFound} ->
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
    spawn(?MODULE, exec_zmm, [update, Arg, Context]),
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
		    mod_zmm:deactivate(Module, Context);
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
