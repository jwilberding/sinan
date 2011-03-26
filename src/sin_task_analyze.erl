%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%---------------------------------------------------------------------------
%%% @author Erlware, LLC
%%% @doc
%%%   Uses dialyzer to analyze the project sources and output messages.
%%% @end
%%% @copyright (C) 2007-2011, Erlware, LLC
%%% Created : 26 Mar 2011 by Erlware, LLC <core@erlware.org>
%%%---------------------------------------------------------------------------
-module(sin_task_analyze).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1]).

-define(TASK, analyze).
-define(DEPS, [build]).
-define(MINUTES_PER_APP, 10).
-define(WIP_TIME, 5000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @spec () -> ok
%% @end
%%--------------------------------------------------------------------
description() ->
    Desc = "Runs dialyzer on the project and outputs information "
        "to the user. This task may take a significant amount "
        "of time",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = false,
	  deps = ?DEPS,
	  example = "analyze",
	  short_desc = "Runs dialyzer on the project",
	  desc = Desc,
	  opts = []}.



%%--------------------------------------------------------------------
%% @doc
%%  dO the task defined in this module.
%% @spec do_task(BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    io:format("JW calling analyze~n"),
    analyze(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%   Run the analyze task.
%%
%% @spec analyze(BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
analyze(BuildRef) ->
    io:format("JW get builddir~n"),
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    io:format("JW get pltpath~n"),
    PltPath = filename:join([BuildDir, "info", "dialyzer_plt"]),
    case sin_config:get_value(BuildRef, "tasks.analyze.init", false) of
        true ->
	    io:format("JW generate local_plt~n"),
            generate_local_plt(BuildRef, PltPath);
        _ ->
	    io:format("JW run analyzer~n"),
            run_analyzer(BuildRef, BuildDir, PltPath)
    end.
    %%eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Analyze the current project repos and print error and warning
%%  messages.
%% @spec (BuildRef, BuildDir, PltPath) -> ok
%% @end
%%--------------------------------------------------------------------
run_analyzer(BuildRef, BuildDir, PltPath) ->
    io:format("JW app list~n"),
    AppList = sin_config:get_value(BuildRef, "project.apps"),
    io:format("JW repoapplist~n"),
    RepoAppList = sin_config:get_value(BuildRef, "project.repoapps"),
    io:format("JW check file exists~n"),
    case sin_utils:file_exists(PltPath) of
        false ->
	    io:format("JW false~n"),
            generate_local_plt(BuildRef, PltPath);
        true ->
	    io:format("JW true~n"),
            ok
    end,
    BuildPath = filename:join([BuildDir, "apps"]),
    io:format("JW buildpath: ~p~n", [BuildPath]),
    io:format("JW Repo~n"),
    %%Repo = sin_config:get_value(BuildRef, "project.repository"),
    %%BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    LibDir = code:lib_dir(),
    io:format("JW getting codepath~n"),
    Codepaths = get_code_paths(LibDir, RepoAppList, []),
    io:format("JW codepaths: ~p~n", [Codepaths]),
    Codepaths2 = get_code_paths(BuildPath, AppList, []) ++ Codepaths,
    io:format("JW getting codepath2s: ~p~n", [Codepaths2]),
    Opts = [{files_rec, Codepaths2}, {from, byte_code},
            {init_plt, PltPath}],
    %%eta_event:task_event(BuildRef, ?TASK, starting_analysis, "Starting analysis..."),
    io:format("JW starting analysis with Opts: ~p~n", [Opts]),
    %%output_info(BuildRef, dialyzer:run(Opts)),
    try Output = dialyzer:run(Opts)
	catch
	    {dialyzer_error, Error} -> io:format("Dialyzer Error: ~p~n", [Error]);
	    OtherError -> io:format("Other error: ~p~n", [OtherError])

    end,
    %%eta_event:task_event(BuildRef, ?TASK, analysis_complete, "analysis complete").
    io:format("analysis complete~n").

%%--------------------------------------------------------------------
%% @doc
%%  Generate a base plt to make use of.
%%
%% @spec (BuildRef, PltPath) -> ok
%% @end
%%--------------------------------------------------------------------
generate_local_plt(BuildRef, PltPath) ->
    RepoAppList = sin_config:get_value(BuildRef, "project.repoapps"),
    %%ProjectRepo = sin_config:get_value(BuildRef, "project.repository"),
    %%BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    LibDir = code:lib_dir(),
    io:format("JW getting code paths: ~p:~p~n", [LibDir, RepoAppList]),
    Codepaths = get_code_paths(LibDir, RepoAppList, []),
    io:format("JW genplt codepaths: ~p~n", [Codepaths]),
    io:format("JW getting Minutes~n"),
    Minutes = integer_to_list(length(Codepaths) * ?MINUTES_PER_APP),
    %%eta_event:task_event(BuildRef, ?TASK, generating_plt,
    %%                     "Generating base plt, this could take as "
    %%                     "long as " ++ Minutes ++ " minutes"),
    %{ok, Pid} = sin_wip:start_link(BuildRef, ?TASK, ".", 5000),
    %try gen_plt(BuildRef, PltPath, Codepaths) after
    %    sin_wip:quit(Pid)
    %end.
    %{ok, Pid} = sin_wip:start_link(BuildRef, ?TASK, ".", 5000),
    gen_plt(BuildRef, PltPath, Codepaths).

%%--------------------------------------------------------------------
%% @doc
%%  generate a plt file for use by the project. Plt should be
%% generated over all dependencies
%% @spec (BuildRef, PltPath, Codepaths) -> ok
%% @end
%%--------------------------------------------------------------------
gen_plt(BuildRef, PltPath, Codepaths) ->
    Opts = [{files_rec, Codepaths},
            {from, byte_code},
            {analysis_type, plt_build},
            {output_plt, PltPath}],
    io:format("Opts: ~p~n", [Opts]),
    io:format("~nJW going to run dialyzer~n"),
    try Output = dialyzer:run(Opts)
	catch
	    {dialyzer_error, Error} -> io:format("Dialyzer Error: ~p~n", [Error]);
	    OtherError -> io:format("Other error: ~p~n", [OtherError])

    end.
    %%io:format("JW Output: ~p~n", [Output]),
    %%output_info(BuildRef, Output).

    %%eta_event:task_event(BuildRef, ?TASK, plt_generation_complete,
    %%                     "Done generating plt").



%%--------------------------------------------------------------------
%% @doc
%%  Print out the informational output from dialyzer.
%% @spec output_info(BuildRef, Result) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
output_info(BuildRef, {ok, Warnings}) ->
    output_warnings(BuildRef, 1, Warnings);
output_info(BuildRef, {error, Warnings, Errors}) ->
    output_warnings(BuildRef, 1, Warnings),
    output_errors(BuildRef, 1, Errors).

%%--------------------------------------------------------------------
%% @doc
%%  Print out dialyzer warnings.
%% @spec output_warnings(BuildRef, Count, WarningList) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
output_warnings(BuildRef, N, [All | T]) ->
    %%eta_event:task_event(BuildRef, ?TASK, analyze_warning,
    %%                     dialyzer:format_warning(All)),
    output_warnings(BuildRef, N + 1, T);
output_warnings(_, _, []) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Print out errors from dialyzer.
%% @spec output_errors(BuildRef, Count, ErrorList) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
output_errors(BuildRef, N, [Error | T]) ->
    %%eta_event:task_event(BuildRef, ?TASK, analyze_error, {"Error ~w: ~s", [N, Error]}),
    output_errors(BuildRef, N + 1, T);
output_errors(_, _, []) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%%  Generate the code paths for the apps we will check with
%%  dialyzer.
%% @spec get_code_paths(BuildDir, Apps, Acc) -> EbinPaths
%% @end
%% @private
%%--------------------------------------------------------------------
get_code_paths(BuildDir, [{AppName, Vsn, _, _} | T], Acc) ->
    io:format("JW codepaths1~n"),
    get_code_paths(BuildDir, [{AppName, Vsn} | T], Acc);
get_code_paths(BuildDir, [{AppName, Vsn} | T], Acc) ->
    io:format("JW codepaths2~n"),
    Dir = filename:join([BuildDir,
                         lists:flatten([atom_to_list(AppName), "-", Vsn]),
                         "ebin"]),
    get_code_paths(BuildDir, T, [Dir | Acc]);
get_code_paths(_BuildDir, [], Acc) ->
    io:format("JW codepaths3~n"),
    Acc.
