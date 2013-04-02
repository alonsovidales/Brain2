%%
%% This module is used to centralize all the logs of the system
%%

-module(logging).

-author('alonso.vidales@tras2.es').

-export([
    init/2]).

%%
%% Adds a log line to the log files with the date, and Pid name in
%% case of be defined
%%
%% @param Log log() The pointer to the log files manager
%% @param Pid pid() The Pid of the process who created the log line
%% @param Type atom The type of the log as an atom, can be: fatal, error, debug, info
%% @param LogLine list The log line string
%%
addLogLine(NodeId, Log, Pid, Type, LogLine) ->
    {{Year, Month, Day}, {Hour, Minute, Seconds}} = calendar:local_time(),
    TimeAsStr = io_lib:format("~p/~p/~p ~p:~p:~p [~s] ", [Year, Month, Day, Hour, Minute, Seconds, NodeId]),

    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} ->
            Line = io_lib:format("~s ~s [modeule: ~s]: ~s", [TimeAsStr, Type, Name, LogLine]);
        _NoName ->
            Line = io_lib:format("~s ~s: ~s", [TimeAsStr, Type, LogLine])
    end,

    disk_log:blog(Log, Line),
    disk_log:sync(Log).

%%
%% This function starts a listener for new log messages in a infinite loop
%%
%% @param Config dict The configuration dictionary
%% @param Log log() The pointer to the log files manager
%%
startListenerLoop(Config, Log, NodeId) ->
    receive
        {add, Pid, Type, LogLine} ->
            Mode = dict:fetch("log_mode", Config),
            if
                ((Mode == "production") and ((Type == error) or (Type == fatal))) or
                (Mode == "debug") ->
                    addLogLine(NodeId, Log, Pid, Type, LogLine)
            end,

            startListenerLoop(Config, Log, NodeId)
    end.

%%
%% Launch the log system, this method is designed to be launched as a new process
%%
%% @param Config dict The configuration dictionary
%%
init(Config, NodeId) ->
    case disk_log:open([
        {name, "brainLog"},
        {file, dict:fetch("log_dir", Config) ++ dict:fetch("log_file_name", Config)},
        {type, wrap},
        {format, external},
        {size, {list_to_integer(dict:fetch("log_max_size", Config)), list_to_integer(dict:fetch("log_max_num_of_files", Config))}}
    ]) of
        {ok, Log} ->
            startListenerLoop(Config, Log, NodeId);
        Error ->
            io:format("Error trying to open the log file: ~p~n", [Error]),
            throw("Error on log files")
    end.
