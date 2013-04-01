%%
%% This module if used to handle all the external requests who cames
%% from the client as messages
%%

-module(data_controller).

-author('alonso.vidales@tras2.es').

-export([
    init/2,
    action_executor/8]).

-import(data_hash).
-import(data_volatile).
-import(data_string).

action_executor(Action, Handler, Key, Params, Redundance, NodeId, PidToConfirm, RingComplete) ->
    % This is the last iteration trying to load the key from memory on one of the nodes of the ring,
    % and this is the node where the query was done. Load the data from the data warehouse, and try
    % to do the action against the loaded data
    if RingComplete == true ->
        Handler ! {self(), load, Key},

        receive
            ko ->
                throw(io_lib:format("Problem trying to load key ~s from Warehouse~n", [Key]));
            ok ->
                logging ! {add, self(), info, io_lib:format("Loaded key ~s from Warehouse~n", [Key])}
        end
    end,

    % Try to execute the action on memory only
    Handler ! {self(), Action, Params},

    receive
        % The data is not in memory on the local node, try to execute the action on the node at the right
        ko ->
            logging ! {add, self(), info, io_lib:format("No data found on node~s~n", [NodeId])},
            % Search on the rest of nodes of the Ring
            ringManager ! {get, right, self()},
            receive
                {ok, NodePid} ->
                    % Execute the action on the right node
                    NodePid ! {ie, NodeId, Action, Handler, Key, Params, PidToConfirm, Redundance};
                _NoNode ->
                    logging ! {add, self(), info, io_lib:format("No node at the right~n")}
                after 100 ->
                    logging ! {add, self(), error, io_lib:format("Ring manager doesn't respond~n")}
            end;

        % The data was found, return execute the action, and in case of a modification on the data,
        % redundance it on the Redundance nodes at the right
        {ok, Result} ->
            PidToConfirm ! {ok, Result}

            %if (Redundance > 0) and (Action /= get) ->
            %    ringManager ! {get, right, self()},
            %    receive
            %        {ok, NodePid} ->
            %            % Execute the action on the right node
            %            NodePid ! {ie, NodeId, redundance, Handler, Key, Params, false, Redundance - 1};
            %        _NoNode ->
            %            logging ! {add, self(), info, io_lib:format("No node at the right~n")};
            %        after 100 ->
            %            logging ! {add, self(), error, io_lib:format("Ring manager doesn't respond~n")}
            %    end;
    end.

%%
%% This method will listen for incomming messages from the client, and on a new process will attend them
%% Acts as an adapter of the different data type managers.
%%
listener_loop(Config, NodeId, OpsSec, Timestamp) ->
    % Used to get stats of ops / sec, etc
    {_Mega, TimestampSec, _Micro} = now(),
    if
        Timestamp == TimestampSec ->
            NowOpsSec = OpsSec + 1;
        true ->
            NowOpsSec = 1
    end,

    receive
        % Internal call to try to execute an action on the memory of one of the nodes
        {ie, InNodeId, InAction, InHandler, InKey, InParams, InPidToConfirm, InRedundance} ->
            spawn(data_controller, action_executor, [
                InAction,
                InHandler,
                InKey,
                InParams,
                InRedundance,
                InNodeId,
                InPidToConfirm,
                InNodeId == NodeId]);

        % action_executor(Action, Handler, Key, Params, Redundance, NodeId, PidToConfirm, RingComplete) ->
        {Pid, "get", Key, _Args} ->
            Pid ! {ok, io_lib:format("This is a get!!!: ~s~n", [Key])};

        {checkAlive, Pid} ->
            Pid ! ok;

        {persistAll} ->
            hash_manager ! {persistAll, false},
            string_manager ! {persistAll, false};

        {persistAndFlushAll} ->
            hash_manager ! {persistAll, true},
            string_manager ! {persistAll, true};

        {flushAll} ->
            hash_manager ! {flushAll},
            string_manager ! {flushAll};

        {getStats, Pid} ->
            hash_manager ! {getStats, self()},
            receive
                {ok, HashStatsInfo} ->
                    HashStats = HashStatsInfo;
                ko ->
                    HashStats = error
            end,

            string_manager ! {getStats, self()},
            receive
                {ok, StringStatsInfo} ->
                    StringStats = StringStatsInfo;
                ko ->
                    StringStats = error
            end,

            volatile_manager ! {getStats, self()},
            receive
                {ok, VolatileStatsInfo} ->
                    VolatileStats = VolatileStatsInfo;
                ko ->
                    VolatileStats = error
            end,
            Pid ! {ok, {NowOpsSec, HashStats, StringStats, VolatileStats}};

        Error ->
            logging ! {add, self(), error, io_lib:format("Commad not recognise~p~n", [Error])}
    end,

    listener_loop(Config, NodeId, NowOpsSec, TimestampSec).

init(Config, NodeId) ->
    register(
        hash_manager,
        spawn(data_hash, init, [Config])),

    register(
        volatile_manager,
        spawn(data_volatile, init, [Config])),

    register(
        string_manager,
        spawn(data_string, init, [Config])),

    listener_loop(Config, NodeId, 0, 0).
