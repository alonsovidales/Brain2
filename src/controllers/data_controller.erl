%%
%% This module if used to handle all the external requests who cames
%% from the client as messages
%%

-module(data_controller).

-author('alonso.vidales@tras2.es').

-export([
    init/2,
    action_executor/7]).

-import(data_hash).
-import(data_volatile).
-import(data_string).

action_executor(Action, Handler, Key, Params, NodeId, PidToConfirm, RingComplete) ->
    logging ! {add, self(), debug, io_lib:format("Executing action: ~p~n", [[Action, Handler, Key, Params, NodeId, PidToConfirm, RingComplete]])},
    % This is the last iteration trying to load the key from memory on one of the nodes of the ring,
    % and this is the node where the query was done. Load the data from the data warehouse, and try
    % to do the action against the loaded data
    if 
        RingComplete ->
            % Check if you are the only one locking for this data key, in case of don't be, check againg the ring
            ringManager ! {getHashPosByKey, Key, self()},
            receive
                NodeAcqPid ->
                    NodeAcqPid ! {self(), acquireHandler, Key},

                    % Ok, we are the only one asking fot this key, load it from the warehouse
                    receive
                        ok ->
                            Handler ! {self(), load, Key, Action == set},

                            receive
                                ko ->
                                    throw(io_lib:format("Problem trying to load key ~s from Warehouse~n", [Key]));
                                ok ->
                                    logging ! {add, self(), info, io_lib:format("Loaded key ~s from Warehouse~n", [Key])}
                            end;
                            
                        ko ->
                            false
                    end,
                    NodeAcqPid ! {releaseHandler, Key}
            end;

        true ->
            false
    end,

    % Try to execute the action on memory only
    if
        length(Params) == 0 ->
            Handler ! {self(), Action, Key};
        true ->
            Handler ! {self(), Action, Key, Params}
    end,

    receive
        % The data is not in memory on the local node, try to execute the action on the node at the right
        ko ->
            logging ! {add, self(), info, io_lib:format("No data found on node: ~s~n", [NodeId])},
            % Search on the rest of nodes of the Ring
            ringManager ! {get, right, self()},
            receive
                {ok, NodePid} ->
                    % Execute the action on the right node
                    NodePid ! {ie, NodeId, Action, Handler, Key, Params, PidToConfirm};
                _NoNode ->
                    logging ! {add, self(), info, io_lib:format("No node at the right~n", [])}
                after 100 ->
                    logging ! {add, self(), error, io_lib:format("Ring manager doesn't respond~n", [])}
            end;

        % The data was found, return the result to the server
        {ok, Result} ->
            PidToConfirm ! {ok, Result}
    end.

%%
%% This method will listen for incomming messages from the client, and on a new process will attend them
%% Acts as an adapter of the different data type managers.
%%
listener_loop(NodeId, LastSecOps, OpsSec, Timestamp, LockedHandlers) ->
    % Used to get stats of ops / sec, etc
    {_Mega, TimestampSec, _Micro} = now(),
    if
        Timestamp == TimestampSec ->
            NowOpsSec = OpsSec + 1,
            NewLastSecOps = LastSecOps;
        true ->
            NewLastSecOps = OpsSec,
            NowOpsSec = 1
    end,

    receive
        % Internal call to try to execute an action on the memory of one of the nodes
        {ie, InNodeId, InAction, InHandler, InKey, InParams, InPidToConfirm} ->
            spawn(data_controller, action_executor, [
                InAction,
                InHandler,
                InKey,
                InParams,
                InNodeId,
                InPidToConfirm,
                InNodeId == NodeId]);

        %%
        %% Strings data type actions
        %%

        {Pid, "get", Key} ->
            spawn(
                data_controller,
                action_executor,
                [get, string_manager, Key, [], NodeId, Pid, false]);

        {Pid, "set", Key, Value} ->
            spawn(
                data_controller,
                action_executor,
                [set, string_manager, Key, [Value, false], NodeId, Pid, false]);

        {Pid, "pset", Key, Value} ->
            spawn(
                data_controller,
                action_executor,
                [set, string_manager, Key, [Value, true], NodeId, Pid, false]);

        {Pid, "del", Key} ->
            spawn(
                data_controller,
                action_executor,
                [set, string_manager, Key, [null, false], NodeId, Pid, false]);

        {Pid, "pdel", Key} ->
            spawn(
                data_controller,
                action_executor,
                [set, string_manager, Key, [null, true], NodeId, Pid, false]);

        %%
        %% Volatile data type actions
        %%
        {Pid, "vget", Key} ->
            spawn(
                data_controller,
                action_executor,
                [get, volatile_manager, Key, [], NodeId, Pid, false]);

        {Pid, "vset", Key, Value} ->
            spawn(
                data_controller,
                action_executor,
                [set, volatile_manager, Key, [Value], NodeId, Pid, false]);

        {Pid, "vdel", Key} ->
            spawn(
                data_controller,
                action_executor,
                [set, volatile_manager, Key, [null], NodeId, Pid, false]);

        %%
        %% Hash data type actions
        %%
        {Pid, "hget", Key} ->
            spawn(
                data_controller,
                action_executor,
                [get, hash_manager, Key, [all], NodeId, Pid, false]);

        {Pid, "hget", Key, IntKeys} ->
            spawn(
                data_controller,
                action_executor,
                [get, hash_manager, Key, [string:tokens(IntKeys, " ")], NodeId, Pid, false]);

        {Pid, "hsetkv", Key, IntKeyValue} ->
            spawn(
                data_controller,
                action_executor,
                [set, hash_manager, Key, [false, re:split(IntKeyValue, " ", [{return, list}, {parts, 2}])], NodeId, Pid, false]);

        {Pid, "hpsetkv", Key, IntKeyValue} ->
            spawn(
                data_controller,
                action_executor,
                [set, hash_manager, Key, [true, re:split(IntKeyValue, " ", [{return, list}, {parts, 2}])], NodeId, Pid, false]);

        {Pid, "hdel", Key} ->
            spawn(
                data_controller,
                action_executor,
                [del, hash_manager, Key, [false, all], NodeId, Pid, false]);

        {Pid, "hdel", Key, IntKeys} ->
            spawn(
                data_controller,
                action_executor,
                [del, hash_manager, Key, [false, string:tokens(IntKeys, " ")], NodeId, Pid, false]);

        {Pid, "hpdel", Key} ->
            spawn(
                data_controller,
                action_executor,
                [del, hash_manager, Key, [true, all], NodeId, Pid, false]);

        {Pid, "hpdel", Key, IntKeys} ->
            spawn(
                data_controller,
                action_executor,
                [del, hash_manager, Key, [true, string:tokens(IntKeys, " ")], NodeId, Pid, false]);

        % Used as traffic light in order to avoid possible duplicated datas on different ring nodes
        {Pid, acquireHandler, HandlerName} ->
            case sets:is_element(HandlerName, LockedHandlers) of
                true ->
                    Pid ! ko,
                    logging ! {add, self(), debug, io_lib:format("Key ~p yet acquired by another node~n", [HandlerName])};
                false ->
                    Pid ! ok,
                    logging ! {add, self(), debug, io_lib:format("Key ~p acquired~n", [HandlerName])},
                    listener_loop(NodeId, NewLastSecOps, NowOpsSec, TimestampSec, sets:add_element(HandlerName, LockedHandlers))
            end;

        {releaseHandler, HandlerName} ->
            logging ! {add, self(), debug, io_lib:format("Key ~p released~n", [HandlerName])},
            listener_loop(NodeId, NewLastSecOps, NowOpsSec, TimestampSec, sets:del_element(HandlerName, LockedHandlers));

        {checkAlive, Pid} ->
            Pid ! ok;

        {shutdown, Pid} ->
            hash_manager ! {shutdown, self()},
            receive
                ok ->
                    logging ! {add, self(), info, io_lib:format("Hash data type dumped~n", [])};
                ko ->
                    logging ! {add, self(), error, io_lib:format("Error trying to dump Hash data type~n", [])}
            end,

            string_manager ! {shutdown, self()},
            receive
                ok ->
                    logging ! {add, self(), info, io_lib:format("String data type dumped~n", [])};
                ko ->
                    logging ! {add, self(), error, io_lib:format("Error trying to dump Strung data type~n", [])}
            end,
            Pid ! ok;

        {update, Nodes} ->
            ringManager ! {update, Nodes};

        {Pid, "info"} ->
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
            % NowOpsSec
            Pid ! {ok, [erlang:memory() ++ [{ops_sec, NewLastSecOps}] ++ [{hash_keys, HashStats}] ++ [{strings_keys, StringStats}] ++ [{volatile_keys, VolatileStats}]]};

        Error ->
            logging ! {add, self(), error, io_lib:format("Commad not recognise~p~n", [Error])}
    end,

    listener_loop(NodeId, NewLastSecOps, NowOpsSec, TimestampSec, LockedHandlers).

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

    listener_loop(
        NodeId,
        0,
        0,
        0,
        sets:new()).
