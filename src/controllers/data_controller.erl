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

action_executor(Action, Handler, Key, Params, Redundance, NodeId, PidToConfirm, LoadFromPersistance) ->
    if LoadFromPersistance == true ->
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
        ko ->
            logging ! {add, self(), info, io_lib:format("No data found on node~s~n", [NodeId])},
            % Search on the rest of nodes of the Ring
            ringManager ! {get, right},
            receive
                {ok, NodePid} ->
                    % Execute the action on the right node
                    NodePid ! {ie, NodeId, Action, Handler, Key, Params, PidToConfirm};
                _NoNode ->
                    logging ! {add, self(), info, io_lib:format("No node at the right~n")}
                after 100 ->
                    logging ! {add, self(), error, io_lib:format("Ring manager doesn't respond~n")}
            end;

        {ok, Result} ->
            if PidToConfirm /= false ->
                PidToConfirm ! {ok, Result}
            end

            %if (Redundance > 0) and (Action /= get) ->
            %    ringManager ! {get, right},
            %    receive
            %        {ok, NodePid} ->
            %            % Execute the action on the right node
            %            NodePid ! {ir, NodeId, Action, Handler, Key, Params, Redundance - 1};
            %        _NoNode ->
            %            logging ! {add, self(), info, io_lib:format("No node at the right~n")};
            %        after ?100 ->
            %            logging ! {add, self(), error, io_lib:format("Ring manager doesn't respond~n")}
            %    end;
    end.

listener_loop(Config, NodeId) ->
    receive
        % Internal call to try to execute an action on the memory of one of the nodes
        {ie, InNodeId, InAction, InHandler, InKey, InParams, InPidToConfirm} ->
            spawn(data_controller, action_executor, [
                InAction,
                InHandler,
                InKey,
                InParams,
                list_to_integer(dict:fetch("redundance", Config)),
                InNodeId,
                InPidToConfirm,
                InNodeId == NodeId]),
            listener_loop(Config, NodeId),
            ExecutorParams = false;

        % Internal call to keep the redundance, execute the action on the InRedundance - 1 next nodes
        {ir, InNodeId, InAction, InHandler, InKey, InParams, InRedundance} ->
            if InNodeId == NodeId ->
                listener_loop(Config, NodeId)
            end,
            spawn(data_controller, action_executor, [
                InAction,
                InHandler,
                InKey,
                InParams,
                InRedundance - 1,
                InNodeId,
                false,
                false]),
            listener_loop(Config, NodeId),
            ExecutorParams = false;

        {get, Key, Pid} ->
            ExecutorParams = [get, Key, string_manager, Pid, []];
        {set, Key, Value} ->
            ExecutorParams = [set, Key, string_manager, false, [Value, false]];
        {set, Key, Value, Persist} ->
            ExecutorParams = [set, Key, string_manager, false, [Value, Persist]];
        {set, Key, Value, Persist, Pid} ->
            ExecutorParams = [set, Key, string_manager, Pid, [Value, Persist]];
        {del, Key} ->
            ExecutorParams = [del, Key, string_manager, false, []];
        {del, Key, Pid} ->
            ExecutorParams = [del, Key, string_manager, Pid, []];

        {vget, Key, Pid} ->
            ExecutorParams = [get, Key, volatile_manager, Pid, []];
        {vset, Key, Value, Ttl} ->
            ExecutorParams = [set, Key, volatile_manager, false, [Value, Ttl]];
        {vdel, Key} ->
            ExecutorParams = [del, Key, volatile_manager, false, []];

        {hget, Key, IntKeys, Pid} ->
            ExecutorParams = [get, Key, hash_manager, Pid, [IntKeys]];
        {hget, Key, Pid} ->
            ExecutorParams = [get, Key, hash_manager, Pid, [all]];
        {hset, Key, IntKeyValues} ->
            ExecutorParams = [set, Key, hash_manager, false, [IntKeyValues, false]];
        {hset, Key, IntKeyValues, Persist} ->
            ExecutorParams = [set, Key, hash_manager, false, [IntKeyValues, Persist]];
        {hset, Key, IntKeyValues, Persist, Pid} ->
            ExecutorParams = [set, Key, hash_manager, Pid, [IntKeyValues, Persist]];
        {hdelall, Key} ->
            ExecutorParams = [del, Key, hash_manager, false, [all]];
        {hdelall, Key, Pid} ->
            ExecutorParams = [del, Key, hash_manager, Pid, [all, true]];
        {hdelall, Key, Pid, Persist} ->
            ExecutorParams = [del, Key, hash_manager, Pid, [all, Persist]];
        {hdel, Key, IntKeys} ->
            ExecutorParams = [del, Key, hash_manager, false, [IntKeys, false]];
        {hdel, Key, IntKeys, Persist} ->
            ExecutorParams = [del, Key, hash_manager, false, [IntKeys, Persist]];
        {hdel, Key, IntKeys, Persist, Pid} ->
            ExecutorParams = [del, Key, hash_manager, Pid, [IntKeys, Persist]];

        {persistAll} ->
            hash_manager ! {persistAll, false},
            string_manager ! {persistAll, false},
            listener_loop(Config, NodeId),
            ExecutorParams = false;

        {persistAndFlushAll} ->
            hash_manager ! {persistAll, true},
            string_manager ! {persistAll, true},
            listener_loop(Config, NodeId),
            ExecutorParams = false;

        {flushAll} ->
            hash_manager ! {flushAll},
            string_manager ! {flushAll},
            listener_loop(Config, NodeId),
            ExecutorParams = false;

        Error ->
            logging ! {add, self(), error, io_lib:format("Commad not recognise~p~n", [Error])},
            listener_loop(Config, NodeId),
            ExecutorParams = false
    end,
   
    [ExAction, ExKey, ExHandler, ExPidToConfirm, ExParams] = ExecutorParams,
    % Launch a new process to execute the action async, this kind of actions can take too much time
    spawn(data_controller, action_executor, [
        ExAction,
        ExHandler,
        ExKey,
        ExParams,
        list_to_integer(dict:fetch("redundance", Config)),
        NodeId,
        ExPidToConfirm,
        false]),

    listener_loop(Config, NodeId).

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

    listener_loop(Config, NodeId).
