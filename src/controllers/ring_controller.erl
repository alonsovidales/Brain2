%%
%% This module manages the string data type, handles the get / set and delete actions
%% And controls the persistance of this data
%%

-module(ring_controller).

-author('alonso.vidales@tras2.es').

-export([
    init/2]).

get_node_pos(NodeId, [], _CurrentPos) ->
    logging ! {add, self(), error, io_lib:format("Node not found on list from managers: ~p ~n", [NodeId])},
    false;

get_node_pos(NodeId, [{CurrentNodeId, _Pid} | Rest], CurrentPos) ->
    if
        CurrentNodeId == NodeId -> CurrentPos;
        true -> get_node_pos(NodeId, Rest, CurrentPos + 1)
    end.

get_left_right_nodes(NodeId, NodesList) ->
    Pos = get_node_pos(NodeId, NodesList, 1),

    case length(NodesList) of
        1 ->
            Nodes = [data_controller, data_controller];
        2 ->
            Nodes = case Pos of
                1 ->
                    {_NodeId, RightPid} = lists:nth(2, NodesList),
                    [data_controller, RightPid];
                _MoreThanOne ->
                    {_NodeId, LeftPid} = lists:nth(1, NodesList),
                    [LeftPid, data_controller]
            end;
        _MoreThanTwo ->
            if
                Pos == length(NodesList) ->
                    {_NodeLeftId, LeftPid} = lists:nth(Pos - 1, NodesList),
                    {_NodeRightId, RightPid} = lists:nth(1, NodesList);

                Pos == 1 ->
                    {_NodeLeftId, LeftPid} = lists:nth(length(NodesList), NodesList),
                    {_NodeRightId, RightPid} = lists:nth(2, NodesList);

                true ->
                    {_NodeLeftId, LeftPid} = lists:nth(Pos - 1, NodesList),
                    {_NodeRightId, RightPid} = lists:nth(Pos + 1, NodesList)
            end,
            Nodes = [LeftPid, RightPid]
    end,
    Nodes.

listener_loop(NodeId, LeftNode, RightNode, Nodes) ->
    receive
        {get, right, Pid} ->
            Pid ! {ok, RightNode},
            listener_loop(NodeId, LeftNode, RightNode, Nodes);
        {get, left, Pid} ->
            Pid ! {ok, LeftNode},
            listener_loop(NodeId, LeftNode, RightNode, Nodes);

        {getHashPosByKey, Key, Pid} ->
            {_NodeId, NodePid} = lists:nth((erlang:crc32(Key) rem length(Nodes)) + 1, Nodes),
            Pid ! NodePid,
            listener_loop(NodeId, LeftNode, RightNode, Nodes);

        % NodesList is a list of {<nodeId>, <Pid>} for each node
        {update, NodesList} ->
            [NewLeftNode, NewRightNode] = get_left_right_nodes(NodeId, NodesList),

            logging ! {add, self(), debug, io_lib:format("Nodes List: ~p ~n", [NodesList])},
            logging ! {add, self(), info, io_lib:format("Updating partner nodes Left: ~p Right: ~p ~n", [NewLeftNode, NewRightNode])},
            listener_loop(NodeId, NewLeftNode, NewRightNode, NodesList);

        {status, Pid} ->
            Pid ! ok,
            listener_loop(NodeId, LeftNode, RightNode, Nodes);

        {monitor, Pid} ->
            {Total, Allocated, Worst} = memsup:get_memory_data(),
            whereis(data_controller) ! {getStats, self()},
            receive
                Stats ->
                    Pid ! {Total, Allocated, Worst, Stats}
            end,
            listener_loop(NodeId, LeftNode, RightNode, Nodes);

        {removeNode, Pid} ->
            logging ! {add, self(), info, io_lib:format("Ring Manager stopped~n", [])},
            Pid ! ok
    end.

%%
%% Register this node on all the available managers
%%
%% @param [Manager | Rest] List The list of managers to register in
%% @param NodeId The unique identifier of the current node
%%
register_node([], _NodeId) ->
    ok;

register_node([Manager | Rest], NodeId) ->
    Manager ! {register, NodeId, self(), whereis(data_controller)},

    receive
        ok ->
            logging ! {add, self(), debug, io_lib:format("Registered node: ~p on manager ~p~n", [NodeId, Manager])};
        ko ->
            logging ! {add, self(), error, io_lib:format("Problem trying to register node: ~p on manager: ~p~n", [NodeId, Manager])}
        after 1000 ->
            logging ! {add, self(), error, io_lib:format("Problem trying to register node: ~p on manager: ~p, connection error~n", [NodeId, Manager])}
    end,

    register_node(Rest, NodeId).

%%
%% Convets a given list of nodes into a list of Pids to stablice the connection
%%
%% @param Managers list() List of strings with the addesses of the nodes to stablice the connection with
%%
convert_to_managers_list([], ManagersList) ->
    ManagersList;

convert_to_managers_list([Manager | Rest], ManagersList) ->
    convert_to_managers_list(Rest, ManagersList ++ [{manager, list_to_atom(string:concat("manager@", Manager))}]).

convert_to_managers_list(Managers) ->
    convert_to_managers_list(Managers, []).

init(Config, NodeId) ->
    % Register this node on the managers 
    ManagerNodes = re:split(dict:fetch("manager_servers", Config), "[,]", [{return, list}]),
    ManagersPids = convert_to_managers_list(ManagerNodes),
    % Register this node on all the available managers
    register_node(ManagersPids, NodeId),

    listener_loop(NodeId, whereis(data_controller), whereis(data_controller), []).
