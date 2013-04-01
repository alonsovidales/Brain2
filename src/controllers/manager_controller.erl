%%
%% This module if used to handle all the external requests who cames
%% from the client as messages
%%

-module(manager_controller).

-author('alonso.vidales@tras2.es').

-export([
    init/1,
    check_node_status/3,
    manager_loop/2]).

check_node_status(CheckInterval, NodeId, NodePid) ->
    io:format("Monitorizing node: ~s, pid: ~p~n", [NodeId, NodePid]),
    timer:sleep(CheckInterval / 2),
    NodePid ! {status, self()},
    receive
        ok ->
            check_node_status(CheckInterval, NodeId, NodePid)
        after CheckInterval / 2 ->
            manager ! {removeNode, NodeId, NodePid}
    end.

set_new_list_to_nodes([], _Nodes) ->
    true;
set_new_list_to_nodes([Node | Rest], Nodes) ->
    Node ! {update, Nodes},
    set_new_list_to_nodes(Rest, Nodes).

manager_loop(CheckInterval, Nodes) ->
    receive
        {removeNode, NodeId, NodePid} -> 
            NewNodes = list:delete({NodeId, NodePid}, Nodes),
            io:format("Removed node from ring: ~p ~n", [NewNodes]),
            manager_loop(CheckInterval, NewNodes);

        {register, NodeId, Pid, ManagerPid} ->
            NewNodes = Nodes ++ {NodeId, ManagerPid},
            io:format("Current nodes ring: ~p ~n", [NewNodes]),
            logging ! {add, self(), error, io_lib:format("Current nodes ring: ~p ~n", [NewNodes])},
            % Send the new list to all the nodes of the ring
            set_new_list_to_nodes(NewNodes, NewNodes),
            % Start the monitorization of the new node
            spawn(manager_controller, check_node_status, [
                CheckInterval,
                NodeId,
                Pid
            ]),
            Pid ! ok,
            manager_loop(CheckInterval, NewNodes);

        Error ->
            logging ! {add, self(), error, io_lib:format("Command not recognised: ~p ~n", [Error])},
            manager_loop(CheckInterval, Nodes)
    end.

init(Config) ->
    register(
        manager,
        spawn_link(manager_controller, manager_loop, [
            list_to_integer(dict:fetch("manager_node_check_interval", Config)),
            []
        ]
    )),

    timer:sleep(infinity).
