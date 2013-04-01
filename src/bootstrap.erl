%%
%% This file is used to run the nodes and managers systems
%%

-module(bootstrap).

-author('alonso.vidales@tras2.es').

-export([
    start_node/0]).

-import(config_parser).
-import(logging).
-import(ring_controller).
-import(data_controller).

start_node() ->
    io:format("Loading config files~n"),
    ConfigFiles = [
        "etc/brain_node.conf",
        "/etc/brain/brain_node.conf",
        "/etc/brain_node.conf"],

    io:format("Parsing config~n"),
    case init:get_argument(verbose) of
        {ok, [["true"]]} ->
            Config = config_parser:parse_file(ConfigFiles, true);
        _Default ->
            Config = config_parser:parse_file(ConfigFiles, false)
    end,

    case init:get_argument(node_id) of
        {ok, [[NodeId]]} ->
            register(
                logging,
                spawn_link(logging, init, [Config])),

            register(
                data_controller,
                spawn_link(data_controller, init, [Config, NodeId])),

            register(
                ringManager,
                spawn_link(ring_controller, init, [Config, NodeId])),

            server_controller:init(Config);
        true ->
            io:format("Error: Use the parameter -node_id <node_id> to specify a unique identifier for this node")
    end.
