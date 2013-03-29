%%
%% This file is used to run the nodes and managers systems
%%

-module(bootstrap).

-author('alonso.vidales@tras2.es').

-export([
    start_node/0]).

-import(config_parser).
-import(logging).
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

    register(
        logging,
        spawn(logging, init, [Config])),

    register(
        ringManager,
        spawn(ring_manager, init, [Config])),

    register(
        main,
        spawn(data_controller, init, [Config, init:get_argument(node_id)])).
