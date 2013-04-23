%%
%% This file is used to run the nodes and managers systems
%%

-module(bootstrap).

-author('alonso.vidales@tras2.es').

-export([
    start_node/0,
    start_manager/0]).

-import(config_parser).
-import(logging).
-import(ring_controller).
-import(data_controller).
-import(manager_controller).
-import(ftp_manager).

get_config(Verbose) ->
    ConfigFiles = [
        "/etc/brain_node.conf",
        "/etc/brain/brain_node.conf",
        "etc/brain_node.conf"],

    case Verbose of
        {ok, [["true"]]} ->
            config_parser:parse_file(ConfigFiles, true);
        _Default ->
            config_parser:parse_file(ConfigFiles, false)
    end.


start_node() ->
    Config = get_config(init:get_argument(verbose)),
    io:format("SECRET: ~s~n", [dict:fetch("secret", Config)]),
    erlang:set_cookie(node(), list_to_atom(dict:fetch("secret", Config))),

    case init:get_argument(node_id) of
        {ok, [[NodeId]]} ->
            register(
                logging,
                spawn_link(logging, init, [Config, NodeId])),

            case dict:fetch("warehouse_manager", Config) of
                "ftp" ->
                    WarehouseStatus = ftp_manager:init(Config)
            end,

            case WarehouseStatus of
                ko ->
                    io:format("Trying to connect with warehouse server~n", []);
                ok ->
                    register(
                        data_controller,
                        spawn_link(data_controller, init, [Config, NodeId])),

                    register(
                        ringManager,
                        spawn_link(ring_controller, init, [Config, NodeId])),

                    server_controller:init(Config, init:get_argument(port))
            end;
                
        _Error ->
            io:format("Error: Use the parameter -node_id <node_id> to specify a unique identifier for this node")
    end.

start_manager() ->
    Config = get_config(init:get_argument(verbose)),
    erlang:set_cookie(node(), list_to_atom(dict:fetch("secret", Config))),

    register(
        logging,
        spawn_link(logging, init, [Config, <<"manager">>])),

    manager_controller:init(Config).
