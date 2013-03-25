%%
%% This file is used to run the nodes and managers systems
%%
%% @author Alonso Vidales <alonso.vidales@tras2.es>
%% @since 2013-03-24
%%

-module(bootstrap).

-export([
    start_node/0]).

-compile("includes/config_parser.erl").

start_node() ->
    ConfigFiles = [
        "etc/brain_node.conf",
        "/etc/brain/brain_node.conf",
        "/etc/brain_node.conf"],

    case init:get_argument(verbose) of
        {ok, [["true"]]} ->
            Config = config_parser:parse_file(ConfigFiles, true);
        _Default ->
            Config = config_parser:parse_file(ConfigFiles, false)
    end,

    io:format("Config File: ~s~n", [Config]).
