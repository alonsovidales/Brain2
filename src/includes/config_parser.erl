%%
%% This file try to parse a config file, and returns it as a dictionary
%%
%% @author Alonso Vidales <alonso.vidales@tras2.es>
%% @since 2013-03-24
%%

-module(config_parser).

-export([
    parse_file/2]).

%%
%% This function gets a line, and divide it in the key and value
%% returning it as a tuple
%%
%% @return {Key, Value} if is possible to parse the line
%% @return false if the line is a comment, or it is a comented line
%%
parse_config_line(Line, Verbose) ->
    case re:split(Line, "[ *]", [{return, list}]) of
        [Key, Value] ->
            if
                Verbose -> io:format("Configuration param: Key: ~s Value: ~s", [Key, Value]);
                true -> false
            end,
            FirstChar = string:substr(Key, 1, 1),
            if
                FirstChar /= "#" -> {Key, Value};
                true -> false
            end;
        _Default ->
            false
    end.

%%
%% Read a config file from the given IoDevice line by line
%% and call for each line to parse_config_line in order to
%% obtain from the line the key and value
%%
%% @param io_device() IoDevice The file to be parsed
%%
read_config_file(IoDevice, Verbose) ->
    case io:get_line(IoDevice, "") of
        eof ->
            file:close(IoDevice),
            dict:new();
        Line ->
            case parse_config_line(Line, Verbose) of
                {Key, Value} ->
                    dict:store(Key, Value, read_config_file(IoDevice, Verbose));
                _Default ->
                    read_config_file(IoDevice, Verbose)
            end
    end.

parse_file([], _Verbose) ->
    throw("Error, no config file found");

%%
%% This method try to read from the first of the available files given on the
%% File_paths list the configuration, and return it as a dict where the keys
%% are the parameters and the values, the value for each param
%%
%% @param The list of possible config files in sorted by priority
%% @param false or true atom to enable or disable the verbose output
%%
parse_file([ConfigFile | RestOfConfigFiles], Verbose) ->
    case file:open(ConfigFile, [read]) of
        {ok, IoDevice} ->
            if
                Verbose -> io:format("Reading config file from: ~s~n", [ConfigFile]);
                true -> false
            end,
            Config = read_config_file(IoDevice, Verbose);

        {error, _Reason} ->
            Config = parse_file(RestOfConfigFiles, Verbose)
    end,

    Config.
