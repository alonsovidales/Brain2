%%
%% This module manages the string data type, handles the get / set and delete actions
%% And controls the persistance of this data
%%

-module(data_volatile).

-author('alonso.vidales@tras2.es').

-export([
    init/1,
    create_handler/3]).

handler_listener(Ttl, Key, Value) ->
    receive
        {Pid, get, Key} ->
            if
                Value == null ->
                    Pid ! {ok, "null"};
                true ->
                    Pid ! {ok, Value}
            end,
            handler_listener(Ttl, Key, Value);

        {Pid, set, NewValue} ->
            % Return 0 if this is a new field, or 1 if the key has an assigned value
            if 
                Pid /= false ->
                    if
                        Value == null -> Pid ! {ok, "1"};
                        true -> Pid ! {ok, "0"}
                    end;
                true ->
                    false
            end,
            handler_listener(Ttl, Key, NewValue);

        Error ->
            logging ! {add, self(), error, io_lib:format("Message not recognised ~p~n", [Error])}

        after Ttl ->
            false
    end.
            
            

create_handler(Pid, Ttl, Key) ->
    Pid ! ok,
    handler_listener(Ttl, Key, null).

get_handler_name(Key) ->
    list_to_atom(string:concat("vol_", Key)).

get_handler(Key) ->
    HandlerName = get_handler_name(Key),
    whereis(HandlerName).

listener_loop(Ttl) ->
    receive
        {Pid, get, Key} ->
            case get_handler(Key) of
                undefined ->
                    Pid ! ko;
                Handler when is_pid(Handler) ->
                    Handler ! {Pid, get, Key}
            end,
            listener_loop(Ttl);

        {Pid, set, Key, [Value]} ->
            case get_handler(Key) of
                undefined ->
                    Pid ! ko;
                Handler when is_pid(Handler) ->
                    Handler ! {Pid, set, Value}
            end,
            listener_loop(Ttl);

        {Pid, load, Key} ->
            case get_handler(Key) of
                undefined ->
                    HandlerName = get_handler_name(Key),
                    register(
                        HandlerName,
                        spawn(data_volatile, create_handler, [Pid, Ttl, Key]));
                _YetDefined ->
                    false
            end,

            listener_loop(Ttl);

        {Pid, persist, _Key} ->
            Pid ! ok,
            listener_loop(Ttl);

        {getStats, Pid} ->
            Pid ! {volatileStatsTODO};

        {persistAll, _Flush} ->
            ok;

        {flushAll} ->
            ok;
            
        Error ->
            logging ! {add, self(), error, io_lib:format("Commad not recognise ~p~n", [Error])}
    end.

init(Config) ->
    listener_loop(
        list_to_integer(dict:fetch("key_ttl", Config))).
