%%
%% This module manages the string data type, handles the get / set and delete actions
%% And controls the persistance of this data
%%

-module(data_string).

-author('alonso.vidales@tras2.es').

-export([
    init/1,
    create_handler_from_warehouse/4]).

warehouse_read(Key) ->
    warehouse ! {self(), get, io_lib:format("str_~s", [Key])},

    receive
        {ok, Value} ->
            Value;
        ko ->
            null
    end.

warehouse_persist(Key, Value) ->
    % Check if value is null, remove it in this case
    case Value of
        null ->
            warehouse ! {self(), del, io_lib:format("str_~s", [Key])},
            logging ! {add, self(), info, io_lib:format("Deleting key: ~s~n", [Key])};
        _Store ->
            warehouse ! {self(), save, io_lib:format("str_~s", [Key]), Value},
            logging ! {add, self(), info, io_lib:format("Persisting key: ~s Value: ~s~n", [Key, Value])}
    end,

    receive
        ok ->
            ok;
        ko ->
            ko
    end.

handler_listener(Ttl, Key, Value) ->
    receive
        {Pid, get, Key} ->
            if
                Value == null ->
                    Pid ! {ok, null};
                true ->
                    Pid ! {ok, {str, "-" ++ Value}}
            end,
            handler_listener(Ttl, Key, Value);

        {Pid, set, NewValue} ->
            % Return 0 if this is a new field, or 1 if the key has an assigned value
            if 
                Pid /= false ->
                    if
                        Value == null -> Pid ! {ok, 1};
                        true -> Pid ! {ok, 0}
                    end;
                true ->
                    false
            end,
            handler_listener(Ttl, Key, NewValue);

        {Pid, persist} ->
            Result = warehouse_persist(Key, Value),
            case Result of
                ok ->
                    logging ! {add, self(), info, io_lib:format("key persisted!!!!: ~s, ~p~n", [Key, {ok, 1}])},
                    Pid ! {ok, 1};
                ko -> 
                    logging ! {add, self(), error, io_lib:format("Problem trying to persist the key ~s~n", [Key])},
                    Pid ! {ok, 0}
            end,

            handler_listener(Ttl, Key, Value);

        Error ->
            logging ! {add, self(), error, io_lib:format("Message not recognised ~p~n", [Error])}

        after Ttl ->
            warehouse_persist(Key, Value),
            logging ! {add, self(), info, io_lib:format("Key ~s persisted~n", [Key])}
    end.
            
            

create_handler_from_warehouse(Pid, Ttl, Key, Init) ->
    case Init of
        true ->
            Value = null;
        false ->
            Value = warehouse_read(Key)
    end,

    Pid ! ok,
    handler_listener(Ttl, Key, Value).

get_handler_name(Key) ->
    list_to_atom(string:concat("str_", Key)).

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

        {Pid, set, Key, [Value, Persist]} ->
            case get_handler(Key) of
                undefined -> Pid ! ko;
                Handler when is_pid(Handler) ->
                    if Persist ->
                        Handler ! {false, set, Value},
                        Handler ! {Pid, persist};
                    true ->
                        Handler ! {Pid, set, Value}
                    end
            end,
            listener_loop(Ttl);

        {Pid, load, Key, Init} ->
            % Check if the data was not previously loaded by another process, this warranties the atomicy
            case get_handler(Key) of
                undefined ->
                    HandlerName = get_handler_name(Key),
                    register(
                        HandlerName,
                        spawn(data_string, create_handler_from_warehouse, [Pid, Ttl, Key, Init]));
                _YetDefined ->
                    false
            end,

            listener_loop(Ttl);

        {Pid, persist, Key} ->
            case get_handler(Key) of
                undefined -> Pid ! ko;
                Handler when is_pid(Handler) ->
                    Handler ! {Pid, persist}
            end,
            listener_loop(Ttl);

        {getStats, Pid} ->
            Pid ! {stringStatsTODO};

        {persistAll, Flush} ->
            TODO = 1;

        {flushAll} ->
            TODO = 1;
            
        Error ->
            logging ! {add, self(), error, io_lib:format("Commad not recognise ~p~n", [Error])}
    end.

init(Config) ->
    listener_loop(
        list_to_integer(dict:fetch("key_ttl", Config))).
