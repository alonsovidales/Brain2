%%
%% This module manages the string data type, handles the get / set and delete actions
%% And controls the persistance of this data
%%

-module(data_string).

-author('alonso.vidales@tras2.es').

-export([
    init/1,
    create_handler_from_warehouse/3]).

warehouse_persist(Key, Value) ->
    logging ! {add, self(), info, io_lib:format("Persisting key: ~s Value: ~s~n", [Key, Value])},
    ok.

warehouse_del(Key) ->
    logging ! {add, self(), info, io_lib:format("Deleting key: ~s~n", [Key])},
    ok.

handler_listener(Ttl, Key, Value) ->
    receive
        {Pid, get} ->
            Pid ! {ok, Value},
            handler_listener(Ttl, Key, Value);

        {Pid, set, NewValue} ->
            if Pid /= false ->
                Pid ! ok
            end,
            handler_listener(Ttl, Key, NewValue);

        {Pid, persist} ->
            Result = warehouse_persist(Key, Value),
            if Result == ko -> 
                logging ! {add, self(), error, io_lib:format("Problem trying to persist the key ~s~n", [Key])}
            end,

            if Pid /= false ->
                Pid ! Result
            end,
            handler_listener(Ttl, Key, Value);

        Error ->
            logging ! {add, self(), error, io_lib:format("Message not recognised ~p~n", [Error])}

        after Ttl ->
            case Value of
                null ->
                    warehouse_del(Key);
                Value ->
                    warehouse_persist(Key, Value)
            end,
            logging ! {add, self(), info, io_lib:format("Key ~s persisted~n", [Key])}
    end.
            
            

create_handler_from_warehouse(Pid, Config, Key) ->
    % TODO: Get the value from the warehouse
    Value = "From Warehouse...",
    Pid ! ok,
    handler_listener(list_to_integer(dict:fetch("key_ttl", Config)), Key, Value).

get_handler_name(Key) ->
    list_to_atom(string:concat("str_", Key)).

get_handler(Key) ->
    HandlerName = get_handler_name(Key),
    whereis(HandlerName).

listener_loop(Config) ->
    receive
        {Pid, get, Key, _Params} ->
            case get_handler(Key) of
                undefined ->
                    Pid ! ko;
                Handler when is_pid(Handler) ->
                    Handler ! {Pid, get}
            end,
            listener_loop(Config);

        {Pid, set, Key, [Value, Persist]} ->
            case get_handler(Key) of
                undefined -> Pid ! ko;
                Handler when is_pid(Handler) ->
                    if Persist == true ->
                        Handler ! {false, set},
                        Handler ! {Pid, persist};
                    true ->
                        Handler ! {Pid, set, Value}
                    end
            end,
            listener_loop(Config);

        {Pid, del, Key, [Persist]} ->
            case get_handler(Key) of
                undefined -> Pid ! ko;
                Handler when is_pid(Handler) ->
                    if Persist == true ->
                        Handler ! {false, set, null},
                        Handler ! {Pid, persist};
                    true ->
                        Handler ! {Pid, set, null}
                    end
            end,
            listener_loop(Config);

        {Pid, load, Key} ->
            HandlerName = get_handler_name(Key),
            register(
                HandlerName,
                spawn(data_string, create_handler_from_warehouse, [Pid, Config, Key])),
            listener_loop(Config);

        {Pid, persist, Key} ->
            case get_handler(Key) of
                undefined -> Pid ! ko;
                Handler when is_pid(Handler) ->
                    Handler ! {Pid, persist}
            end,
            listener_loop(Config);

        {getStats, Pid} ->
            Pid ! {stringStatsTODO};

        {persistAll, Flush} ->
            TODO = 1;
        {flushAll} ->
            TODO = 1;
            
        Error ->
            logging ! {add, self(), error, io_lib:format("Commad not recognise~p~n", [Error])}
    end.

init(Config) ->
    listener_loop(Config).
