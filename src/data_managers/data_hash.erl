%%
%% This module manages the hash data type, handles the get / set and delete actions
%% And controls the persistance of this data on the warehause system
%%

-module(data_hash).

-author('alonso.vidales@tras2.es').

-import(mochijson2).

-export([
    init/1,
    create_handler_from_warehouse/4]).

convert_bin_to_string_kv([], Converted) ->
    Converted;
convert_bin_to_string_kv([{Key, Value} | Rest], Converted) ->
    convert_bin_to_string_kv(Rest, Converted ++ [{binary:bin_to_list(Key), binary:bin_to_list(Value)}]).

struct_to_dict({struct, DictList}) ->
    dict:from_list(convert_bin_to_string_kv(DictList, [])).

warehouse_read(Key) ->
    warehouse ! {self(), get, io_lib:format("hash_~s", [Key])},

    receive
        {ok, Value} ->
            struct_to_dict(mochijson2:decode(Value));
        ko ->
            dict:new()
    end.

get_serialized_str([], _Dict, Serialized) ->
    Serialized;
get_serialized_str([Key | Rest], Dict, Serialized) ->
    case dict:is_key(Key, Dict) of
        true ->
            get_serialized_str(Rest, Dict, Serialized ++ [io_lib:format("~p:~p", [Key, dict:fetch(Key, Dict)])]);
        false ->
            get_serialized_str(Rest, Dict, Serialized)
    end.

serialize(Dict) ->
    "{" ++ string:join(get_serialized_str(dict:fetch_keys(Dict), Dict, ""), ",") ++ "}".

serialize_keys(IntKeys, Dict) ->
    "{" ++ string:join(get_serialized_str(IntKeys, Dict, ""), ",") ++ "}".

warehouse_persist(Key, Value) ->
    % Check if value is null, remove it in this case
    case length(dict:fetch_keys(Value)) of
        0 ->
            warehouse ! {self(), del, io_lib:format("hash_~s", [Key])},
            logging ! {add, self(), info, io_lib:format("Deleting key: ~s~n", [Key])};
        _Store ->
            warehouse ! {self(), save, io_lib:format("hash_~s", [Key]), serialize(Value)},
            logging ! {add, self(), info, io_lib:format("Persisting key: ~s Value: ~p~n", [Key, Value])}
    end,

    receive
        ok ->
            ok;
        ko ->
            ko
    end.

del_dict_keys([], Value) ->
    Value;
del_dict_keys([Key | Rest], Value) ->
    del_dict_keys(Rest, dict:erase(Key, Value)).

handler_listener(Ttl, Key, Value, HandlerName, Inactivity) ->
    receive
        {Pid, get, Key, IntKeys} ->
            case length(dict:fetch_keys(Value)) of
                0 ->
                    Pid ! {ok, null};
                _HaveValues ->
                    if
                        IntKeys == all ->
                            Pid ! {ok, {str, serialize(Value)}};
                        true ->
                            Pid ! {ok, {str, serialize_keys(IntKeys, Value)}}
                    end
            end,
            handler_listener(Ttl, Key, Value, HandlerName, false);

        {Pid, set, IntKey, IntValue} ->
            % Return 0 if this is a new field, or 1 if the key has an assigned value
            if 
                Pid /= false ->
                    case dict:is_key(IntKey, Value) of
                        true -> Pid ! {ok, 0};
                        false -> Pid ! {ok, 1}
                    end;
                true ->
                    false
            end,
            handler_listener(Ttl, Key, dict:store(IntKey, IntValue, Value), HandlerName, false);

        {Pid, del, Key, IntKeys} ->
            if
                IntKeys == all ->
                    NewValue = dict:new();
                true ->
                    NewValue = del_dict_keys(IntKeys, Value)
            end,
            if 
                Pid /= false ->
                    Pid ! {ok, 1};
                true ->
                    false
            end,
            handler_listener(Ttl, Key, NewValue, HandlerName, false);

        {Pid, persist} ->
            Result = warehouse_persist(Key, Value),
            case Result of
                ok ->
                    logging ! {add, self(), info, io_lib:format("key persisted: ~s, ~p~n", [Key, {ok, 1}])},
                    Pid ! {ok, 1};
                ko -> 
                    logging ! {add, self(), error, io_lib:format("Problem trying to persist the key ~s~n", [Key])},
                    Pid ! {ok, 0}
            end,

            handler_listener(Ttl, Key, Value, HandlerName, false);

        Error ->
            logging ! {add, self(), error, io_lib:format("Message not recognised ~p~n", [Error])}

        after Ttl ->
            case Inactivity of
                false ->
                    warehouse_persist(Key, Value),
                    logging ! {add, self(), info, io_lib:format("Key ~s persisted~n", [Key])},
                    handler_listener(Ttl, Key, Value, HandlerName, true);
                true ->
                    hash_manager ! {keyDeleted, HandlerName},
                    logging ! {add, self(), info, io_lib:format("Key ~s removed from memory~n", [Key])}
            end
    end.

create_handler_from_warehouse(Pid, Ttl, Key, HandlerName) ->
    Value = warehouse_read(Key),
    Pid ! ok,
    handler_listener(Ttl, Key, Value, HandlerName, false).

get_handler_name(Key) ->
    list_to_atom(string:concat("hash_", Key)).

get_handler(Key) ->
    HandlerName = get_handler_name(Key),
    whereis(HandlerName).

persists_all([]) ->
    true;

persists_all([Pid | Rest]) ->
    Pid ! {self(), persist},
    receive
        {ok, _Result} ->
            true
    end,
    persists_all(Rest).

listener_loop(Ttl, CurrentKeys, Handlers) ->
    receive
        {Pid, get, Key, [IntKeys]} ->
            case get_handler(Key) of
                undefined ->
                    Pid ! ko;
                Handler when is_pid(Handler) ->
                    Handler ! {Pid, get, Key, IntKeys}
            end;

        {Pid, set, Key, [Persist, [IntKey, IntValue]]} ->
            case get_handler(Key) of
                undefined -> Pid ! ko;
                Handler when is_pid(Handler) ->
                    if Persist ->
                        Handler ! {false, set, IntKey, IntValue},
                        Handler ! {Pid, persist};
                    true ->
                        Handler ! {Pid, set, IntKey, IntValue}
                    end
            end;

        {Pid, del, Key, [Persist, IntKeys]} ->
            case get_handler(Key) of
                undefined ->
                    Pid ! ko;
                Handler when is_pid(Handler) ->
                    if Persist ->
                        Handler ! {false, del, Key, IntKeys},
                        Handler ! {Pid, persist};
                    true ->
                        Handler ! {Pid, del, Key, IntKeys}
                    end
            end;

        {Pid, load, Key, _Init} ->
            % Check if the data was not previously loaded by another process, this warranties the atomicy
            case get_handler(Key) of
                undefined ->
                    HandlerName = get_handler_name(Key),
                    register(
                        HandlerName,
                        spawn(data_hash, create_handler_from_warehouse, [Pid, Ttl, Key, HandlerName])),

                    listener_loop(Ttl, CurrentKeys + 1, Handlers ++ [HandlerName]);
                _YetDefined ->
                    listener_loop(Ttl, CurrentKeys + 1, Handlers)
            end;

        {Pid, persist, Key} ->
            case get_handler(Key) of
                undefined -> Pid ! ko;
                Handler when is_pid(Handler) ->
                    Handler ! {Pid, persist}
            end;

        {keyDeleted, HandlerName} ->
            listener_loop(Ttl, CurrentKeys - 1, lists:delete(HandlerName, Handlers));

        {getStats, Pid} ->
            Pid ! {ok, CurrentKeys};

        {shutdown, Pid} ->
            persists_all(Handlers),
            Pid ! ok;
            
        Error ->
            logging ! {add, self(), error, io_lib:format("Commad not recognise ~p~n", [Error])}
    end,
    listener_loop(Ttl, CurrentKeys, Handlers).

init(Config) ->
    listener_loop(list_to_integer(dict:fetch("key_ttl", Config)), 0, []).
