%%
%% This module contains all the code of the server who communicate with the clients
%% gen_server as server
%%

-module(server_controller).

-author('alonso.vidales@tras2.es').

-export([
    init/1]).

process_command(Command, TimeOut) ->
    % Remove the \r\n at the end of the line added by the client
    StripCommand = string:strip(string:substr(Command, 1, string:len(Command) - 2), left),

    case re:split(StripCommand, " +", [{return, list}, {parts, 3}]) of
        [Action, Key, Args] ->
            data_controller ! {self(), Action, Key, Args},
            receive
                {ok, Result} ->
                    Result;
                {ko, Error} ->
                    logging ! {add, self(), error, io_lib:format("Request error: ~p ~n", [Error])},
                    io_lib:format("Request error: ~p ~n", [Error])

                after TimeOut ->
                    io_lib:format("Max execution time exceeded ~p ~n", [TimeOut])
            end;
                    
        _Error ->
            io_lib:format("Problem parsing the input~n")
    end.

handle(MainPid, Socket, TimeOut) ->
    inet:setopts(Socket, [{active, once}]),

    receive
        {tcp, Socket, "shutdown\r\n"} ->
            gen_tcp:close(Socket),
            MainPid ! shutdown;

        {tcp, Socket, "quit\r\n"} ->
            gen_tcp:close(Socket);

        {tcp, Socket, Command} ->
            gen_tcp:send(
                Socket,
                process_command(Command, TimeOut)),
            handle(MainPid, Socket, TimeOut)
    end.

acceptor(MainPid, ListenSocket, TimeOut) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> acceptor(MainPid, ListenSocket, TimeOut) end),
            handle(MainPid, Socket, TimeOut);

        _Error ->
            logging ! {add, self(), debug, io_lib:format("Socket closed...~n", [])}
    end.

sleep_until_shutdown() ->
    receive
        shutdown ->
            logging ! {add, self(), info, io_lib:format("Shutdown order received, stopping server...~n", [])};

        _Error ->
            sleep_until_shutdown()
    end.

init(Config) ->
    io:format("Starting server on port: ~s ...~n", [dict:fetch("server_port", Config)]),
    
    case gen_tcp:listen(
        list_to_integer(dict:fetch("server_port", Config)),
        [
            list,
            {active, false}
        ]) of
        {ok, Listen} ->
            logging ! {add, self(), info, io_lib:format("Server listening on port: ~s~n", [dict:fetch("server_port", Config)])},
            acceptor(
                self(),
                Listen,
                list_to_integer(dict:fetch("server_exec_timeout", Config))
            ),
            sleep_until_shutdown();
            
        {error, Message} ->
            logging ! {add, self(), fatal, io_lib:format("Port: ~s, can not be opened: ~p~n", [dict:fetch("server_port", Config), Message])},
            io:format("Error: Port: ~s, can not be opened: ~p~n", [dict:fetch("server_port", Config), Message]),
            throw("Error starting server")
    end.
