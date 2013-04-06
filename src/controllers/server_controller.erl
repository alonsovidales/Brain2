%%
%% This module contains all the code of the server who communicate with the clients
%% gen_server as server
%%

-module(server_controller).

-author('alonso.vidales@tras2.es').

-export([
    init/2]).

process_command(Command, TimeOut) ->
    % Remove the \r\n at the end of the line added by the client
    StripCommand = string:strip(string:substr(Command, 1, string:len(Command) - 2), left),

    case re:split(StripCommand, " +", [{return, list}, {parts, 3}]) of
        [Action, Key] ->
            data_controller ! {self(), Action, Key},
            receive
                {ok, Result} ->
                    Result;
                {ko, Error} ->
                    logging ! {add, self(), error, io_lib:format("Request error: ~p ~n", [Error])},
                    {str, io_lib:format("Request error: ~p ~n", [Error])}

                after TimeOut ->
                    {str, io_lib:format("Max execution time exceeded ~p ~n", [TimeOut])}
            end;
                    
        [Action, Key, Args] ->
            data_controller ! {self(), Action, Key, Args},
            receive
                {ok, Result} ->
                    Result;
                {ko, Error} ->
                    logging ! {add, self(), error, io_lib:format("Request error: ~p ~n", [Error])},
                    {str, io_lib:format("Request error: ~p ~n", [Error])}

                after TimeOut ->
                    {str, io_lib:format("Max execution time exceeded ~p ~n", [TimeOut])}
            end;
                    
        Error ->
            logging ! {add, self(), error, io_lib:format("Problem parsing the input: ~p ~n", [Error])},
            {str, io_lib:format("Problem parsing the input ~p ~n", [Error])}
    end.

return_lsit(_Socket, []) ->
    true;
return_lsit(Socket, [Line | Rest]) ->
    gen_tcp:send(Socket, Line),
    return_lsit(Socket, Rest).

handle(MainPid, Socket, TimeOut) ->
    inet:setopts(Socket, [{active, once}]),

    receive
        {tcp, Socket, "shutdown\r\n"} ->
            gen_tcp:close(Socket),
            MainPid ! shutdown;

        {tcp, Socket, "quit\r\n"} ->
            gen_tcp:close(Socket);

        {tcp, Socket, Command} ->
            case process_command(Command, TimeOut) of
                {str, Result} ->
                    gen_tcp:send(Socket, Result);

                {list, Result} ->
                    return_lsit(Socket, Result);

                Result ->
                    gen_tcp:send(
                        Socket,
                        io_lib:format("~w", [Result]))
            end,
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

init(Config, DefPort) ->
    Port = case DefPort of
        {ok, [[PortNum]]} ->
            list_to_integer(PortNum);
        _NoDefined ->
            list_to_integer(dict:fetch("server_port", Config))
    end,

    io:format("Starting server on port: ~p ...~n", [Port]),
   
    case gen_tcp:listen(
        Port,
        [
            list,
            {active, false}
        ]) of
        {ok, Listen} ->
            io:format("~n", []),
            io:format(" '||'''|,~n", []),
            io:format("  ||   ||                  ''~n", []),
            io:format("  ||;;;;   '||''|  '''|.   ||  `||''|,~n", []),
            io:format("  ||   ||   ||    .|''||   ||   ||  ||~n", []),
            io:format(" .||...|'  .||.   `|..||. .||. .||  ||.~n~n", []),
            io:format("Author: Alonso Vidales <alonso.vidales@tras2.es>~n", []),
            io:format("Node ready and listening on port: ~p~n", [Port]),
            logging ! {add, self(), info, io_lib:format("Server listening on port: ~p~n", [Port])},
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
