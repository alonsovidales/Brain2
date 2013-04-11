%%
%% This module contains all the code of the server who communicate with the clients
%% gen_server as server
%%

-module(server_controller).

-author('alonso.vidales@tras2.es').

-export([
    init/2,
    acceptor/3]).

process_command(Command, TimeOut) ->
    % Remove the \r\n at the end of the line added by the client
    StripCommand = string:strip(Command, left),

    case re:split(StripCommand, " +", [{return, list}, {parts, 3}]) of
        [Action, Key] ->
            data_controller ! {self(), Action, Key},
            receive
                {ok, Result} ->
                    Result;
                {ko, Error} ->
                    logging ! {add, self(), error, io_lib:format("Request error: ~p ~n", [Error])},
                    {str, io_lib:format("error Request error: ~p ~n", [Error])}

                after TimeOut ->
                    {str, io_lib:format("error Max execution time exceeded ~p ~n", [TimeOut])}
            end;
                    
        [Action, Key, Args] ->
            data_controller ! {self(), Action, Key, Args},
            receive
                {ok, Result} ->
                    Result;
                {ko, Error} ->
                    logging ! {add, self(), error, io_lib:format("Request error: ~p ~n", [Error])},
                    {str, io_lib:format("error Request error: ~p ~n", [Error])}

                after TimeOut ->
                    {str, io_lib:format("error Max execution time exceeded ~p ~n", [TimeOut])}
            end;
                    
        [Action] ->
            data_controller ! {self(), Action},
            receive
                {ok, Result} ->
                    Result;
                {ko, Error} ->
                    logging ! {add, self(), error, io_lib:format("Request error: ~p ~n", [Error])},
                    {str, io_lib:format("error Request error: ~p ~n", [Error])}

                after TimeOut ->
                    {str, io_lib:format("error Max execution time exceeded ~p ~n", [TimeOut])}
            end;

        Error ->
            logging ! {add, self(), error, io_lib:format("Problem parsing the input: ~p ~n", [Error])},
            {str, io_lib:format("error Problem parsing the input ~p ~n", [Error])}
    end.

return_list(_Socket, []) ->
    true;
return_list(Socket, [Line | Rest]) ->
    gen_tcp:send(Socket, Line),
    return_list(Socket, Rest).

handle(MainPid, Socket, TimeOut) ->
    inet:setopts(Socket, [{active, once}]),

    receive
        {tcp, Socket, "shutdown"} ->
            logging ! {add, self(), info, io_lib:format("Shutting down the node~n", [])},
            % The server will not accept any more query, and wait for 3 seconds until
            % dump all the information in order to ensure that nobody are using it
            logging ! {add, self(), info, io_lib:format("Server closed, sleep for 3 secs...~n", [])},
            timer:sleep(3000),
            logging ! {add, self(), info, io_lib:format("Persist all the keys into the warehouse...~n", [])},
            data_controller ! {shutdown, self()},
            receive
                ok -> true
            end,
            % Remove this node from the ring
            ringManager ! {removeNode, self()},
            receive
                ok -> true
            end,

            timer:sleep(3000),

            gen_tcp:send(Socket, "Node: Shutdown complete"),
            logging ! {add, self(), info, io_lib:format("Execution complete~n", [])},
            mainSleep ! shutdown;

        {tcp, Socket, "quit"} ->
            gen_tcp:close(Socket);

        {tcp, Socket, Command} ->
            case process_command(Command, TimeOut) of
                {str, Result} ->
                    gen_tcp:send(Socket, Result);

                {list, Result} ->
                    return_list(Socket, Result);

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
            logging ! {add, self(), info, io_lib:format("Shutdown complete~n", [])};

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
            spawn(
                server_controller,
                acceptor,
                [
                    self(),
                    Listen,
                    list_to_integer(dict:fetch("server_exec_timeout", Config))
                ]
            ),
            register(
                mainSleep,
                self()),
            sleep_until_shutdown(),
            io_lib:format("Shutdown complete~n", []),
            init:stop();
            
        {error, Message} ->
            logging ! {add, self(), fatal, io_lib:format("Port: ~s, can not be opened: ~p~n", [dict:fetch("server_port", Config), Message])},
            io:format("Error: Port: ~s, can not be opened: ~p~n", [dict:fetch("server_port", Config), Message]),
            throw("Error starting server")
    end.
