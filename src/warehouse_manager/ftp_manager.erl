%%
%% This module manages the string data type, handles the get / set and delete actions
%% And controls the persistance of this data
%%

-module(ftp_manager).

-author('alonso.vidales@tras2.es').

-export([
    init/1,
    listener/5]).

exec_action(Action, Pid, Host, Port, User, Pass, BaseDir, Key, Value) ->
    case ftp:open(Host, [{port, Port}]) of
        {ok, FtpPid} ->
            case ftp:user(FtpPid, User, Pass) of
                ok ->
                    case ftp:cd(FtpPid, BaseDir) of
                        ok ->
                            logging ! {add, self(), info, io_lib:format("Connected to warehouse~n", [])},

                            case Action of
                                get ->
                                    case ftp:recv_bin(FtpPid, Key) of
                                        {ok, Content} ->
                                            Pid ! {ok, binary:bin_to_list(Content)};
                                        {error, _Reason} ->
                                            Pid ! ko
                                    end;
                                save ->
                                    case ftp:send_bin(FtpPid, binary:list_to_bin(Value), Key) of
                                        ok ->
                                            Pid ! ok;
                                        {error, Reason} ->
                                            logging ! {add, self(), error, io_lib:format("Trying to store Key: ~p Error: ~p~n", [Key, Reason])},
                                            Pid ! ko
                                    end;
                                del ->
                                    case ftp:delete(FtpPid, Key) of
                                        ok ->
                                            Pid ! ok;
                                        {error, _Reason} ->
                                            Pid ! ko
                                    end
                            end;

                        {error, Reason} ->
                            logging ! {add, self(), error, io_lib:format("Problem trying Change FTP dir to: ~p~nError: ~p~n", [BaseDir, Reason])},
                            Pid ! ko
                    end;
                Error ->
                    io:format("Doing logging on FTP server with user: ~p~nError: ~p~n", [User, Error]),
                    Pid ! ko
            end,

            ftp:close(FtpPid);

        {error, Reason} ->
            io:format("Error opening connection with FTP server \"~w\" on port: \"~w\" ~p~n", [Host, Port, Reason]),
            Pid ! ko,
            false
    end.

listener(Host, Port, User, Pass, BaseDir) ->
    receive
        {Pid, del, Key} ->
            exec_action(del, Pid, Host, Port, User, Pass, BaseDir, Key, false);
        {Pid, get, Key} ->
            exec_action(get, Pid, Host, Port, User, Pass, BaseDir, Key, false);
        {Pid, save, Key, Value} ->
            exec_action(save, Pid, Host, Port, User, Pass, BaseDir, Key, Value)
    end,

    listener(Host, Port, User, Pass, BaseDir).

init(Config) ->
    Host = dict:fetch("ftp_host", Config),
    Port = list_to_integer(dict:fetch("ftp_port", Config)),
    User = dict:fetch("ftp_user", Config),
    Pass = dict:fetch("ftp_pass", Config),
    BaseDir = dict:fetch("ftp_dir", Config),

    register(
        warehouse,
        spawn_link(ftp_manager, listener, [Host, Port, User, Pass, BaseDir])),

    ok.
