%%
%% This module manages the string data type, handles the get / set and delete actions
%% And controls the persistance of this data
%%

-module(ftp_manager).

-author('alonso.vidales@tras2.es').

-export([
    init/1,
    listener/1]).

listener(FtpPid) ->
    receive
        {Pid, del, Key} ->
            case ftp:delete(FtpPid, Key) of
                ok ->
                    Pid ! ok;
                {error, _Reason} ->
                    Pid ! ko
            end;
            
        {Pid, get, Key} ->
            case ftp:recv_bin(FtpPid, Key) of
                {ok, Content} ->
                    Pid ! {ok, Content};
                {error, _Reason} ->
                    Pid ! ko
            end;
        {Pid, save, Key, Value} ->
            logging ! {add, self(), info, io_lib:format("Save on warhouse: ~p - ~p~n", [Key, Value])},
            case ftp:send_bin(FtpPid, binary:list_to_bin(Value), Key) of
                ok ->
                    Pid ! ok;
                {error, Reason} ->
                    logging ! {add, self(), error, io_lib:format("Trying to store Key: ~p Error: ~p~n", [Key, Reason])},
                    Pid ! ko
            end
    end,

    listener(FtpPid).

init(Config) ->
    Host = dict:fetch("ftp_host", Config),
    Port = list_to_integer(dict:fetch("ftp_port", Config)),
    User = dict:fetch("ftp_user", Config),
    Pass = dict:fetch("ftp_pass", Config),
    BaseDir = dict:fetch("ftp_dir", Config),

    case ftp:open(Host, [{port, Port}]) of
        {ok, FtpPid} ->
            case ftp:user(FtpPid, User, Pass) of
                ok ->
                    case ftp:cd(FtpPid, BaseDir) of
                        ok ->
                            logging ! {add, self(), info, io_lib:format("Connected to warehouse~n", [])},
                            register(
                                warehouse,
                                spawn_link(ftp_manager, listener, [FtpPid])),
                            ok;
                        {error, Reason} ->
                            logging ! {add, self(), error, io_lib:format("Problem trying Change FTP dir to: ~p~nError: ~p~n", [BaseDir, Reason])},
                            ko
                    end;
                Error ->
                    io:format("Doing logging on FTP server with user: ~p~nError: ~p~n", [User, Error])
            end;

        {error, Reason} ->
            io:format("Error opening connection with FTP server \"~w\" on port: \"~w\" ~p~n", [Host, Port, Reason]),
            false
    end.
