-module(watercast).
-export([start/0, stop/0, init/2]).
-export([shutdown/0, config_reread/0]).

start() ->
    PrivDir = code:priv_dir(watercast),
    ConfigFile = filename:join([PrivDir, "icecast", "etc", "watercast.xml"]),
    Args = ["-c", ConfigFile],
    ExtPrg = filename:join([PrivDir, "icecast", "bin", "icecast"]),
    spawn(?MODULE, init, [ExtPrg, Args]).
stop() ->
    complex ! stop.

shutdown() ->
    call_port(shutdown).
config_reread() ->
    call_port(config_reread).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
        {complex, Result} ->
            Result
    end.

init(ExtPrg, Args) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Default = [{packet, 2}],
    PortSettings = lists:merge(Default, [{args, Args}]),
    Port = open_port({spawn_executable, ExtPrg}, PortSettings),
    loop(Port).

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {complex, decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, _Reason} ->
            exit(port_terminated)
    end.

encode(shutdown) -> [1, 0];
encode(config_reread) -> [2, 0].

decode([0]) -> ok.
