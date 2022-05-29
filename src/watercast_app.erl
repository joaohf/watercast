%%%-------------------------------------------------------------------
%% @doc watercast public API
%% @end
%%%-------------------------------------------------------------------

-module(watercast_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Filename = make_icecast_config_filename(),
    Xml = watercast_config:make_config_xml(default),
    watercast_config:write_config(Filename, Xml),

    watercast_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

make_icecast_config_filename() ->
    filename:join([code:priv_dir(watercast), "icecast", "etc", "watercast.xml"]).
