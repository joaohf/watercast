%% @doc Icecast config generator.
%%
%% This modules produces a valid icecast config.
%%
%% Based on icecast XML configuration file:
%%
%% https://www.icecast.org/docs/icecast-trunk/config_file/
%% https://github.com/xiph/Icecast-Server/blob/master/conf/icecast.xml.in
%%
%% @end
-module(watercast_config).

-export([write_config/2, make_config_xml/1]).

-record(authentication, {
    'source-password' = "hackme",
    'relay-password' = "hackme",
    'admin-user' = "user",
    'admin-password' = "hackme"
}).

-record('listen-socket', {
    port = "8000"
}).

-record(logging, {
    accesslog = "access.log",
    errorlog = "error.log",
    loglevel = "information",
    logsize = "10000",
    logarchive = "true"
}).

-record(paths, {
    basedir = "/usr/local/share/icecast",
    logdir = "/var/log/icecast",
    webroot = "/usr/local/share/icecast/web",
    adminroot = "/usr/local/share/icecast/admin",
    reportxmldb = "/usr/local/share/icecast/report-db.xml"
}).

-record(security, {
    chroot = "false"
}).

-record(icecast, {
    'listen-sockets' = #'listen-socket'{},
    authentication = #authentication{},
    paths = #paths{},
    logging = #logging{},
    security = #security{}
}).

write_config(Filename, Xml) ->
    ok = file:write_file(Filename, Xml).

make_config_xml(default) ->
    I = icecast_default(),
    Xml = xmerl:export_simple([icecast_xml_simple(I)], xmerl_xml, [{prolog, ""}]),
    unicode:characters_to_binary(Xml).

icecast_xml_simple(I) ->
    Content = [
        {location, ["watercast"]},
        {admin, ["icemaster@localhost"]},
        {hostname, ["localhost"]},
        icecast_authentication(I),
        icecast_limits(),
        'icecast_listen-socket'(I),
        icecast_paths(I),
        icecast_logging(I),
        icecast_security(I)
    ],
    {icecast, Content}.

icecast_default() ->
    #icecast{
        'listen-sockets' = #'listen-socket'{},
        paths = make_default_paths(),
        logging = #logging{},
        security = #security{}
    }.

make_default_paths() ->
    PrivDir = code:priv_dir(watercast),
    BaseDir = filename:join([PrivDir, "icecast"]),
    WebRoot = filename:join([BaseDir, "share", "icecast", "web"]),
    AdminRoot = filename:join([BaseDir, "share", "icecast", "admin"]),
    Reportxmldb = filename:join([BaseDir, "share", "icecast", "report-db.xml"]),

    % TODO add based on priv dir, but should be a tmp dir
    Logdir = "/tmp",

    #paths{
        basedir = BaseDir,
        webroot = WebRoot,
        adminroot = AdminRoot,
        reportxmldb = Reportxmldb,
        logdir = Logdir
    }.

icecast_authentication(I) ->
    Au = I#icecast.authentication,
    {authentication, [
        {'source-password', [Au#authentication.'source-password']},
        {'relay-password', [Au#authentication.'relay-password']},
        {'admin-user', [Au#authentication.'admin-user']},
        {'admin-password', [Au#authentication.'admin-password']}
    ]}.

icecast_limits() ->
    {limits, [
        {clients, ["100"]},
        {sources, ["2"]},
        {'queue-size', ["524288"]},
        {'client-timeout', ["30"]},
        {'header-timeout', ["15"]},
        {'source-timeout', ["10"]},
        {'burst-size', ["65535"]}
    ]}.

'icecast_listen-socket'(I) ->
    Ls = I#icecast.'listen-sockets',
    {'listen-socket', [
        {port, [Ls#'listen-socket'.port]}
    ]}.

icecast_paths(I) ->
    Pa = I#icecast.paths,
    {paths, [
        {basedir, [Pa#paths.basedir]},
        {logdir, [Pa#paths.logdir]},
        {webroot, [Pa#paths.webroot]},
        {adminroot, [Pa#paths.adminroot]},
        {reportxmldb, [Pa#paths.reportxmldb]}
    ]}.

icecast_logging(I) ->
    Lo = I#icecast.logging,
    {logging, [
        {accesslog, [Lo#logging.accesslog]},
        {errorlog, [Lo#logging.errorlog]},
        {loglevel, [Lo#logging.loglevel]},
        {logsize, [Lo#logging.logsize]},
        {logarchive, [Lo#logging.logarchive]}
    ]}.

icecast_security(I) ->
    Sc = I#icecast.security,
    {security, [
        {chroot, [Sc#security.chroot]}
    ]}.
