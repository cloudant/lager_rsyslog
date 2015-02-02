lager_rsyslog
=============

This library application provides an [Rsyslog][rsyslog] backend for [Lager][lager]. It allows you to send messages from your Erlang application to a remote rsyslog daemon.

This backend is based on the [lager_syslog][lager_syslog] backend from Basho.

Configuration
-------------

  Configure a Lager handler like the following:

    {lager_rsyslog_backend, [
        {host, Hostname},       % The hostname of the rsyslog server
        {port, Port},           % The rsyslog port, defaults to 514
        {identity, Identity},   % A name for this application
        {facility, Facility},   % The rsyslog facility, defaults to local2
        {level, Level},         % The log level threshold
        {formatter, {           % A custom message formatter
            FormatterModule,    %   The formatter module name
            FormatterConfig     %   The formatter config
        }}           
    ]}

The Identity is the string to tag all messages with in syslog, usually the application's name. The facility is the facility to log to (see the rsyslog documentation for a list of these). The Level is the lager level at which the backend accepts messages (eg. using `info` would send all messages at info level or above into syslog). While you can filter messages at the syslog level, it will improve performance if you filter in lager instead.

You can refer to the [Lager documentation][lager_levels] for using a syslog style level definition for configuring the level thresholds which allows more flexibility than the simple threshold.

An example for Apache CouchDB might look something like this:

    {handlers, [
        {lager_rsyslog_backend, [
            {identity, "couchdb"},
            {facility, local2},
            {level, info}
        ]}
    ]}

Multiple `lager_rsyslog_backend` handlers can be specified but you *MUST* use
unique identity values for each handler. Refer to Lager's documentation for
further information on configuring handlers.

Configuring Rsyslog is left as an exercise for the reader.

[rsyslog]: http://www.rsyslog.com/
[lager]: https://github.com/basho/lager
[lager_syslog]: https://github.com/basho/lager_syslog
[lager_levels]: https://github.com/basho/lager#syslog-style-loglevel-comparison-flags
