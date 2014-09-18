lager_rsyslog
=============

This library application provides an rsyslog backend for [Lager][lager]. It allows you to send messages from your Erlang application to a remote rsyslog daemon.

This backend is based on the [lager_syslog][lager_syslog] backend from Basho.

Configuration
-------------

  Configure a Lager handler like the following:

    {lager_rsyslog_backend, [
        {identity, Identity},
        {facility, Facility},
        {level, Level},
        {formatter, }
    ]}

The Identity is the string to tag all messages with in syslog, usually the application's name. The facility is the facility to log to (see the syslog documentation for a list of these). The Level is the lager level at which the backend accepts messages (eg. using 'info' would send all messages at info level or above into syslog). While you can filter messages at the syslog level, it will improve performance if you filter in lager instead.

An example for Apache CouchDB might look something like this:

    {handles, [
        {lager_rsyslog_backend, [
            {identity, "couchdb"},
            {facility, local2},
            {level, info}
        ]}
    ]}

Multiple lager_rsyslog_backend handlers can be specified but you *MUST* use
unique identity values for each handler. Refer to Lager's documentation for
further information on configuring handlers.

Configuring Rsyslog is left as an exercise for the reader.

[lager]: https://github.com/basho/lager
[lager_syslog]: https://github.com/basho/lager_syslog
