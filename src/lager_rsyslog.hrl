
%% Copied from lager.hrl

-define(SYSLOG_VERSION, 1).

-define(DEFAULT_FORMATTER, {lager_default_formatter, [
    "[", severity, "] ",
    {pid, ""},
    {module, [
        {pid, ["@"], ""},
        module,
        {function, [":", function], ""},
        {line, [":", line], ""}
    ], ""},
    " ",
    message
]}).