%% Copyright (c) 2011-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc Rsyslog backend for lager.

-module(lager_rsyslog_backend).

-behaviour(gen_event).

-export([
    config_to_id/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    code_change/3
]).

-record(st, {
    id,
    hostname,
    socket,
    dest_addr,
    dest_port,
    mask,
    ident,
    facility,
    formatter
}).

-include("lager_rsyslog.hrl").

config_to_id(Config) ->
    Ident = lager_rsyslog_util:identity(Config),
    Facility = lager_rsyslog_util:facility(Config),
    {?MODULE, {Ident, Facility}}.


init(Config) when is_list(Config) ->
    {ok, Socket} = gen_udp:open(0),
    {ok, #st{
        id = config_to_id(Config),
        hostname = net_adm:localhost(),
        socket = Socket,
        dest_addr = lager_rsyslog_util:dest_addr(Config),
        dest_port = lager_rsyslog_util:dest_port(Config),
        mask = lager_rsyslog_util:mask(Config),
        ident = lager_rsyslog_util:identity(Config),
        facility = lager_rsyslog_util:facility(Config),
        formatter = lager_rsyslog_util:formatter(Config)
    }}.


terminate(_Reason, _St) ->
    ok.


handle_call(get_loglevel, St) ->
    {ok, St#st.mask, St};

handle_call({set_loglevel, Level}, St) ->
    Mask = lager_rsyslog_util:mask(Level),
    {ok, ok, St#st{mask = Mask}};

handle_call(_Request, St) ->
    {ok, ignored, St}.


handle_event({log, Message}, St) ->
    case lager_util:is_loggable(Message, St#st.mask, St#st.id) of
        true ->
            LagerLevel = lager_msg:severity(Message),
            RsyslogLevel = lager_rsyslog_util:level(LagerLevel),
            {FmtMod, FmtCfg} = St#st.formatter,
            MsgStr = [FmtMod:format(Message, FmtCfg)],
            send_log(St, RsyslogLevel, MsgStr),
            {ok, St};
        false ->
            {ok, St}
    end;

handle_event(_Event, St) ->
    {ok, St}.


handle_info(_Info, St) ->
    {ok, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


send_log(St, Level, Msg) ->
    Pre = io_lib:format("<~B>~B ~s ~s ~s ", [
        St#st.facility bor Level,
        ?SYSLOG_VERSION,
        lager_rsyslog_util:iso8601_timestamp(),
        St#st.hostname,
        St#st.ident
    ]),
    gen_udp:send(St#st.socket, St#st.dest_addr, St#st.dest_port, [Pre, Msg]).
