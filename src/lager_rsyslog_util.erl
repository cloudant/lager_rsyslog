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

-module(lager_rsyslog_util).

-export([
    dest_addr/1,
    dest_port/1,
    identity/1,
    facility/1,
    mask/1,
    level/1,
    formatter/1,
    iso8601_timestamp/0
]).


-include("lager_rsyslog.hrl").


dest_addr(Config) ->
    case lists:keyfind(host, 1, Config) of
        {host, Host} ->
            case inet:getaddr(Host, inet) of
                {ok, Address} ->
                    Address;
                _ ->
                   {127, 0, 0, 1} 
            end;
        false ->
            {127, 0, 0, 1}
    end.


dest_port(Config) ->
    case lists:keyfind(port, 1, Config) of
        {port, P} when is_integer(P), P > 0, P < 65536 ->
            P;
        _ ->
            514
    end.


identity(Config) ->
    case lists:keyfind(identity, 1, Config) of
        {identity, Ident} when is_atom(Ident) orelse is_list(Ident) ->
            Ident;
        false ->
            hd(string:tokens(atom_to_list(node()), "@"))
    end.


facility(Config) ->
    case lists:keyfind(facility, 1, Config) of
        {facility, Facility} ->
            facility_int(Facility);
        false ->
            facility_int(local2)
    end.


facility_int(kern)     -> (0 bsl 3);
facility_int(user)     -> (1 bsl 3);
facility_int(mail)     -> (2 bsl 3);
facility_int(daemon)   -> (3 bsl 3);
facility_int(auth)     -> (4 bsl 3);
facility_int(syslog)   -> (5 bsl 3);
facility_int(lpr)      -> (6 bsl 3);
facility_int(news)     -> (7 bsl 3);
facility_int(uucp)     -> (8 bsl 3);
facility_int(cron)     -> (9 bsl 3);
facility_int(authpriv) -> (10 bsl 3);
facility_int(ftp)      -> (11 bsl 3);
facility_int(local0)   -> (16 bsl 3);
facility_int(local1)   -> (17 bsl 3);
facility_int(local2)   -> (18 bsl 3);
facility_int(local3)   -> (19 bsl 3);
facility_int(local4)   -> (20 bsl 3);
facility_int(local5)   -> (21 bsl 3);
facility_int(local6)   -> (22 bsl 3);
facility_int(local7)   -> (23 bsl 3);

facility_int(Facility) when is_list(Facility) ->
    facility_int(list_to_existing_atom(Facility));
facility_int(Facility) when is_binary(Facility) ->
    facility_int(list_to_existing_atom(binary_to_list(Facility))).


mask(Config) ->
    case lists:keyfind(level, 1, Config) of
        {level, Level} ->
            try
                lager_util:config_to_mask(Level)
            catch _:_ ->
                lager_util:config_to_mask(info)
            end;
        false ->
            lager_util:config_to_mask(info)
    end.


level(debug) ->     7;
level(info) ->      6;
level(notice) ->    5;
level(warn) ->      4;
level(warning) ->   4;
level(err) ->       3;
level(error) ->     3;
level(crit) ->      2;
level(alert) ->     1;
level(emerg) ->     0;
level(panic) ->     0;

level(I) when is_integer(I), I >= 0, I =< 7 ->
    I;
level(_BadLevel) ->
    3.


formatter(Config) ->
    case lists:keyfind(formatter, 1, Config) of
        {formatter, {Mod, Config}} when is_atom(Mod) ->
            {Mod, Config};
        false ->
            ?DEFAULT_FORMATTER
    end.


iso8601_timestamp() ->
    {_,_,Micro} = Now = os:timestamp(),
    {{Year,Month,Date},{Hour,Minute,Second}} = calendar:now_to_datetime(Now),
    Format = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
    io_lib:format(Format, [Year, Month, Date, Hour, Minute, Second, Micro]).
