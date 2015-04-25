%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2015, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 17 Apr 2015 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(lager_rsyslog).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export(
   [
    init/1, handle_event/2, handle_call/2, handle_info/2,
    terminate/2, code_change/3
   ]
  ).

-define(SERVER, ?MODULE).

-define(DEFAULT_SYSLOG_FACILITY, daemon).
-define(DEFAULT_SYSLOG_LEVEL, info).
-define(DEFAULT_SYSLOG_PORT, 514).
-define(DEFAULT_SYSLOG_HOST, "localhost").
-define(DEFAULT_FORMATTER, lager_default_formatter).
-define(DEFAULT_FORMAT,
        ["[", severity, "] ",
         {pid, ""},
         {module,
          [
           {pid, ["@"], ""},
           module,
           {function, [":", function], ""},
           {line, [":",line], ""}
          ], ""},
         " ", message]).

-record(s, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(Options) ->
    Ident = get_option(ident, Options),
    Facility = get_option(facility, Options, ?DEFAULT_SYSLOG_FACILITY),
    Level = get_option(level, Option, ?DEFAULT_SYSLOG_LEVEL),
    SysLogHost = get_option(syslog_host, Options, ?DEFAULT_SYSLOG_HOST),
    SysLogPort = get_option(syslog_port, Options, ?DEFAULT_SYSLOG_PORT),
    Formatter = get_option(syslog_formatter, Option, ?DEFAULT_FORMATTER),
    Format = get_option(syslog_format, Options, ?DEFAULT_FORMAT),
    init(Ident, Facility, Level, SysLogHost, SysLogPort, Formatter, Format).

init(Ident, Facility, Level, SysLogHost, SysLogPort, Formatter, Format)
  when is_list(Ident) ->
    SysLogAddr = {syslog_addr(SysLogHost), syslog_port(SysLogPort)},
    ID = log_event_id([Ident, Facility]),
    {ok, Sock} = gen_udp:open(0),
    {ok, #s{
            addr = SysLogAddr,
            id = ID,
            socket = Sock,
            level = Level
           }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(
  {log, Message},
  State #s{
    level = Level,
    id = ID,
    socket = Sock,
    addr = {SysLogAddr, SysLogPort}
   }) ->
    case lager_util:is_loggable(Message, Level, ID) of
        true ->
            _ = gen_udp:send(Sock, SysLogAddr, SysLogPort, ["<test>: ", Message]),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_option(Option, Options) ->
    get_option(Option, Options, undefined).

get_option(Option, Options, Default) ->
    proplists:get_value(Option, Options, Default).

syslog_addr(Host) ->
    {ok, InetAddr} = inet:getaddr(Host, inet),
    InetAddr.

syslog_port(Port) when is_integer(Port) andalso Port > 0 andalso Port < 65536 -> Port.

log_event_id([Ident, Facility]) -> {?MODULE, {Ident, Facility}}.
