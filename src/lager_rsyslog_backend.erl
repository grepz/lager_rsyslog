%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2015, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 17 Apr 2015 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(lager_rsyslog_backend).

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

-define(SYSLOG_VERSION, 1).

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

-record(s, {addr, id, ident, socket, level, format, facility}).

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
    Level = get_option(level, Options, ?DEFAULT_SYSLOG_LEVEL),
    SysLogHost = get_option(host, Options, ?DEFAULT_SYSLOG_HOST),
    SysLogPort = get_option(port, Options, ?DEFAULT_SYSLOG_PORT),
    Formatter = get_option(formatter, Options, ?DEFAULT_FORMATTER),
    Format = get_option(format, Options, ?DEFAULT_FORMAT),
    init(Ident, Facility, Level, SysLogHost, SysLogPort, Formatter, Format).

init(Ident, Facility, Level, SysLogHost, SysLogPort, Formatter, Format)
  when is_list(Ident) ->
    SysLogAddr = {syslog_addr(SysLogHost), syslog_port(SysLogPort)},
    SysLogFacility = syslog:facility(Facility),
    {ok, LagerLevel} = lager_log_level(Level),
    MessageFormat = {Formatter, Format},
    ID = log_event_id([Ident, Facility]),
    {ok, Sock} = gen_udp:open(0),
    {ok, #s{
            addr = SysLogAddr,
            id = ID,
            ident = Ident,
            format = MessageFormat,
            facility = SysLogFacility,
            socket = Sock,
            level = LagerLevel
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
  State = #s{
             level = Level,
             id = ID,
             ident = Ident,
             socket = Sock,
             addr = {SysLogAddr, SysLogPort},
             facility = Facility,
             format = {Formatter, Format}
            }
 ) ->
    case lager_util:is_loggable(Message, Level, ID) of
        true ->
            Severity = Facility + priority(lager_msg:severity(Message)),
            Header = syslog_msg_header(Severity, Ident),
            _ = gen_udp:send(
                  Sock, SysLogAddr, SysLogPort,
                  [Header, Formatter:format(Message, Format)]
                 ),
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
handle_call(get_loglevel, #s{level = Level} = State) -> {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    case lager_log_level(Level) of
        {ok, LagerLevel} -> {ok, ok, State#s{level = LagerLevel}};
        {error, Error} -> {ok, Error, State}
    end;
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

lager_log_level(Level) ->
    try lager_util:config_to_mask(Level) of
        Res -> {ok, Res}
    catch
        error:undef -> {ok, lager_util:level_to_num(Level)};
        _:_ -> {error, bad_log_level}
    end.

priority(emergency) -> 0;
priority(alert)     -> 1;
priority(critical)  -> 2;
priority(error)     -> 3;
priority(warning)   -> 4;
priority(notice)    -> 5;
priority(info)      -> 6;
priority(debug)     -> 7;
priority(N) when is_integer(N), N >= 0 -> N;
priority(_) -> erlang:error(badarg).


%% The syslog message has the following ABNF [RFC5234] definition:

%%    SYSLOG-MSG      = HEADER SP STRUCTURED-DATA [SP MSG]

%%    HEADER          = PRI VERSION SP TIMESTAMP SP HOSTNAME
%%                      SP APP-NAME SP PROCID SP MSGID
%%    PRI             = "<" PRIVAL ">"
%%    PRIVAL          = 1*3DIGIT ; range 0 .. 191
%%    VERSION         = NONZERO-DIGIT 0*2DIGIT
%%    HOSTNAME        = NILVALUE / 1*255PRINTUSASCII

%%    APP-NAME        = NILVALUE / 1*48PRINTUSASCII
%%    PROCID          = NILVALUE / 1*128PRINTUSASCII
%%    MSGID           = NILVALUE / 1*32PRINTUSASCII

%%    TIMESTAMP       = NILVALUE / FULL-DATE "T" FULL-TIME
%%    FULL-DATE       = DATE-FULLYEAR "-" DATE-MONTH "-" DATE-MDAY
%%    DATE-FULLYEAR   = 4DIGIT
%%    DATE-MONTH      = 2DIGIT  ; 01-12
%%    DATE-MDAY       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on
%%                              ; month/year
%%    FULL-TIME       = PARTIAL-TIME TIME-OFFSET
%%    PARTIAL-TIME    = TIME-HOUR ":" TIME-MINUTE ":" TIME-SECOND
%%                      [TIME-SECFRAC]
%%    TIME-HOUR       = 2DIGIT  ; 00-23
%%    TIME-MINUTE     = 2DIGIT  ; 00-59
%%    TIME-SECOND     = 2DIGIT  ; 00-59
%%    TIME-SECFRAC    = "." 1*6DIGIT
%%    TIME-OFFSET     = "Z" / TIME-NUMOFFSET
%%    TIME-NUMOFFSET  = ("+" / "-") TIME-HOUR ":" TIME-MINUTE


%%    STRUCTURED-DATA = NILVALUE / 1*SD-ELEMENT
%%    SD-ELEMENT      = "[" SD-ID *(SP SD-PARAM) "]"
%%    SD-PARAM        = PARAM-NAME "=" %d34 PARAM-VALUE %d34
%%    SD-ID           = SD-NAME
%%    PARAM-NAME      = SD-NAME
%%    PARAM-VALUE     = UTF-8-STRING ; characters '"', '\' and
%%                                   ; ']' MUST be escaped.
%%    SD-NAME         = 1*32PRINTUSASCII
%%                      ; except '=', SP, ']', %d34 (")

%%    MSG             = MSG-ANY / MSG-UTF8
%%    MSG-ANY         = *OCTET ; not starting with BOM
%%    MSG-UTF8        = BOM UTF-8-STRING
%%    BOM             = %xEF.BB.BF

%%    UTF-8-STRING    = *OCTET ; UTF-8 string as specified
%%                   ; in RFC 3629

%%    OCTET           = %d00-255
%%    SP              = %d32
%%    PRINTUSASCII    = %d33-126
%%    NONZERO-DIGIT   = %d49-57
%%    DIGIT           = %d48 / NONZERO-DIGIT
%%    NILVALUE        = "-"

syslog_msg_header(Facility, Ident) ->
    io_lib:format(
      "<~B>~B ~s ~s ~s - - -",
      [Facility, ?SYSLOG_VERSION, timestamp_str(), net_adm:localhost(), Ident]
     ).

timestamp_str() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:now_to_universal_time(erlang:now()),
    io_lib:format(
      "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
      [Year, Month, Day, Hour, Min, Sec]
     ).
