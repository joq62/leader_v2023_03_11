%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created :  8 Mar 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(toggle).

-behaviour(gen_statem).

%% API
-export([start/0,
	 stop/0]).

%% Common Events for all States
-export([
	 get_count/0,
	 ping/0
	]).

%% Events
-export([push/0
	]).


%% States
-export([
	 off/3,
	 on/3
	]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([state_name/3]).

-define(SERVER, ?MODULE).

-record(data, {count}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
start()->
    start_link().

stop() ->
    gen_statem:stop(?SERVER).

-spec start_link() ->
	  {ok, Pid :: pid()} |
	  ignore |
	  {error, Error :: term()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).


push()->
    gen_statem:call(?SERVER, {push}).

get_count() ->
    gen_statem:call(?SERVER,{get_count}).

ping() ->
    {ping,?MODULE,?LINE}.
%    gen_statem:call(?SERVER,{ping}).



%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
	  gen_statem:init_result(atom()).

init([]) ->
    io:format(" ~p~n",[{node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    NextState=off,
    Count=0,
    io:format("Count,NextState ~p~n",[{Count,NextState,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok, NextState, #data{count=Count}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function 
%% with the name of the current state (StateName) 
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------

-spec state_name('enter',
		 OldState :: atom(),
		 Data :: term()) ->
	  gen_statem:state_enter_result('state_name');
		(gen_statem:event_type(),
		 Msg :: term(),
		 Data :: term()) ->
	  gen_statem:event_handler_result(atom()).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
off({call,From},{push}, Data) ->
    NewData=Data#data{count=Data#data.count+1},
    {next_state,on,NewData,[{reply,From,on}]};
off(EventType, EventContent, Data) ->
  %  io:format("EventType  ~p~n",[{EventType,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
  %  io:format("EventContent  ~p~n",[{EventContent,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    handle_event(EventType, EventContent, Data).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
on({call,From},{push}, Data) ->
    NewData=Data#data{count=Data#data.count+1},
    {next_state,off,NewData,[{reply,From,off}]};
%on({call,From}, {get_count}, Data) ->
%    {keep_state,Data,[{reply,From,Data#data.count}]};
on(EventType, EventContent, Data) ->
   % io:format("EventType  ~p~n",[{EventType,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
   % io:format("EventContent  ~p~n",[{EventContent,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    handle_event(EventType, EventContent, Data).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

state_name({call,Caller}, Msg, Data) ->
    io:format("unmatched signal  ~p~n",[{Caller, Msg,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    {next_state, state_name, Data, [{reply,Caller,ok}]}.


%% Handle events common to all states

handle_event({call,From},{ping}, Data) ->
    Reply=pong,
    {keep_state,Data,[{reply,From,Reply}]};

handle_event({call,From},{get_count}, Data) ->
    Reply=Data#data.count,
    {keep_state,Data,[{reply,From,Reply}]};

handle_event(From, UnMatchedSignal, Data) ->
    io:format("unmatched signal  ~p~n",[{From, UnMatchedSignal,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("From  ~p~n",[{From,?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("UnMatchedSignal  ~p~n",[{UnMatchedSignal,?MODULE,?FUNCTION_NAME,?LINE}]),
    {keep_state,Data}.





%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
	  any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
	OldVsn :: term() | {down,term()},
	State :: term(), Data :: term(), Extra :: term()) ->
	  {ok, NewState :: term(), NewData :: term()} |
	  (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
