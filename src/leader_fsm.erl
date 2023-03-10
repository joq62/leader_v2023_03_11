%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created :  8 Mar 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(leader_fsm).

-behaviour(gen_statem).

%% API
-export([start/1]).

%% Common Events for all States
-export([
	 am_i_leader/1,
	 who_is_leader/0,
	 ping/0,
	 debug/0
	]).

%% Events
-export([start_election/0,
	 victory/1,
	 i_am_alive/1
	]).


%% States
-export([candidate/3,
	 leader/3,
	 election/3,
	 wait_for_victory/3]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([state_name/3]).

-define(SERVER, ?MODULE).
-define(ELECTION_TIME,200).

-record(data, {leader,
	       nodes}).

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
start(Nodes)->
    start_link(Nodes).

-spec start_link(term()) ->
	  {ok, Pid :: pid()} |
	  ignore |
	  {error, Error :: term()}.
start_link(Nodes) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, Nodes, []).


start_election()->
    gen_statem:cast(?SERVER, {start_election}).

victory(Node)->
    gen_statem:cast(?SERVER, {victory,Node}).

i_am_alive(Node)->
    gen_statem:cast(?SERVER, {i_am_alive,Node}).

	 
am_i_leader(Node) ->
    gen_statem:call(?SERVER,{am_i_leader,Node}).
who_is_leader() ->
    gen_statem:call(?SERVER,{who_is_leader}).

ping() ->
    gen_statem:call(?SERVER,{ping}).

debug() ->
    gen_statem:call(?SERVER,{debug}).



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

init(Nodes) ->
  %  io:format(" ~p~n",[{Nodes,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    case nodes_with_higher_ids(Nodes) of
	[]->
	    send_victory_msg(Nodes),
	    NewLeader=node(),
	    NextState=leader;
	_->
	    send_election_msg(Nodes),
	    NewLeader=undefined,
	    NextState=election
    end,
  %  io:format("NewLeader,NextState ~p~n",[{NewLeader,NextState,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok, NextState, #data{leader=NewLeader,
			  nodes=Nodes}}.

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
leader(cast,{victory,NewLeader},Data)->
    io:format(" ~p~n",[{victory,NewLeader,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    {next_state, candidate, Data#data{leader=NewLeader}};
leader(cast,{start_election},Data)->
    io:format(" ~p~n",[{start_election,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    send_i_am_alive_msg(Data#data.nodes),
    send_election_msg(Data#data.nodes),
    {next_state, election, Data,
     [{state_timeout,?ELECTION_TIME,start_election}]};

leader(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
election(cast,{i_am_alive,Node},Data)->
    io:format(" ~p~n",[{i_am_alive,Node,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    {next_state,wait_for_victory, Data};
election(state_timeout,start_election,Data)->
    io:format(" ~p~n",[{state_timeout,start_election,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    send_victory_msg(Data#data.nodes),
    {next_state,leader, Data#data{leader=node()}};

election(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
wait_for_victory(cast,{victory,NewLeader},Data)->
    io:format(" ~p~n",[{victory,NewLeader,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    {next_state, candidate, Data#data{leader=NewLeader}};
wait_for_victory(state_timeout,start_election,Data)->
    io:format(" ~p~n",[{state_timeout,start_election,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    send_election_msg(Data#data.nodes),
    {next_state, election, Data,
     [{state_timeout,?ELECTION_TIME,start_election}]};
wait_for_victory(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
candidate(cast,{victory,NewLeader},Data)->
    io:format(" ~p~n",[{victory,NewLeader,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    {next_state, candidate, Data#data{leader=NewLeader}};
candidate(cast,{start_election},Data)->
    io:format(" ~p~n",[{start_election,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    send_i_am_alive_msg(Data#data.nodes),
    send_election_msg(Data#data.nodes),
    {next_state, election, Data,
     [{state_timeout,?ELECTION_TIME,start_election}]};
candidate(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).



state_name({call,Caller}, _Msg, Data) ->
    {next_state, state_name, Data, [{reply,Caller,ok}]}.


%% Handle events common to all states

handle_event({call,From},{ping}, Data) ->
    Reply=pong,
    {keep_state,Data,[{reply,From,Reply}]};

handle_event({call,From},{am_i_leader,Node}, Data) ->
    Reply=case Node==Data#data.leader of
	      true->
		  true;
	      false->
		  false
	  end,
    {keep_state,Data,[{reply,From,Reply}]};

handle_event({call,From},{who_is_leader}, Data) ->
    Reply=Data#data.leader,
    {keep_state,Data,[{reply,From,Reply}]};

handle_event(From, UnMatchedSignal, Data) ->
    io:format("unmatched signal  ~p~n",[{From, UnMatchedSignal,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
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
nodes_with_higher_ids(Nodes) ->
  [Node || Node <- Nodes, Node > node()].

nodes_with_lower_ids(Nodes) ->
  [Node || Node <- Nodes, Node < node()].

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
send_i_am_alive_msg(Nodes)->
    R=[{Node,rpc:cast(Node,?MODULE,i_am_alive,[node()])}||Node<-nodes_with_lower_ids(Nodes)],
    io:format(" ~p~n",[{R,node(),?MODULE,?FUNCTION_NAME,?LINE}]).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
 send_election_msg(Nodes)->
    HigherNodes=nodes_with_higher_ids(Nodes),
    io:format("HigherNodes ~p~n",[{HigherNodes,node(),?MODULE,?FUNCTION_NAME,?LINE}]),
    R=[{Node,rpc:cast(Node,?MODULE,start_election,[])}||Node<-HigherNodes],
    io:format(" ~p~n",[{R,node(),?MODULE,?FUNCTION_NAME,?LINE}]).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
send_victory_msg(Nodes)->
    R=[{Node,rpc:cast(Node,?MODULE,victory,[node()])}||Node<-nodes_with_lower_ids(Nodes)],
    io:format(" ~p~n",[{R,node(),?MODULE,?FUNCTION_NAME,?LINE}]).


