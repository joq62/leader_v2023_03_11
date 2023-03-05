%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(leader).

-behaviour(gen_server).
%% Server API

-export([who_is_leader/0,
	 am_i_leader/0]).

%% API
-export([start/1,
	 start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-define(COORDINATOR_SERVER, ?MODULE).
-define(ELECTION_MESSAGE, election).
-define(ELECTION_RESPONSE, i_am_alive).
-define(COORDINATOR_MESSAGE, coordinator).
-define(ELECTION_RESPONSE_TIMEOUT, 3*50).

-record(state, {coordinatorNode,
		nodes,
		timeout_pid}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
who_is_leader()->
    gen_server:call(?SERVER, {who_is_leader},infinity).
am_i_leader()->
    gen_server:call(?SERVER, {am_i_leader},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start(Nodes)->
    start_link(Nodes).

-spec start_link(Nodes::[]) -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link(Nodes) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Nodes, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init(Nodes) ->
    ServerPid=self(),
    Pid=spawn(fun()->timeout_loop(ServerPid,infinity) end),
    start_election(Nodes),
    Pid!{self(),?ELECTION_RESPONSE_TIMEOUT},
    {ok, #state{timeout_pid=Pid,
		nodes=Nodes}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

handle_call({who_is_leader}, _From, State) ->
    Reply = State#state.coordinatorNode,
    {reply, Reply, State};

handle_call({am_i_leader}, _From, State) ->
    MyNode=node(),
    Reply = if 
		MyNode/=State#state.coordinatorNode ->
		    false;
		true->
		    true
	    end,
		
    {reply, Reply, State};


handle_call(Request, _From, State) ->
    io:format("Unmatched signal ~p~n",[{Request,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,Request]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast(Request, State) ->
    io:format("Unmatched signal ~p~n",[{Request,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({?ELECTION_RESPONSE,_Node}, State) ->
    State#state.timeout_pid!{self(),infinity},
    {noreply,State};

handle_info({?ELECTION_MESSAGE,Node}, State) ->
    if
	Node < node()->
	    send_election_response(Node), 
	    start_election(State#state.nodes),
	    State#state.timeout_pid!{self(),?ELECTION_RESPONSE_TIMEOUT};
	true->
	    ok
    end,
    {noreply,State};

handle_info({?COORDINATOR_MESSAGE,Node}, State) ->
    State#state.timeout_pid!{self(),infinity},
    Coordinator=set_coordinator(State, Node),   
    {noreply,State#state{coordinatorNode=Coordinator}};

handle_info({nodedown,Node}, State) ->
    if
	Node==State#state.coordinatorNode->
	    start_election(State#state.nodes),
	    State#state.timeout_pid!{self(),?ELECTION_RESPONSE_TIMEOUT};
	true->
	    ok
    end,
    {noreply,State};

handle_info({timeout}, State) ->
    State#state.timeout_pid!{self(),infinity},
    Coordinator=win_election(State),   
    {noreply,State#state{coordinatorNode=Coordinator}};


handle_info(Info, State) ->
    io:format("Unmatched signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
timeout_loop(ServerPid,TimeOut)->
    receive
	{ServerPid,NewTimeOut}->
	    ok
    after
	TimeOut->
	    NewTimeOut=infinity,
	    ServerPid!{timeout}
    end,
    timeout_loop(ServerPid,NewTimeOut).
       
	

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_election(Nodes) ->
    lists:foreach(fun send_election_message/1, nodes_with_higher_ids(Nodes)),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
win_election(State) ->
  io:format("Node ~s has declared itself a leader.~n", [atom_to_list(node())]),
  lists:foreach(fun send_coordinator_message/1, nodes_with_lower_ids(State#state.nodes)),
  set_coordinator(State, node()).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set_coordinator(State, Coordinator) ->
    io:format("Node ~p has changed leader from ~p to ~p~n", [node(), State#state.coordinatorNode, Coordinator]),
    monitor_node(State#state.coordinatorNode, false),
    monitor_node(Coordinator, true),
    Coordinator.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

send_election_message(Node) ->
  send_bully_message(Node, ?ELECTION_MESSAGE).

send_election_response(Node) ->
  send_bully_message(Node, ?ELECTION_RESPONSE).

send_coordinator_message(Node) ->
  send_bully_message(Node, ?COORDINATOR_MESSAGE).

send_bully_message(Node, BullyAlgorithmMessage) ->
  send_to_node(Node, {BullyAlgorithmMessage, node()}).

send_to_node(Node, Message) ->
  {?COORDINATOR_SERVER, Node} ! Message.

nodes_with_higher_ids(Nodes) ->
  [Node || Node <- Nodes, Node > node()].

nodes_with_lower_ids(Nodes) ->
  [Node || Node <- Nodes, Node < node()].
