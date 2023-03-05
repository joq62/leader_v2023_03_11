%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).      
    
 
-export([start/1
	]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start([_ClusterSpec,_HostSpec])->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=setup(),
    AllNodes=test_nodes:get_nodes(),
    ok=test_1(AllNodes),
    
    
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    timer:sleep(2000),
  % init:stop(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test_1(Nodes)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    [N1,N2,N3,N4]=Nodes,
    %% N1
    true=rpc:cast(N1,leader,start,[Nodes]),
    timer:sleep(500),
    N1=rpc:call(N1,leader,who_is_leader,[],2000),
    true=rpc:call(N1,leader,am_i_leader,[],2000),
  %  io:format("Leader ~p~n",[{rpc:call(N1,leader,who_is_leader,[],2000),?MODULE,?FUNCTION_NAME,?LINE}]),
    
    %% N2
    true=rpc:cast(N2,leader,start,[Nodes]),
    timer:sleep(500),
    N2=rpc:call(N1,leader,who_is_leader,[],2000),
    false=rpc:call(N1,leader,am_i_leader,[],2000),
    true=rpc:call(N2,leader,am_i_leader,[],2000),
    
  %  io:format("Leader ~p~n",[{rpc:call(N1,leader,who_is_leader,[],2000),?MODULE,?FUNCTION_NAME,?LINE}]),

    %% N3
    true=rpc:cast(N3,leader,start,[Nodes]),
    timer:sleep(500),
    N3=rpc:call(N1,leader,who_is_leader,[],2000),
    false=rpc:call(N1,leader,am_i_leader,[],2000),
    false=rpc:call(N2,leader,am_i_leader,[],2000),
    true=rpc:call(N3,leader,am_i_leader,[],2000),

%    io:format("Leader ~p~n",[{rpc:call(N1,leader,who_is_leader,[],2000),?MODULE,?FUNCTION_NAME,?LINE}]),


    %% N4
    true=rpc:cast(N4,leader,start,[Nodes]),
    timer:sleep(500),
    N4=rpc:call(N1,leader,who_is_leader,[],2000),
    false=rpc:call(N1,leader,am_i_leader,[],2000),
    false=rpc:call(N2,leader,am_i_leader,[],2000),
    false=rpc:call(N3,leader,am_i_leader,[],2000),
    true=rpc:call(N4,leader,am_i_leader,[],2000),
   % io:format("Leader ~p~n",[{rpc:call(N1,leader,who_is_leader,[],2000),?MODULE,?FUNCTION_NAME,?LINE}]),

    %% Kill N4 and a new elections starts
    rpc:call(N4,init,stop,[],2000),
    timer:sleep(2000),
    N3=rpc:call(N1,leader,who_is_leader,[],2000),

    %io:format("Leader ~p~n",[{rpc:call(N1,leader,who_is_leader,[],2000),?MODULE,?FUNCTION_NAME,?LINE}]),

    %% restart N4
    {ok,N4}=test_nodes:start_slave("c4","-pa ebin"),
    true=rpc:cast(N4,leader,start,[Nodes]),
    timer:sleep(500),
    N4=rpc:call(N1,leader,who_is_leader,[],2000),
  %  io:format("Leader ~p~n",[{rpc:call(N1,leader,who_is_leader,[],2000),?MODULE,?FUNCTION_NAME,?LINE}]),

    ok.


%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok=test_nodes:start_nodes(),
    io:format("nodes ~p~n",[{nodes(),?MODULE,?LINE,?FUNCTION_NAME}]),
    
    ok.
