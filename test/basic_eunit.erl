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
-module(basic_eunit).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    ok=setup(),
    Nodes=test_nodes:get_nodes(),
    ok=start_leader(Nodes,Nodes),
    timer:sleep(4000),
   % shutdown_ok=rpc:call(c_0@c100,leader_server,stop,[],1000),
   % timer:sleep(2000),
    rpc:cast(c_0@c100,init,stop,[]),
    
   
  %  init:stop(),
    ok.

start_leader([],_)->
    ok;
start_leader([Node|T],Nodes)->
    ok=rpc:call(Node,application,set_env,[[{leader,[{nodes,Nodes}]}]],5000),
    ok=rpc:call(Node,application,start,[leader],5000),
    start_leader(T,Nodes).
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

setup()->
  
    ok=test_nodes:start_nodes(),
    Nodes=test_nodes:get_nodes(),
%    ['controller_0@c100','controller_1@c100','controller_2@c100','controller_3@c100','controller_4@c100']=Nodes,
    ['c_0@c100','c_1@c100','c_2@c100','c_3@c100','c_4@c100','c_5@c100']=Nodes,
    % Starts config and leader on all nodes
    Dirs=["ebin"],
    Cookie=cookie_test,
    [{rpc:call(N,code,add_pathsa,[Dirs],5000),rpc:call(N,erlang,set_cookie,[Cookie],5000)}||N<-Nodes],
    io:format("nodes() ~p~n",[nodes()]),
    ok.
