all:
	rm -rf  *~ */*~  src/*.beam test/*.beam erl_cra*;
	rm -rf *_info_specs deployments;
	rm -rf _build test_ebin ebin;		
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin logs;
	echo Done
check:
	rebar3 check
eunit:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs;
	rm -rf deployments host_info_specs;
	rm -rf rebar.lock;
	rm -rf ebin;
#	host_info_specs dir and deployments dir shall be installed once
	rm -rf  *~ */*~ apps/k3/src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir
	rm -rf deployments *_info_specs;
	rm -rf rebar.lock;
	rm -rf ebin;
#	host_info_specs dir and deployments dir shall be installed once
	mkdir  host_info_specs;
	cp ../../specifications/host_info_specs/*.host host_info_specs;
	git clone https://github.com/joq62/deployments.git;
	git clone https://github.com/joq62/application_info_specs.git;
	git clone https://github.com/joq62/deployment_info_specs.git;
#	Delete and create new cluster dir to make a clean start
	mkdir ebin;
	rebar3 compile;
	cp _build/default/lib/*/ebin/* ebin;
#	testing
	mkdir test_ebin;
	erlc -o test_ebin test/*.erl;
	erl -pa * -pa ebin -pa test_ebin\
	    -pa /home/joq62/erlang/infra_2/common/ebin\
	    -pa /home/joq62/erlang/infra_2/config/ebin\
	    -pa /home/joq62/erlang/infra_2/etcd/ebin\
	    -pa /home/joq62/erlang/infra_2/node/ebin\
	    -pa /home/joq62/erlang/infra_2/nodelog/ebin\
	    -pa /home/joq62/erlang/infra_2/sd/ebin\
	    -sname leader_test -run basic_eunit start
