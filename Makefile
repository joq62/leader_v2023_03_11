all:
	rm -rf  *~ */*~  src/*.beam test/*.beam erl_cra*;
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
	rm -rf _build logs ;
	rm -rf ebin;
	mkdir test_ebin;
	mkdir ebin;
	rebar3 compile;
	cp _build/default/lib/*/ebin/* ebin;
	erlc -o test_ebin test/*.erl;
	erl -pa ebin -pa test_ebin\
	    -pa /home/joq62/erlang/infrastructure/sd/ebin\
	    -sname test -run basic_eunit start -setcookie cookie_test
release:
	rm -rf  *~ */*~  test_ebin erl_cra*;
	mkdir test_ebin;
	erlc -o test_ebin test/*.erl;
	erl -pa test_ebin -run release start leader ../catalog/catalog.specs
