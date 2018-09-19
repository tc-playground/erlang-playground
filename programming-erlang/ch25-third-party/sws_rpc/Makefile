all:
	rebar clean
	rebar get-deps
	test -d deps || rebar get-deps
	rebar compile
	@erl \
	  -noshell \
	  -pa './deps/mochiweb/ebin' \
	  -pa './deps/ranch/ebin' \
	  -pa './deps/cowlib/ebin' \
	  -pa './deps/cowboy/ebin' \
	  -pa './ebin' \
	  -s sws_rpc start