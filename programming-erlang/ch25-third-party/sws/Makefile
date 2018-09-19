all:
	rebar clean
	test -d deps || rebar get-deps
	rebar compile
	@erl \
	  -noshell \
	  -pa './deps/ranch/ebin' \
	  -pa './deps/cowlib/ebin' \
	  -pa './deps/cowboy/ebin' \
	  -pa './ebin' \
	  -s sws start