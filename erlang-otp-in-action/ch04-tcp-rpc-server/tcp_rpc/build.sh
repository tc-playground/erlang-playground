#!/bin/bash

# Compile the OTP App..
#
erlc -o ebin  src/*.erl


# Run the OTP App.
#
# $ erl –pa ebin
# 
# NB: '-pa' stands for 'path add', adding a directory to the beginning of the code path.)
#
# With the Erlang shell up and running, you have to do only one thing to launch the 
# application: call the standard library function application:start/1, passing it the 
# application name tcp_rpc, like this:
# 
# Eshell V5.5.5  (abort with ^G)
# 1> application:start(tcp_rpc).
# ok


# Use the OTP App.
#
# $ telnet localhost 1055
#
# >>> Enter some Erlang Module:function:[Params] tests...
# e.g.: io:format("Hello!").
#       observer:start().
#       init:stop().
# 


# Generate the OTP App EDoc.
#
# $ erl –pa ebin
#
# Eshell V5.5.5  (abort with ^G)
# 1> edoc:application(tcp_rpc, ".", []).
# ok
#
# NB: The empty list [] is for additional options to EDoc (none right now), and the period (.) 
#     means the application is supposed to be found in the current directory.
#
# 
# OR
#
# $ cd ${PARENT_DIR}
# $ erl –pa tcp_rpc/ebin
#
# Eshell V5.5.5  (abort with ^G)
# 1> edoc:application(tcp_rpc).
# ok


