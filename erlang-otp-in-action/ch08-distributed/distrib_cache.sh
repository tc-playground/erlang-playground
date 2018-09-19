# ***** Starting Nodes ********************************************************
#

# Node : Long Domain Name
#
# The first is used in a normal network environment, with a working 
# DNS, where you can use fully qualified domain names.
#
erl -pa /ebin -name simple_cache


# Node : Long Domain Name      
#
# Short names work as long as the nodes are on the same subnet. For 
# eample wireless LAN, where you may be able to connect two computers 
# to the network but they still can’t find each other via DNS.
#
erl -pa /ebin -sname simple_cache


# Long and short names can’t be mixed
#
# Nodes with short names and long names work in different communication modes 
# and can’t be part of the same Erlang cluster. All connected nodes must use 
# the same mode.


# ***** Connecting Nodes ******************************************************
#

# Create 3 nodes - a, b, c
#
# $> erl -name a
# $> erl -name b
# $> erl -name c


# $> erl -name a
# Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]
#
# Eshell V6.1  (abort with ^G)
# (a@plato.config)1>


# Check the 'connected nodes'
#
# (a@plato.config)1> nodes().
# []

# Connecting Nodes using 'net_adm'
#

# Successful Connection via 'ping' 
#
# (a@plato.config)3> net_adm:ping('b@plato.config').
# pong
# (a@plato.config)5> nodes().
# ['b@plato.config']
# (a@plato.config)6>

# Failed Connection via 'ping'
#
# (a@plato.config)2> net_adm:ping('b@mybox.home.net').
# pang


# Join Node C to Node A. Now the netowrk is fully connected!
#
# (c@plato.config)1> node().
# 'c@plato.config'
# (c@plato.config)2> net_adm:ping('a@plato.config').
# pong
# (c@plato.config)3> nodes().
# ['a@plato.config','b@plato.config']
# (c@plato.config)4>

# Kill Node B. Node 
#
# (b@plato.config)1> q().

# (a@plato.config)6> nodes().
# ['c@plato.config']
# (a@plato.config)7>


# Find the 'epmd' Erlang Port Mapper Daemon
#
# 502 Temple@plato:~
# $> ps ax | grep -i epmd
#  2103   ??  S      0:00.03 /usr/local/Cellar/erlang/17.1/lib/erlang/erts-6.1/bin/epmd -daemon
#  2408 s008  S+     0:00.00 grep -i epmd
#
#
# EPMD is the Erlang Port Mapper Daemon. Whenever you start a node, the node checks that 
# EPMD is running on your local machine and starts it otherwise. EPMD keeps track of 
# which nodes are running on the local machine and what ports they have been assigned. 
# 
# When an Erlang node on one machine wants to talk to a remote node, the local EPMD 
# talks to the EPMD on the remote machine (by default using TCP/IP on port 4369) and 
# asks if it has a node by that name up and running. If so, the remote EPMD replies 
# with the port used for communicating directly with the remote node. But EPMDs never 
# try to locate each other automatically communication must always be triggered by one 
# node looking for another.
#
#
# Advanced node discovery
#
# Systems exist that allow you to find and connect Erlang nodes through network 
# multicast, broadcast, and other clever means. If you’re running Erlang in a cloud 
# environment like EC2 where you add and remove computers on demand, you may want to 
# look into these. One project that has been receiving some attention recently is 
# nodefinder, at http://code.google.com/p/nodefinder/.


# ***** Node Cookies **********************************************************
#

# $> cat .erlang.cookie
# YVDZMDODSIZKQFITMIMC
#
# (a@plato.config)7> auth:get_cookie().
# 'YVDZMDODSIZKQFITMIMC'

# By default, a node assumes that all other nodes it wants to talk to are using the 
# same cookie as itself. When you start several nodes on a single computer, as you did 
# earlier (using the same user account and home directory), all of them use the same 
# cookie file, and so they’re allowed to communicate with each other. If you want nodes 
# on two different machines to communicate, the easiest way is to copy the generated 
# cookie file from one machine to the other, ensuring that they’re equal but still 
# sufficiently hard to guess. Preferably, nobody except the owner should have read 
# access to the cookie file.

# This security model guards against basic attacks—for example, if you’ve started an 
# Erlang node on a computer with no firewall, an attacker won’t be able to easily 
# guessyour cookie—but more importantly, it also guards against human error. Suppose 
# you have two separate clusters of Erlang nodes running on your network, and you don’t 
# want them to accidentally join to form a single fully connected cluster (for example, 
# if there’s a bandwidth bottleneck between them). By using different cookies, you 
# can guarantee that members of the two separate clusters won’t accidentally be 
# connected by a net_adm:ping(...) or similar.


# ***** Sending Messages between Nodes ****************************************
#

# $> erl -name a
# $> erl -name b
# $> erl -name c


# Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

# Create 'b@plato.config'; register the process as 'shell' and listen for message.
#
# Eshell V6.1  (abort with ^G)
# (b@plato.config)1> register(shell, self()).
# true
# (b@plato.config)2> receive
# (b@plato.config)2>  {From, Msg} ->
# (b@plato.config)2>    From ! {self(), "thanks"},
# (b@plato.config)2>    io:format("Msg: ~p~n", [Msg])
# (b@plato.config)2>  end.

# Create 'c@plato.config'; register the process as 'shell' and listen for message.
#
# Eshell V6.1  (abort with ^G)
# (c@plato.config)1> register(shell, self()).
# true
# (c@plato.config)2> receive
# (c@plato.config)2>  {From, Msg} ->
# (c@plato.config)2>    From ! {self(), "thanks"},
# (c@plato.config)2>    io:format("Msg: ~p~n", [Msg])
# (c@plato.config)2>  end.


# Create 'a@plato.config'; register with node 'b' and 'c' and send a 'hello'
# message to the 'shell' process on each node...
#
# Eshell V6.1  (abort with ^G)
# (a@plato.config)1> net_adm:ping('b@plato.config').
# pong
# (a@plato.config)2> net_adm:ping('c@plato.config').
# pong
# (a@plato.config)3> lists:foreach(
# (a@plato.config)3>  fun(Node) ->
# (a@plato.config)3>    {shell, Node} ! {self(), "hello!"}
# (a@plato.config)3>    end,
# (a@plato.config)3>    nodes()
# (a@plato.config)3>    ).
# ok
# (a@plato.config)4>

# =>  on b@plato.config.
# Msg: "hello!"
# ok
# (b@plato.config)3>

# =>  on b@plato.config.
# Msg: "hello!"
# ok
# (b@plato.config)3>

# Get the PId of the sender process on node 'a@plato.config'.
#
# (b@plato.config)4> From.
# <6104.38.0>

# Get the responses from 'b' and 'c' nodes.
#
# (a@plato.config)4> self().
# <0.38.0>
# (a@plato.config)5> receive R1 -> R1 end.
# {<6204.38.0>,"thanks"}
# (a@plato.config)6> receive R2 -> R2 end.
# {<7294.38.0>,"thanks"}
# (a@plato.config)7>

# Receive from 'a' and then send message from 'b'.
#
# (a@plato.config)7> receive R3 -> R3 end.
# =>
# (b@plato.config)5> From ! "Wobble!"
# (b@plato.config)5> .
# "Wobble!"
# =>
# "Wobble!"
# (a@plato.config)8>


# ***** Working with remote shells ********************************************
#

# Erlang’s location transparency is nicely demonstrated by its rather remarkable 
# ability to run shells remotely. After all, when you start a normal Erlang shell, 
# you get an Erlang process that talks to the input and output streams of your 
# console window. This communication is also built on message passing, and the 
# shell process doesn’t care much whether it’s running on the same node as the
# console it’s connected to. As a consequence, it’s easy to start a shell process 
# that runs on the remote node and does all its work there, but that is connected 
# to the console of your local node.


# <Ctrl+G>
# User switch command
#  --> h
#   c [nn]            - connect to job
#   i [nn]            - interrupt job
#   k [nn]            - kill job
#   j                 - list all jobs
#   s [shell]         - start local shell
#   r [node [shell]]  - start remote shell
#   q                 - quit erlang
#   ? | h             - this message


# The 'r' command takes a node name as an argument and starts a remote shell job on that 
# node, like the s command starts a new local job.
#
#  --> r 'b@mybox.home.net'
#  -->

# The 'j' command inspects the list of currently running jobs.
#
#  --> j
#   1  {shell,start,[init]}
#   2* {'b@mybox.home.net',shell,start,[]}
#  -->
#
# The 'c(id)'' command connects to the specfied job (default is *).


# ***** Quit with care when leaving a remote shell ****************************
#

# When you’re finished with your remote shell session and ready to quit, your 
# fingers may find themselves typing the shell shortcut q(). Stop! Don’t press 
# Enter! That command is short for init:stop(), which shuts down the node where t
# he com- mand is executed: that is, the remote node. Probably every Erlang 
# programmer has been burned by this at one time or another. The Ctrl-G and 
# Ctrl-C (Ctrl-Break) escapes are safe to use, because they always work within 
# the context of your local node. Use Ctrl-G followed by Q, or Ctrl-C (Ctrl-Break 
# on Windows) followed by A, to shut down the local node and leave the remote 
# node running.






