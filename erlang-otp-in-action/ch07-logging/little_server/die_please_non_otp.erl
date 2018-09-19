%%%============================================================================
%%% @doc 
%%% A non OTP version of the 'die_please' OTP GenServer.
%%% @end
%%%============================================================================
-module(die_please_non_otp).


%%%============================================================================
%%% Public Interface
%%%============================================================================

-export([go/0]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(SLEEP_TIME, 2000).


%%%============================================================================
%%% Public Interface Implementation
%%%============================================================================

go() ->
  %% just sleep for a while, then crash
  timer:sleep(?SLEEP_TIME),
  i_really_want_to_die = right_now.


%% *** Run - No SASL! ***
%%  

%% To use SASL, a little work is needed. When you build your application on 
%% behaviours like gen_server and supervisor, this work is already done for you.

% 9> die_please_non_otp:go.
% * 1: illegal expression
% 10> die_please_non_otp:go().
% ** exception error: no match of right hand side value right_now
%      in function  die_please_non_otp:go/0 (die_please_non_otp.erl, line 30)
% 11>


%% *** Run - With 'proc_lib' => SASL! (sort of) ***
%%

%% This time, you get a crash report from SASL. The proc_lib module is part of  
%% the Erlang stdlib application, and it supports starting processes the OTP way  
%% so they’re properly set up to follow all the necessary conventions. 
%%
%% In the (for now) unlikely event that you want to write processes that aren’t  
%% built on existing behaviours, you should typically start them via proc_lib. 
%% You’ll be doing yourself a favor in the long run.

% 11> proc_lib:spawn(fun die_please_non_otp:go/0).
% <0.75.0>
% 12>
% =CRASH REPORT==== 21-Aug-2014::17:32:47 ===
%   crasher:
%     initial call: die_please_non_otp:go/0
%     pid: <0.75.0>
%     registered_name: []
%     exception error: no match of right hand side value right_now
%       in function  die_please_non_otp:go/0 (die_please_non_otp.erl, line 30)
%     ancestors: [<0.73.0>]
%     messages: []
%     links: []
%     dictionary: []
%     trap_exit: false
%     status: running
%     heap_size: 376
%     stack_size: 27
%     reductions: 87
%   neighbours:
