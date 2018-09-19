%%%============================================================================
%%% @doc 
%%% An 'event stream generator' for the 'simple_cache' module.
%%% @end
%%%============================================================================

-module(sc_event).


%%%============================================================================
%%% OTP GenEvent Behaviour
%%%============================================================================

% -behaviour(gen_event).


%%%============================================================================
%%% Public API
%%%============================================================================

%% Management API
-export([
  start_link/0,
  add_handler/2,
  delete_handler/2
]).

%% Client API
-export([
  lookup/1,
  create/2,
  create/3,
  replace/2,
  delete/1
]).


%%%============================================================================
%%% Macros
%%%============================================================================

-define(SERVER, ?MODULE).


%%%============================================================================
%%% Public Management API Implementation
%%%============================================================================

%% This API module doesn’t implement any specific OTP behaviour. But it does 
%% provide a start_link() function similar to what you’re used to. In this 
%% case, it hides a call to the function gen_event:start_link/1, starting a 
%% new gen_event container and registering it locally using the same name as 
%% the module.
%%
%% Many gen_event behaviour implementation modules don’t provide a 'start_link' 
%% API function. Normally, the gen_event container (also called the event 
%% manager) is instead started directly from a supervisor, as illustrated by 
%% the following child specification example:
%%
%% {
%%    my_logger,
%%    {gen_event, start_link, [{local, my_logger}]},
%%    permanent, 
%%    1000, 
%%    worker, 
%%    [gen_event]
%% }
%%
%% After it’s started, the process can be referenced by the name 'my_logger' 
%% in order to add handlers. Although, this is letting an implementation 
%% details (the name of the process 'my_logger') leak into the code. It is far 
%% better to mange such details within the API. This makes the user interface 
%% completely independent of the registered name.
%%
start_link() ->
  % Hide gen_event start function.
  gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
  % Hides gen_event handler registration.
  gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
  % Hides gen_event handler de-registration.
  gen_event:delete_handler(?SERVER, Handler, Args).


%%%============================================================================
%%% Public Cient API Implementation
%%%============================================================================

%% For event handlers, the encapsulation isn’t as complete as for gen_servers:  
%% the protocol defined in this API module must be understood by every callback 
%% module you want to add, and so it should be documented (possibly as internal 
%% documentation. The terms used in the protocol shouldn’t be allowed to leak 
%% into any other part of the system.
%%
%% The protocol for the 'sc_event' custom event stream: 
%%
%% +---------------------------------------+---------------------+
%% | Event Tuple                           | Posted By           |
%% +---------------------------------------+---------------------+
%% |{lookup, Key}                          | sc_event:lookup/1   |
%% |{create, {Key, Value}}                 | sc_event:create/2   |
%% |{create, {Key, Value, LeaseTime}}      | sc_event:create/3   |
%% |{replace, {Key, Value}}                | sc_event:replace/2  |
%% |{delete, Key} | warning_report()       | sc_event:delete/1   |
%% +---------------------------------------+---------------------+
%%
%% With this API in place, when you want to do something like post a lookup 
%% event, all you need to do is call sc_event:lookup(Key). If you need to change 
%% the event protocol or any other detail of the implementation, you won’t have 
%% to go back and modify every line that posts such an event throughout your code 
%% base.
%%

lookup(Key) ->
  % The gen_event module provides the function notify/2 for posting events 
  % asynchronously, similar to the cast/2 function in gen_server.
  gen_event:notify(?SERVER, {lookup, Key}).

create(Key, Value) ->
  % The gen_event module provides the function notify/2 for posting events 
  % asynchronously, similar to the cast/2 function in gen_server.
  gen_event:notify(?SERVER, {create, {Key, Value}}).

create(Key, Value, LeaseTime) ->
  % The gen_event module provides the function notify/2 for posting events 
  % asynchronously, similar to the cast/2 function in gen_server.
  gen_event:notify(?SERVER, {create, {Key, Value, LeaseTime}}).

replace(Key, Value) ->
  % The gen_event module provides the function notify/2 for posting events 
  % asynchronously, similar to the cast/2 function in gen_server.
  gen_event:notify(?SERVER, {replace, {Key, Value}}).

delete(Key) ->
  % The gen_event module provides the function notify/2 for posting events 
  % asynchronously, similar to the cast/2 function in gen_server.
  gen_event:notify(?SERVER, {delete, Key}).




