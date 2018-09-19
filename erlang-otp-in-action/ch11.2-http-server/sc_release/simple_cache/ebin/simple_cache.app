%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-


%%-----------------------------------------------------------------------------
%% The OTP 'app file' is used by OTP to understand how the application should 
%% be started and how it fits in with other applications in the system.
%%
%% Defined by tuple:
%%
%%  {application, AppName, KeyValParamList} =
%%    {application, $APP_NAME, [{Key1, Va1}, {Key2, Val2}}]}
%%
%%
%% # KeyVal Params #
%%
%% 'description'  - App description.
%% 
%% 'vsn'          - <major>.<minor>.<patch>
%% 
%% 'modules'      - A list of all the modules in your application.
%% 
%% 'registered'   - Processes to 'register' by name.
%% 
%% 'applications' - All the applications that need to be started before this 
%%                  application can start. (Dependencies).
%% 
%% 'mod'          - Tells the OTP system how to start your application. The 
%%                  value is a tuple containing the module name along with  
%%                  some optional startup arguments.
%%

{application, simple_cache,
 [{description, "A simple caching system"},
  {vsn, "0.3.0"},
  {modules, [
    sc_app, 
    sc_sup,
    sc_element_sup,
    sc_store,
    sc_element,
    sc_event,
    sc_event_logger
    ]},
  {registered, [sc_sup]},
  {applications, [
    kernel, 
    stdlib,
    sasl,
    mnesia,
    resource_discovery
    ]},
  {mod, {sc_app, []}}
]}.