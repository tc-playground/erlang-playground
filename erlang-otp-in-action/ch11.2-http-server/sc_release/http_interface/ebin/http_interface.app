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

{application, http_interface,
 [{description, "A simple http interface."},
  {vsn, "0.1.0"},
  {modules, [
    hi_app, 
    hi_sup,
    hi_server
    ]},
  {registered, [hi_sup]},
  {applications, [
    kernel, 
    stdlib,
    sasl
    ]},
  {mod, {hi_app, []}}
]}.