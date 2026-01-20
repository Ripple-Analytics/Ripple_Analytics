%%%-------------------------------------------------------------------
%%% @doc Error Fixer - Automatically fixes compilation and runtime errors
%%% 
%%% Monitors for errors and uses LLM to generate fixes:
%%% - Erlang compilation errors
%%% - Docker build failures
%%% - Runtime crashes
%%% - Health check failures
%%% @end
%%%-------------------------------------------------------------------
-module(error_fixer).
%% Helper modules: error_fixer_part2, error_fixer_part3
-behaviour(gen_server).

-export([start_link/0, fix_compile_error/2, fix_build_error/2, 
         fix_runtime_error/2, get_fix_history/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    fix_history = [] :: list(),
    total_fixes = 0 :: integer(),
    successful_fixes = 0 :: integer(),
    failed_fixes = 0 :: integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Fix an Erlang compilation error
fix_compile_error(FilePath, ErrorOutput) ->
    gen_server:call(?MODULE, {fix_compile, FilePath, ErrorOutput}, 120000).

%% Fix a Docker build error
fix_build_error(ServiceName, BuildOutput) ->
    gen_server:call(?MODULE, {fix_build, ServiceName, BuildOutput}, 120000).

%% Fix a runtime error/crash
fix_runtime_error(ServiceName, ErrorLog) ->
    gen_server:call(?MODULE, {fix_runtime, ServiceName, ErrorLog}, 120000).

get_fix_history() ->
    gen_server:call(?MODULE, get_history).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[ERROR-FIXER] Error fixer initialized~n"),
    {ok, #state{}}.

handle_call({fix_compile, FilePath, ErrorOutput}, _From, State) ->
    io:format("[ERROR-FIXER] Attempting to fix compile error in ~s~n", [FilePath]),
    
    Result = do_fix_compile_error(FilePath, ErrorOutput),
    
    NewState = record_fix_attempt(State, compile, FilePath, Result),
    {reply, Result, NewState};

handle_call({fix_build, ServiceName, BuildOutput}, _From, State) ->
    io:format("[ERROR-FIXER] Attempting to fix build error for ~s~n", [ServiceName]),
    
    Result = do_fix_build_error(ServiceName, BuildOutput),
    
    NewState = record_fix_attempt(State, build, ServiceName, Result),
    {reply, Result, NewState};

handle_call({fix_runtime, ServiceName, ErrorLog}, _From, State) ->
    io:format("[ERROR-FIXER] Attempting to fix runtime error for ~s~n", [ServiceName]),
    
    Result = do_fix_runtime_error(ServiceName, ErrorLog),
    
    NewState = record_fix_attempt(State, runtime, ServiceName, Result),
    {reply, Result, NewState};

handle_call(get_history, _From, State) ->
    History = #{
        <<"total_fixes">> => State#state.total_fixes,
        <<"successful_fixes">> => State#state.successful_fixes,
        <<"failed_fixes">> => State#state.failed_fixes,
        <<"recent_fixes">> => lists:sublist(State#state.fix_history, 20)
    },
    {reply, {ok, History}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Compile Error Fixing
%%====================================================================

do_fix_compile_error(FilePath, ErrorOutput) ->
    %% Parse the error to understand what's wrong
    ParsedError = parse_erlang_error(ErrorOutput),
    
    %% Read the file content
    case file:read_file(FilePath) of
        {ok, Content} ->
            %% Generate fix using LLM
            Prompt = build_compile_fix_prompt(FilePath, Content, ParsedError, ErrorOutput),
            SystemPrompt = get_compile_fix_system_prompt(),
            
            case lm_client:generate(Prompt, SystemPrompt) of
                {ok, Response} ->
                    apply_compile_fix(FilePath, Content, Response);
                {error, Reason} ->
                    {error, {lm_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.
