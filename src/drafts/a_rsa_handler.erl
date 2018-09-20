%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc The RSA-handler gen_server example
%%%
%%% @end
%%% Created : 08. Aug 2015 18:48
%%%-------------------------------------------------------------------
-module(a_rsa_handler).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").

-behaviour(gen_server).

%% API
-export([start/0,start_link/0]).
-export([test/0]).

%% Includes
-include_lib("public_key/include/public_key.hrl").

%% Gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

%% Server Definitions
%% ------------------------------------
%% !!! Set the SERVER_DIR for running this example
%% ------------------------------------
-define(SERVER, ?MODULE).
-define(KEY_PRIVATE_SOURCE,private_key_source).
-define(KEY_PUBLIC_SOURCE,public_key_source).
-define(KEY_PRIVATE,private_key).
-define(KEY_PUBLIC,public_key).
-define(KEY_STORE,acrypto_keys).
-define(SERVER_DIR, fun() -> "/Path/to/Erlang/Crypto/Server/" end).
-define(SERVER_LOGS_DIR, fun() -> lists:concat([?SERVER_DIR(),"logs/"]) end).
-define(SERVER_KEYS_DIR, fun() -> lists:concat([?SERVER_DIR(),"keys/"]) end).
-define(PUBLIC_KEY_FILE, fun() -> lists:concat([?SERVER_KEYS_DIR(),"id_rsa.pub"]) end).
-define(PRIVATE_KEY_FILE, fun() -> lists:concat([?SERVER_KEYS_DIR(),"id_rsa"]) end).

-record(state, {}).

%% -------------------------------------------------
%% Start gen_server
%% -------------------------------------------------
start() ->
	try
		%% Spawn gen_server process
		spawn(?MODULE,start_link,[]),
		io:fwrite("------------------\nServer started\n")
	catch
		Exception:Reason -> {Exception,Reason}
	end.


%% -------------------------------------------------
%% Gen_server handler
%% -------------------------------------------------
%% Start link for gen_server
start_link() ->
	gen_server:start_link({local,?SERVER},?MODULE,[],[]).

%% Init gen_server
init([]) ->
	%% Setting KEY_STORE cache, for production should be givven away
	%% Might be rissen like another one gen_server
	ets:new(?KEY_STORE,[set,public,named_table,{read_concurrency,true}]),
	%% Checking directory
	ensure_dir(),
	%% Load key-pair
	load_pair(),
	io:fwrite("Server keys loaded\n"),
	{ok, #state{}}.

%% Cast handler
handle_cast(_Request, State) ->
	{noreply, State}.

%% Info handler
handle_info(_Info, State) ->
	{noreply, State}.

%% Terminator
terminate(_Reason, _State) ->
	%% Delete key-pair
	unload_pair(),
	%% Delete KEY_STORE
	ets:delete(?KEY_STORE),
	io:fwrite("Server keys deleted\n"),
	io:fwrite("------------------\nServer stopped\n"),
	ok.

%% Code load handler
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% -------------------------------------------------
%% CALLS
%% -------------------------------------------------
handle_call(Request, _From, State) ->
	try
	    case Request of

	    %%  Heartbeat system
			heartbeat ->
				case heartbeat() of
					false -> {reply,{ok,false},State};
					_ -> {reply,{ok,true},State}
				end;

	    %%  Get public key
		    {get,public_key} ->
			    {reply, {ok,get_key(public)}, State};

	    %%  Encrypt message
		    {encrypt,Data} ->
			    case is_binary(Data) of
					true -> {reply, {ok,encrypt(public_key,Data)}, State};
					_ -> {reply, {error,wrong_data_type}, State}
			    end;

	    %%  Decrypt message
		    {decrypt,Data} ->
			    case is_binary(Data) of
				    true -> {reply, {ok,decrypt(private_key,Data)}, State};
				    _ -> {reply, {error,wrong_data_type}, State}
			    end;

	    %%  Set new pair
		    {set_pair,[Private_key,Public_key]} ->
			    set_pair(Private_key,Public_key),
			    {reply, {ok,keys_settled}, State};

	    %%  Unrecognised call
	        _ ->
		        {reply, {error,unrecognised_call}, State}

	    end
	catch
		Exception:Reason -> {reply, {error,{Exception,Reason}}, State}
	end.


%% -------------------------------------------------
%% Application handler
%% -------------------------------------------------

%% Functionality test function
test() ->
	%RsaEncrypted = encrypt(private_key,<<"Hello world!">>),
	%decrypt(public_key,RsaEncrypted)
	RsaEncrypted = encrypt(public_key,<<"Hello world!">>),
	decrypt(private_key,RsaEncrypted).

%% Application directory handler
ensure_dir() ->
	filelib:ensure_dir(?SERVER_DIR()),
	io:fwrite("Server directory checking passed\n"),
	filelib:ensure_dir(?SERVER_LOGS_DIR()),
	io:fwrite("Server logs directory checking passed\n"),
	filelib:ensure_dir(?SERVER_KEYS_DIR()),
	io:fwrite("Server keys directory checking passed\n").

%% Key-pair setter
set_pair(Private_key,Public_key) ->
	file:write_file(?PRIVATE_KEY_FILE(),Private_key,[write]),
	file:write_file(?PUBLIC_KEY_FILE(),Public_key,[write]),
	load_pair().

%% Key-pair loader
load_pair() ->
	{ok,Private_key_source} = file:read_file(?PRIVATE_KEY_FILE()),
	{ok,Public_key_source} = file:read_file(?PUBLIC_KEY_FILE()),
	[{Public_key,_}] = public_key:ssh_decode(Public_key_source,public_key),
	[Private_key_entry] = public_key:pem_decode(Private_key_source),
	Private_key = public_key:pem_entry_decode(Private_key_entry),
	ets:delete_all_objects(?KEY_STORE),
	ets:insert_new(?KEY_STORE,{?KEY_PRIVATE,Private_key}),
	ets:insert_new(?KEY_STORE,{?KEY_PUBLIC,Public_key}).

% Key-pair unloader
unload_pair() -> ets:delete_all_objects(?KEY_STORE).

%% Encrypt data
encrypt(public_key,Data) ->
	public_key:encrypt_public(Data, get_key(public));
encrypt(private_key,Data) ->
	public_key:encrypt_private(Data, get_key(private)).

%% Decrypt data
decrypt(public_key,Data) ->
	public_key:decrypt_public(Data, get_key(public));
decrypt(private_key,Data) ->
	public_key:decrypt_private(Data, get_key(private)).

%% Heartbeat system
heartbeat() -> true.

%% RSA keys from ETS Key Store
get_key(private) ->
	[{_,Value}] = ets:lookup(?KEY_STORE,?KEY_PRIVATE), Value;
get_key(public) ->
	[{_,Value}] = ets:lookup(?KEY_STORE,?KEY_PUBLIC), Value.