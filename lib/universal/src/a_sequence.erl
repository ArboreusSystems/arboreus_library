%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc Sequesnce handler
%%%
%%% @end
%%% Created : 30. Jul 2015 1:27
%%%-------------------------------------------------------------------
-module(a_sequence).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").

%% Data types
-include("../../data_models/types/types_general.hrl").

%% Module API
-export([
	test/0,
	dictionaries/0,
	make_dictionary/1,
	make_password_hash/3,
	random_seed/0,
	random/2,random/3,
	md/2,
	unique/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Generate password hash
-spec make_password_hash(Password,Salt,Encryption_type) -> md()
	when
	Password :: utf_text_string() | utf_text_binary(),
	Salt :: utf_text_string() | utf_text_binary(),
	Encryption_type :: md5 | md4.

make_password_hash(Password,Salt,Encryption_type) when is_list(Salt) ->
	make_password_hash(
		Password,unicode:characters_to_binary(Salt),Encryption_type
	);
make_password_hash(Password,Salt,Encryption_type) when is_list(Password) ->
	make_password_hash(
		unicode:characters_to_binary(Password),Salt,Encryption_type
	);
make_password_hash(Password,Salt,Encryption_type)
	when is_binary(Password), is_binary(Salt) ->
	md(<<Password/binary,Salt/binary>>,Encryption_type).


%%-----------------------------------
%% @spec dictionaries() -> list()
%% @doc Return a list of registered dictionaries
-spec dictionaries() -> list().

dictionaries() ->
	[
		{numeric,<<("0123456789")/utf8>>},
		{alpha_lower,<<("abcdefghijklmnopqrstuvwxyz")/utf8>>},
		{alpha_upper,<<("ABCDEFGHIJKLMNOPQRSTUVWXYZ")/utf8>>}
	].


%%-----------------------------------
%% @doc Return a binary within a dictionary
-spec make_dictionary(Schema) -> byte()
	when
	Schema :: list().

make_dictionary(Schema) when is_list(Schema) ->
	make_dictionary(Schema,<<>>).


%%-----------------------------------
%% @doc Return a binary within a dictionary
-spec make_dictionary(Schema,Dictionary) -> byte()
	when
	Schema :: list(),
	Dictionary :: byte().

make_dictionary([],Dictionary) when is_binary(Dictionary) -> Dictionary;
make_dictionary([Head|Tail],Dictionary) when is_binary(Dictionary) ->
	Part = proplists:get_value(Head,dictionaries()),
	make_dictionary(Tail,<<Part/binary,Dictionary/binary>>).


%%-----------------------------------
%% @doc Seeding random generator
-spec random_seed() -> tuple().

random_seed() ->
	Integer1 = erlang:phash2([node()]),
	Integer2 = erlang:unique_integer()*(-1),
	Integer3 = erlang:monotonic_time()*(-1),
	rand:seed(exs64,{Integer1,Integer2,Integer3}).


%%-----------------------------------
%% @doc Return a binary within sequence
-spec random(Dictionary_schema,Length) -> byte()
	when Dictionary_schema :: list(), Length :: pos_integer().

random(Dictionary_schema,Length)
	when
	is_list(Dictionary_schema),
	is_integer(Length),
	Length > 0 ->
	make_random(make_dictionary(Dictionary_schema),Length).


%%-----------------------------------
%% @doc Return random value from range between Minor and Major
-spec random(number,Minor,Major) -> integer()
	when
	Minor :: pos_integer(),
	Major :: pos_integer().

random(number,Minor,Major)
	when
	is_integer(Minor), Minor >= 0,
	is_integer(Major), Major > Minor ->
	random_seed(),
	rand:uniform(Major - Minor) + Minor.


%%-----------------------------------
%% @doc Return a binary within sequense by the Dictionary
-spec make_random(Dictionary,Length) -> byte()
	when
	Dictionary :: list(),
	Length :: integer().

make_random(Dictionary,Length) ->
	make_random(Dictionary,Length,<<>>).
make_random(_,0,Sequence) ->
	Sequence;
make_random(Dictionary,Length,Sequence) ->
	Random_byte = binary:part(Dictionary,{random(number,0,byte_size(Dictionary))-1,1}),
	make_random(Dictionary,Length-1,<<Random_byte/binary,Sequence/binary>>).


%%-----------------------------------
%% @doc Return a binary within a hash from object
-spec md(Object,Type) -> byte() | {error,_Reason}
	when Object :: any(), Type :: md4 | md5.

md(Object,Type)
	when
	Type == md4; Type == md5 ->
	<<  A1:4, A2:4, A3:4, A4:4, A5:4, A6:4, A7:4, A8:4,
	A9:4, A10:4,A11:4,A12:4,A13:4,A14:4,A15:4,A16:4,
	A17:4,A18:4,A19:4,A20:4,A21:4,A22:4,A23:4,A24:4,
	A25:4,A26:4,A27:4,A28:4,A29:4,A30:4,A31:4,A32:4
	>> = crypto:hash(Type,Object),
	<<  (md_hex(A1)),  (md_hex(A2)),  (md_hex(A3)),  (md_hex(A4)),
	(md_hex(A5)),  (md_hex(A6)),  (md_hex(A7)),  (md_hex(A8)),
	(md_hex(A9)),  (md_hex(A10)), (md_hex(A11)), (md_hex(A12)),
	(md_hex(A13)), (md_hex(A14)), (md_hex(A15)), (md_hex(A16)),
	(md_hex(A17)), (md_hex(A18)), (md_hex(A19)), (md_hex(A20)),
	(md_hex(A21)), (md_hex(A22)), (md_hex(A23)), (md_hex(A24)),
	(md_hex(A25)), (md_hex(A26)), (md_hex(A27)), (md_hex(A28)),
	(md_hex(A29)), (md_hex(A30)), (md_hex(A31)), (md_hex(A32)) >>.

md_hex(X) ->
	element(X+1,{$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f}).


%%-----------------------------------
%% @doc Return a binary within random unique sequence
-spec unique(Source) -> byte()
	when
	Source :: time | any().

unique(time) ->
	Time = a_var:to_binary(a_time:timestamp()),
	Random_value = make_random(make_dictionary([alpha_lower]),64),
	md(<<Time/binary,Random_value/binary>>,md4);
unique(Object) ->
	Object_sequence = md(Object,md5),
	Random_value = make_random(make_dictionary([alpha_lower,numeric]),64),
	md(<<Object_sequence/binary,Random_value/binary>>,md4).