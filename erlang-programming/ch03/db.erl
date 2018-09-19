% Write a module db.erl that creates a database and is able to store, retrieve,
% and delete elements in it. The destroy/1 function will delete the database. 
% Considering that Erlang has garbage collection, you do not need to do anything. 
% Had the db module stored everything on file, however, you would delete the file. 
% We are including the destroy function to make the interface consistent. You may 
% not use the lists library module, and you have to implement all the recursive 
% functions yourself.
%
% Hint: use lists and tuples as your main data structures. When testing your 
%       program, remember that Erlang variables are single-assignment:
% 
% Interface:
% 		db:new()						⇒ Db.
% 		db:destroy(Db) 					⇒ ok.
%		db:write(Key, Element, Db) 		⇒ NewDb.
% 		db:delete(Key, Db) 				⇒ NewDb.
% 		db:read(Key, Db) 				⇒ {ok, Element} | {error, instance}.
%		db:match(Element, Db)			⇒ [Key1, ..., KeyN].
%

% Example: 
%
% 1> c(db). {ok,db}
% 2> Db = db:new().
% []
% 3> Db1 = db:write(francesco, london, Db). [{francesco,london}]
% 4> Db2 = db:write(lelle, stockholm, Db1). [{lelle,stockholm},{francesco,london}]
% 5> db:read(francesco, Db2).
% {ok,london}
% 6> Db3 = db:write(joern, stockholm, Db2). [{joern,stockholm},{lelle,stockholm},{francesco,london}] 7> db:read(ola, Db3).
% {error,instance}
% 8> db:match(stockholm, Db3).
% [joern,lelle]
% 9> Db4 = db:delete(lelle, Db3). [{joern,stockholm},{francesco,london}]
% 10> db:match(stockholm, Db4).
% [joern]
% 11>


-module(db).

-export([new/0, destroy/1]).
-export([write/3, read/2, match/2, delete/2]).


% Public Interface ------
%

new() ->
	[].

destroy(Db) ->
	Db.

write(Key, Element, Db) ->
 	do_write(Key, Element, Db).


read(Key, Db) ->
	Element = do_read(Key, Db),
	Element.

match(Element, Db) ->
	Keys = do_match(Element, Db, []),
	Keys.

delete(Key, Db) ->
	NewDb = do_delete(Key, Db, []),
	NewDb.


% Private Methods ------
%

do_write(Key, Element, Db) ->
	{Exists, _} = do_read(Key, Db),
	if
	 	Exists == ok ->
	 		{key_already_bound, Key};
	 	true ->
	 		[{Key, Element} | Db]
	end.


do_read(Key, [DbH|DbT]) ->
 	{DbHKey, DbHElement} = DbH,
 	if 
 		Key == DbHKey -> 
 			Result = {ok, DbHElement};
 	 	Key /= DbHKey ->
 	 		Result = do_read(Key, DbT)
 	end,
 	Result;
do_read(Key, []) ->
  	{error, Key}.


do_match(Element, [DbH|DbT], ResultKeys) ->
 	{DbHKey, DbHElement} = DbH,
 	if 
 		Element == DbHElement -> 
 			Result = do_match(Element, DbT, [DbHKey | ResultKeys]);
 	 	Element /= DbHElement ->
 	 		Result = do_match(Element, DbT, ResultKeys)
 	end,
 	Result;
do_match(_, [], ResultKeys) ->
  	ResultKeys.


do_delete(Key, [DbH|DbT], NewDb) ->
 	{DbHKey, _} = DbH,
 	if 
 		Key == DbHKey -> 
 			Db = do_delete(Key, DbT, NewDb);
 	 	Key /= DbHKey ->
 	 		Db = do_delete(Key, DbT, [DbH | NewDb])
 	end,
 	Db;
do_delete(_, [], NewDb) ->
  	NewDb.


