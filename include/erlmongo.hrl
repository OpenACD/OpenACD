% recindex is index of record in RECTABLE define, it has to be the first record field
% docid is _id in mongodb, it has to be named docid and it has to be the second field in the record
-record(mydoc, {recindex = 1, docid, name, i, address, tags}).
-record(address, {recindex = 2, docid, street, city, country}).
% metadata is an embedded document,
-record(gfs_file, {recindex = 3, docid, filename, contentType, length, chunkSize, uploadDate, aliases, metadata, md5}).
-record(gfs_chunk, {recindex = 4, docid, files_id, n, data}).
% A table of records used with mongodb (tuple of record fields).
% If you arent using an embedded record, you can use record_info(fields, name_of_record)
% If a record uses an embedded record, you have to write the fields yourself
%  and the field which is an embedded record is: {name_of_record, index_of_record_in_RECTABLE}
%  field name also has to match the record name.
-define(RECTABLE, {[recindex,docid,name,i, {address, 2}, tags],
                   record_info(fields, address),
				   % If you wish to use metadata embedded record.
				   % [recindex, docid, filename, contentType, length, chunkSize, uploadDate, aliases, {metadata, INDEX_HERE}, md5]
				   record_info(fields, gfs_file),
				   record_info(fields, gfs_chunk)}).

-record(gfs_state,{pool,proc, db, file, collection, coll_name, length = 0, mode,
				   nchunk = 0, flush_limit = 1048576, closed = false}).

-export([rec2prop/2, prop2rec/4]).

% Convert record to prop list
rec2prop(Rec, RecordFields) ->
	loop_rec(RecordFields, 1, Rec, []).

loop_rec([H|T], N, Rec, L) ->
	loop_rec(T, N+1, Rec, [{H, element(N+1, Rec)}|L]);
loop_rec([], _, _, L) ->
	L.

% convert prop list to record
prop2rec(Prop, RecName, DefRec, RecordFields) ->
	loop_fields(erlang:make_tuple(tuple_size(DefRec), RecName), RecordFields, DefRec, Prop, 2).

loop_fields(Tuple, [Field|T], DefRec, Props, N) ->
	case lists:keysearch(Field, 1, Props) of
		{value, {_, Val}} ->
			loop_fields(setelement(N, Tuple, Val), T, DefRec, Props, N+1);
		false ->
			loop_fields(setelement(N, Tuple, element(N, DefRec)), T, DefRec, Props, N+1)
	end;
loop_fields(Tuple, [], _, _, _) ->
	Tuple.

-ifdef(DEBUG).
-define(DBG(Format, Args), io:format("L(~p:~p:~p:~p) : "++Format++"~n", [time(),self(),?MODULE,?LINE]++Args)).
-define(DBG0(Format), io:format("L(~p:~p:~p:~p) : "++Format++"~n", [time(),self(),?MODULE,?LINE])).
-else.
-define(DBG(F,A),[]).
-define(DBG0(F),[]).
-endif.

% mongo
-define(QUER_OPT_NONE, 0).
-define(QUER_OPT_CURSOR, 2).
-define(QUER_OPT_SLAVEOK, 4).
-define(QUER_OPT_NOTIMEOUT, 16).

% criteria = either a record or proplist with parameters you are searching by
% field_selector = list of fields you wish to return
% ndocs = how many documents you wish to return, 0 = default
% nskip = how many documents to skip
% opts - Don't touch it.
-record(search, {ndocs = 0, nskip = 0, criteria = <<>>, field_selector = <<>>, opts = ?QUER_OPT_NONE}).
-record(cursor, {id, pid, limit = 0}).
-record(update, {upsert = 1, selector = <<>>, document = <<>>}).
-record(insert, {documents = []}).
-record(delete, {selector = <<>>}).
-record(killc, {cur_ids = <<>>}).
