%%%-------------------------------------------------------------------
%%% File    : s3.erl
%%% Author  : Andrew Birkett <andy@nobugs.org>
%%% Description : 
%%%
%%% Created : 14 Nov 2007 by Andrew Birkett <andy@nobugs.org>
%%%-------------------------------------------------------------------
-module(s3).

%% API
-export([ list_buckets/0, create_bucket/1, delete_bucket/1,
	  list_objects/2, list_objects/1, write_object/4, write_object/5, read_object/2, delete_object/2 ]).


-include_lib("xmerl/include/xmerl.hrl").
-include("s3.hrl").

-define(RETRIES, 3).

%%====================================================================
%% API
%%====================================================================
create_bucket (Name) -> do_put(Name).
delete_bucket (Name) -> do_delete(Name).
list_buckets ()      -> do_listbuckets().

write_object (Bucket, Key, Data, ContentType) -> 
    write_object (Bucket, Key, Data, ContentType, undefined).
write_object (Bucket, Key, Data, ContentType, ACL) -> 
    do_put(Bucket, Key, Data, ContentType, ACL).
read_object (Bucket, Key) -> 
    do_get(Bucket, Key).
delete_object (Bucket, Key) -> 
    do_delete(Bucket, Key).

%% option example: [{delimiter, "/"},{maxresults,10},{prefix,"/foo"}]
list_objects (Bucket, Options ) -> do_list(Bucket, Options).
list_objects (Bucket) -> list_objects(Bucket, []).



% Bucket operations
do_listbuckets() ->
    xmlToBuckets(getRequest( "", "", [] )).

do_put(Bucket) ->
    {_Headers,_Body} = putRequest( Bucket, "", <<>>, "", undefined),
    ok.

do_delete(Bucket) ->
    try 
	{_Headers,_Body} = deleteRequest( Bucket, ""),
	ok
    catch
	throw:X -> X
    end.

% Object operations
do_put(Bucket, Key, Content, ContentType, ACL) ->
    {Headers,_Body} = putRequest( Bucket, Key, Content, ContentType, ACL),
    [ETag | _] = [V || {K, V} <- Headers,
		       string:to_lower(K) == "etag"],
    {ok, ETag}.

do_list(Bucket, Options) ->
    Params = lists:map( fun option_to_param/1, Options ),
    {_, Body} = getRequest( Bucket, "", Params ),
    parseBucketListXml(Body).

do_get(Bucket, Key) ->
    getRequest( Bucket, Key, [] ).

do_delete(Bucket, Key) ->
    try 
	{_Headers,_Body} = deleteRequest( Bucket, Key),
	ok
    catch
	throw:X -> X
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

s3Host () ->
    "s3.amazonaws.com".

option_to_param( { prefix, X } ) -> 
    { "prefix", X };
option_to_param( { maxkeys, X } ) -> 
    { "max-keys", integer_to_list(X) };
option_to_param( { delimiter, X } ) -> 
    { "delimiter", X }.

getRequest( Bucket, Key, Params ) ->
    genericRequest( get, Bucket, Key, Params, [], <<>>, "" ).
putRequest( Bucket, Key, Content, ContentType, ACL) ->
    Headers = case ACL of
		  [_ | _] -> [{"x-amz-acl", ACL}];
		  _ -> []
	      end,
    genericRequest( put, Bucket, Key, [], Headers, Content, ContentType ).
deleteRequest( Bucket, Key ) ->
    genericRequest( delete, Bucket, Key, [], [], <<>>, "" ).


isAmzHeader( Header ) -> lists:prefix("x-amz-", Header).

canonicalizedAmzHeaders( AllHeaders ) ->
    AmzHeaders = [ {string:to_lower(K),V} || {K,V} <- AllHeaders, isAmzHeader(K) ],
    Strings = lists:map( 
		fun s3util:join/1, 
		s3util:collapse( 
		  lists:keysort(1, AmzHeaders) ) ),
    s3util:string_join( lists:map( fun (S) -> S ++ "\n" end, Strings), "").
    
canonicalizedResource ( "", "" ) -> "/";
canonicalizedResource ( Bucket, "" ) -> "/" ++ Bucket ++ "/";
canonicalizedResource ( Bucket, Path ) -> "/" ++ Bucket ++ "/" ++ Path.

stringToSign ( Verb, ContentMD5, ContentType, Date, Bucket, Path, OriginalHeaders ) ->
    Parts = [ Verb, ContentMD5, ContentType, Date, canonicalizedAmzHeaders(OriginalHeaders)],
    s3util:string_join( Parts, "\n") ++ canonicalizedResource(Bucket, Path).
    
sign (Key,Data) ->
%    io:format("Data being signed is ~p~n", [Data]),
    binary_to_list( base64:encode( crypto:sha_mac(Key,Data) ) ).

queryParams( [] ) -> "";
queryParams( L ) -> 
    Stringify = fun ({K,V}) -> K ++ "=" ++ V end,
    "?" ++ s3util:string_join( lists:map( Stringify, L ), "&" ).

buildHost("") -> s3Host();
buildHost(Bucket) -> Bucket ++ "." ++ s3Host().
    
buildUrl(Bucket,Path,QueryParams) -> 
    ibrowse_lib:parse_url("http://" ++ buildHost(Bucket) ++ "/" ++ Path ++ queryParams(QueryParams)).

buildContentHeaders( <<>>, _ ) -> [];
buildContentHeaders( Contents, ContentType ) -> 
    [{"Content-Length", integer_to_list(size(Contents))},
     {"Content-Type", ContentType}].

genericRequest( Method, Bucket, Path, QueryParams, UserHeaders, Contents, ContentType ) ->
    Date = httpd_util:rfc1123_date(),
    MethodString = string:to_upper( atom_to_list(Method) ),
    Url = buildUrl(Bucket,Path,QueryParams),

    OriginalHeaders = [{"Connection", "keep-alive"}] ++
	buildContentHeaders( Contents, ContentType ) ++
	UserHeaders,
    ContentMD5 = "",
    Body = Contents,

    {ok, #aws_credentials{ accessKeyId=AKI, secretAccessKey=SAK }} = s3pool:get_credentials(),

    Signature = sign( SAK,
		      stringToSign( MethodString, ContentMD5, ContentType, 
				    Date, Bucket, Path, OriginalHeaders )),

    Headers = [ {"Authorization","AWS " ++ AKI ++ ":" ++ Signature },
		{"Host", buildHost(Bucket) },
		{"Date", Date } 
	       | OriginalHeaders ],
    
%    io:format("Sending request ~p~n", [Request]),
    Options = case Method of
		put ->
		      [{content_type, ContentType}];
		_ ->
		      [{response_format, binary}]
	    end,
    Reply = attempt(
	      fun() ->
		      {ok, HttpClient} = s3pool:get_worker(),
		      ibrowse_http_client:send_req(HttpClient, 
						   Url, Headers, Method,
						   Body, Options, 10000)
	      end, ?RETRIES),
%    io:format("HTTP reply was ~p~n", [Reply]),
    case Reply of
	{ok, Code, ResponseHeaders, ResponseBody } 
	 when Code=="200"; Code=="204"
	      -> 
	    {ResponseHeaders,ResponseBody};

	{ok, _HttpCode, _ResponseHeaders, ResponseBody } -> 
	    throw ( parseErrorXml(ResponseBody) )
    end.


parseBucketListXml (Xml) ->
    {XmlDoc, _Rest} = xmerl_scan:string( binary_to_list(Xml) ),
    ContentNodes = xmerl_xpath:string("/ListBucketResult/Contents", XmlDoc),

    GetObjectAttribute = fun (Node,Attribute) -> 
		      [Child] = xmerl_xpath:string( Attribute, Node ),
		      {Attribute, s3util:string_value( Child )}
	      end,

    NodeToRecord = fun (Node) ->
			   #object_info{ 
			 key =          GetObjectAttribute(Node,"Key"),
			 lastmodified = GetObjectAttribute(Node,"LastModified"),
			 etag =         GetObjectAttribute(Node,"ETag"),
			 size =         GetObjectAttribute(Node,"Size")}
		   end,
    { ok, lists:map( NodeToRecord, ContentNodes ) }.

parseErrorXml (Xml) ->
    {XmlDoc, _Rest} = xmerl_scan:string( binary_to_list(Xml) ),
    [#xmlText{value=ErrorCode}]    = xmerl_xpath:string("/Error/Code/text()", XmlDoc),
    [#xmlText{value=ErrorMessage}] = xmerl_xpath:string("/Error/Message/text()", XmlDoc),
    { s3error, ErrorCode, ErrorMessage }.


xmlToBuckets( {_Headers,Body} ) ->
    {XmlDoc, _Rest} = xmerl_scan:string( binary_to_list(Body) ),
    TextNodes       = xmerl_xpath:string("//Bucket/Name/text()", XmlDoc),
    lists:map( fun (#xmlText{value=T}) -> T end, TextNodes).


attempt(F, Retries) ->
    case (catch F()) of
	{error, connection_closed} when Retries > 0 ->
	    attempt(F, Retries - 1);
	{error, timeout} when Retries > 0 ->
	    attempt(F, Retries - 1);
	{'EXIT', Reason} when Retries > 0 ->
	    exit(Reason);
	R ->
	    R
    end.
