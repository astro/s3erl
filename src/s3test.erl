-module(s3test).
-export([run/3]).
-include("s3.hrl").
 
run( AKI, SAK, Bucket ) ->

    Credentials = #aws_credentials{ accessKeyId=AKI, secretAccessKey=SAK },
    Key = "testkey",

    {ok, _} = s3:start( Credentials ),
    s3:set_acl("public-read"),

    io:format("Exercising s3erl on bucket ~p~n", [Bucket]),
    io:format("Listing all buckets: ~p~n", [ s3:list_buckets() ] ),
    io:format("Creating new bucket: ~p~n", [ s3:create_bucket( Bucket ) ] ),

    Write = <<1, 2, 3>>,
    io:format("Writing object ~p~n", [s3:write_object( Bucket, Key, Write, "text/plain")] ),
    {_, Read} = s3:read_object( Bucket, Key ),
    io:format("Reading object, match? ~p~n", [list_to_binary(Read) == Write] ),
    io:format("Listing objects ~p~n", [s3:list_objects( Bucket )] ),
    io:format("Deleting object ~p~n", [s3:delete_object( Bucket, Key )] ),
    io:format("Deleting bucket ~p~n", [s3:delete_bucket( Bucket )] ),
    io:format("Test completed~n", []).
