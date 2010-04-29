-module(s3test).
-export([run/3]).
-include("s3.hrl").
 
run( AKI, SAK, Bucket ) ->

    Credentials = #aws_credentials{ accessKeyId=AKI, secretAccessKey=SAK },
    Key = "testkey",

    {ok, Pid} = s3pool_sup:start_link( Credentials, 1 ),

    io:format("Exercising s3erl on bucket ~p~n", [Bucket]),
    io:format("Listing all buckets: ~p~n", [ s3:list_buckets() ] ),
    io:format("Creating new bucket: ~p~n", [ s3:create_bucket( Bucket ) ] ),

    io:format("Writing object ~p~n", [s3:write_object( Bucket, Key, <<1,2,3>>, "text/plain")] ),
    io:format("Listing objects ~p~n", [s3:list_objects( Bucket )] ),
    io:format("Deleting object ~p~n", [s3:delete_object( Bucket, Key )] ),
    io:format("Deleting bucket ~p~n", [s3:delete_bucket( Bucket )] ),
    exit(Pid, kill),
    io:format("Test completed~n", []).
