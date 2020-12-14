-module(day13).
-export([main/1]).

main(_) ->
    {ok, File} = file:read_file("resources/input.txt"),
    io:fwrite(File).
