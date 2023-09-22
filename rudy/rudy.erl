-module(rudy).
-export([start/1, stop/0]).
start(Port) ->
register(rudy, spawn(fun() -> init(Port) end)).
stop() ->
exit(whereis(rudy), "time to die").

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      io:format("rudy: listening on port ~w~n", [Port]),
      handler(Listen);
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      io:format("rudy: client connected~n"),
      request(Client),
      handler(Listen);
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),
      Reply = reply(Request),
      gen_tcp:send(Client, Reply);
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).

% This function is used to reply to the client
reply({{get, URI, _}, _, _}) ->
    timer:sleep(40), % simulate a slow server
    T=tl(URI),
    RequestedFile = seperateQueryFromUri(T),
    {FileName, _} = RequestedFile,
    File = file:read_file(FileName),
    case File of
    % file is found
    {ok, Binary} ->
     Substring = string:split(FileName, "."),
     File_type = tl(Substring),
     Type = unicode:characters_to_list(File_type, {utf16, little}),
     % check the file type and return the appropriate header
     case Type of
      "html" ->
        http:ok("text/html", Binary);
      "txt" -> 
        http:ok("text/plain", Binary);
      "ico" ->
        http:ok("image/x-icon", Binary)
     end;
     % file is not found
    {error, Reason} ->
      io:format("rudy: error: ~w~n", [Reason]),
      http:ok("No such file with given name")
  end.

% Used to seperate the query from the URI
% If there is a questionmark we split the string input into a tuple on the questionmark, the first part is the URI and the second part is the query
seperateQueryFromUri(URI) ->
  case string:str(URI, "?") of
    0 -> {URI, []};
    N -> {string:substr(URI, 1, N-1), string:substr(URI, N+1, length(URI)-N)}
  end.