-module(http).
-export([parse_request/1, ok/1, get/1, ok/2]).

parse_request(R0) ->
{Request, R1} = request_line(R0),
{Headers, R2} = headers(R1),
{Body, _} = message_body(R2),
{Request, Headers, Body}.

request_line([$G, $E, $T, 32 |R0]) ->
{URI, R1} = request_uri(R0),
{Ver, R2} = http_version(R1),
[13,10|R3] = R2,
{{get, URI, Ver}, R3}.

request_uri([32|R0])->
{[], R0};
request_uri([C|R0]) ->
{Rest, R1} = request_uri(R0),
{[C|Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
{v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
{v10, R0}.

headers([13,10|R0]) ->
{[],R0};
headers(R0) ->
{Header, R1} = header(R0),
{Rest, R2} = headers(R1),
{[Header|Rest], R2}.

header([13,10|R0]) ->
{[], R0};
header([C|R0]) ->
{Rest, R1} = header(R0),
{[C|Rest], R1}.

message_body(R) ->
{R, []}.

% Used to construct a response to a request
ok(Body) ->
   "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

% Used to construct a request
get(URI) ->
"GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".

% Used to construct a response to a request with additional info for when a file is requested
ok(Content_Type, Body) ->
    ResponseHeader = "HTTP/1.1 200 OK\r\n",
    ContentTypeHeader = "Content-Type: " ++ Content_Type ++ "\r\n",
    ContentEncodingHeader = "Content-Encoding: identity \r\n",
    ContentLengthHeader = "Content-Length: " ++ integer_to_list(byte_size(Body)) ++ "\r\n",
    Response = ResponseHeader ++ ContentTypeHeader ++ ContentEncodingHeader ++ ContentLengthHeader ++ "\r\n" ++ Body,
    Response.