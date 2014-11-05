-module(hcl).

-export([load/1, parse/1]).

load(File) ->
  case file:read_file(File) of
    {ok, Binary} ->
      parse(Binary);
    Error ->
      Error
  end.

parse(Binary) ->
  hcl_parser:parse(hcl_lexer:lex(Binary)).
