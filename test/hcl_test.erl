-module(hcl_test).

-export([read_fixture/1]).

-include_lib("eunit/include/eunit.hrl").

read_fixture(File) ->
  Path = filename:join(fixtures_path(), File),
  case file:read_file(Path) of
    {ok, Binary} ->
      Binary;
    _ ->
      error(fail_to_read_fixture)
  end.

fixtures_path() ->
  filename:join(code:lib_dir(hcl, test), "fixtures").
