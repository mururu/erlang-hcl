-module(hcl_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_CASES, [
                     {"basic.hcl",
                      {[
                        {<<"foo">>, <<"bar">>},
                        {<<"bar">>, <<"${file(\"bing/bong.txt\")}">>}
                      ]}
                     },
                     {"basic_squish.hcl",
                      {[
                        {<<"foo">>, <<"bar">>},
                        {<<"bar">>, <<"${file(\"bing/bong.txt\")}">>},
                        {<<"foo-bar">>, <<"baz">>}
                      ]}
                     },
                     {"empty.hcl",
                      {[
                        {<<"resource">>, [{[{<<"foo">>, [{[]}]}]}]}
                      ]}
                     },
                     {"escape.hcl",
                      {[
                        {<<"foo">>, <<"bar\"baz">>}
                      ]}
                     },
                     {"multiline_bad.hcl",
                      {[
                        {<<"foo">>, <<"bar\nbaz\n">>}
                      ]}
                     },
                     {"scientific.hcl",
                      {[
                        {<<"a">>, 1.0e-10},
                        {<<"b">>, 1.0e10},
                        {<<"c">>, 1.0e10},
                        {<<"d">>, 1.2e-10},
                        {<<"e">>, 1.2e10},
                        {<<"f">>, 1.2e10}
                      ]}
                     },
                     {"terraform_heroku.hcl",
                      {[
                        {<<"name">>, <<"terraform-test-app">>},
                        {<<"config_vars">>, [{[{<<"FOO">>, <<"bar">>}]}]}
                      ]}
                     },
                     {"structure_multi.hcl",
                      {[
                        {<<"foo">>, [{[{<<"baz">>, [{[{<<"key">>, 7}]}]}]},
                                     {[{<<"bar">>, [{[{<<"key">>, 12}]}]}]}
                                    ]
                        }
                      ]}
                     },
                     {"structure_list.hcl",
                      {[
                        {<<"foo">>, [{[{<<"key">>, 7}]}, {[{<<"key">>, 12}]}]}
                      ]}
                     }
                    ]).

parse_test_() ->
  lists:map(fun({File, Expected}) ->
                Binary = hcl_test:read_fixture(File),
                {ok, Result} = hcl:parse(Binary),
                ?_assertEqual(Expected, Result)
            end, ?TEST_CASES).
