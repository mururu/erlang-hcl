-module(hcl_lexer).

-export([lex/1]).

-record(state, {line = 1,
                last_number = false}).

-define(is_digit(C), $0 =< C, C =< $9).
-define(is_letter(C), (65 =< C andalso C =< 90) orelse (97 =< C andalso C =< 122)).
-define(is_space1(C0), C0 == $ ; C0 == $\t; C0 == $\v; C0 == $\f; C0 == $\r).
-define(is_space2(C0, C1), C0 == 16#C2 andalso (C1 == 16#85 orelse C1 == 16#A0)).


lex(Binary) ->
  lex(Binary, [], #state{}).

lex(<<>>, Acc, #state{line = Line}) ->
  lists:reverse([{'$end', Line}|Acc]);
%% newline
lex(<<$\n, Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, Acc, State#state{last_number = false, line = Line+1});
%% whitespace
lex(<<C:8, Rest/binary>>, Acc, State) when ?is_space1(C) ->
  lex(Rest, Acc, State#state{last_number = false});
lex(<<C0:8, C1:8, Rest/binary>>, Acc, State) when ?is_space2(C0, C1) ->
  lex(Rest, Acc, State#state{last_number = false});
%% comment
lex(<<$#, Rest/binary>>, Acc, State) ->
  comment(Rest, Acc, State);
lex(<<$\r, Rest/binary>>, Acc, State) ->
  comment(Rest, Acc, State);
%% number
lex(<<C:8, Rest/binary>>, Acc, State) when ?is_digit(C) ->
  number(<<C:8, Rest/binary>>, Acc, State);
%% e
lex(<<$e, Rest/binary>>, Acc, #state{last_number = true} = State) ->
  e(Rest, Acc, State);
lex(<<$E, Rest/binary>>, Acc, #state{last_number = true} = State) ->
  e(Rest, Acc, State);
%% others
lex(<<$., Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'PERIOD', Line}|Acc], State#state{last_number = false});
lex(<<$-, Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'MINUS', Line}|Acc], State#state{last_number = false});
lex(<<$,, Rest/binary>>, Acc, State) ->
  comma(Rest, Acc, State#state{last_number = false});
lex(<<$=, Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'EQUAL', Line}|Acc], State#state{last_number = false});
lex(<<$[, Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'LEFTBRACKET', Line}|Acc], State#state{last_number = false});
lex(<<$], Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'RIGHTBRACKET', Line}|Acc], State#state{last_number = false});
lex(<<${, Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'LEFTBRACE', Line}|Acc], State#state{last_number = false});
lex(<<$}, Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'RIGHTBRACE', Line}|Acc], State#state{last_number = false});
lex(<<$", Rest/binary>>, Acc, State) ->
  string(Rest, Acc, State#state{last_number = false});
lex(<<$<, Rest/binary>>, Acc, State) ->
  heredoc(Rest, Acc, State#state{last_number = false});
lex(Binary, Acc, State) ->
  id(Binary, Acc, State#state{last_number = false}).

comment(<<$#, Rest/binary>>, Acc, State) ->
  comment_single(Rest, Acc, State);
comment(<<$/, $/, Rest/binary>>, Acc, State) ->
  comment_single(Rest, Acc, State);
comment(<<$/, $*, Rest/binary>>, Acc, State) ->
  comment_multi(Rest, 1, Acc, State).

comment_single(<<>>, Acc, State) ->
  lex(<<>>, Acc, State);
comment_single(<<$\n, Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, Acc, State#state{line = Line+1});
comment_single(<<_:8, Rest/binary>>, Acc, State) ->
  comment_single(Rest, Acc, State).

comment_multi(<<>>, _Nested, Acc, State) ->
  lex(<<>>, Acc, State);
comment_multi(Binary, 0, Acc, State) ->
  lex(Binary, Acc, State);
comment_multi(<<$/, $*, Rest/binary>>, Nested, Acc, State) ->
  comment_multi(Rest, Nested+1, Acc, State);
comment_multi(<<$*, $/, Rest/binary>>, Nested, Acc, State) ->
  comment_multi(Rest, Nested+1, Acc, State);
comment_multi(<<$\n, Rest/binary>>, Nested, Acc, #state{line = Line} = State) ->
  comment_multi(Rest, Nested, Acc, State#state{line = Line+1});
comment_multi(<<_:8, Rest/binary>>, Nested, Acc, State) ->
  comment_multi(Rest, Nested, Acc, State).

%% TODO: fix line
comma(<<$\n, Rest/binary>>, Acc, #state{line = Line} = State) ->
  comma(Rest, Acc, State#state{line = Line+1});
comma(<<C:8, Rest/binary>>, Acc, State) when ?is_space1(C) ->
  comma(Rest, Acc, State);
comma(<<C0:8, C1:8, Rest/binary>>, Acc, State) when ?is_space2(C0, C1) ->
  comma(Rest, Acc, State);
comma(<<$], Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'COMMAEND', Line}|Acc], State);
comma(Binary, Acc, #state{line = Line} = State) ->
  lex(Binary, [{'COMMA', Line}|Acc], State).

%% TODO: utf8
id(Binary, Acc, State) ->
  id(Binary, <<>>, Acc, State).

id(<<C:8, Rest/binary>>, SubAcc, Acc, State) when ?is_letter(C) ->
  id0(Rest, <<SubAcc/binary, C:8>>, Acc, State);
id(<<$_, Rest/binary>>, SubAcc, Acc, State) ->
  id0(Rest, <<SubAcc/binary, $_>>, Acc, State).

id0(<<C:8, Rest/binary>>, SubAcc, Acc, State) when ?is_digit(C) ->
  id0(Rest, <<SubAcc/binary, C:8>>, Acc, State);
id0(<<C:8, Rest/binary>>, SubAcc, Acc, State) when ?is_letter(C) ->
  id0(Rest, <<SubAcc/binary, C:8>>, Acc, State);
id0(<<$_, Rest/binary>>, SubAcc, Acc, State) ->
  id0(Rest, <<SubAcc/binary, $_>>, Acc, State);
id0(<<$-, Rest/binary>>, SubAcc, Acc, State) ->
  id0(Rest, <<SubAcc/binary, $->>, Acc, State);
id0(Binary, <<"true">>, Acc, #state{line = Line} = State) ->
  lex(Binary, [{'BOOL', Line, true}|Acc], State);
id0(Binary, <<"false">>, Acc, #state{line = Line} = State) ->
  lex(Binary, [{'BOOL', Line, true}|Acc], State);
id0(Binary, SubAcc, Acc, #state{line = Line} = State) ->
  lex(Binary, [{'IDENTIFIER', Line, SubAcc}|Acc], State).

heredoc(<<$<, Rest/binary>>, Acc, State) ->
  heredoc0(Rest, <<>>, Acc, State).

heredoc0(<<>>, _SubAcc, Acc, State) ->
  lex(<<>>, Acc, State);
heredoc0(<<$\n, _Rest/binary>>, <<>>, _Acc, _State) ->
  exit(heredoc_must_have_a_marker);
heredoc0(<<$\n, Rest/binary>>, SubAcc, Acc, State) ->
  heredoc1(Rest, SubAcc, byte_size(SubAcc), <<>>, Acc, State);
heredoc0(<<C:8, Rest/binary>>, SubAcc, Acc, State) ->
  heredoc0(Rest, <<SubAcc/binary, C:8>>, Acc, State).

heredoc1(Binary, Marker, MarkerLen, SubAcc, Acc, #state{line = Line} = State) ->
  case Binary of
    <<Marker:MarkerLen/binary, Rest/binary>> ->
      lex(Rest, [{'STRING', Line, SubAcc}|Acc], State);
    _ ->
      heredoc2(Binary, Marker, MarkerLen, SubAcc, Acc, State)
  end.

heredoc2(<<>>, _Marker, _MarkerLen, _SubAcc, Acc, State) ->
  lex(<<>>, Acc, State);
heredoc2(<<$\n, Rest/binary>>, Marker, MarkerLen, SubAcc, Acc, #state{line = Line} = State) ->
  heredoc1(Rest, Marker, MarkerLen, <<SubAcc/binary, $\n>>, Acc, State#state{line = Line+1});
heredoc2(<<C:8, Rest/binary>>, Marker, MarkerLen, SubAcc, Acc, State) ->
  heredoc2(Rest, Marker, MarkerLen, <<SubAcc/binary, C:8>>, Acc, State).


%% TODO: fix 1.02 -> 1.2
number(Binary, Acc, State) ->
  number(Binary, [], Acc, State#state{last_number = true}).

number(<<>>, SubAcc, Acc, #state{line = Line} = State) ->
  Integer = list_to_integer(lists:reverse(SubAcc)),
  lex(<<>>, [{'NUMBER', Line, Integer}|Acc], State);
number(<<C:8, Rest/binary>>, SubAcc, Acc, State) when ?is_digit(C) ->
  number(Rest, [C|SubAcc], Acc, State);
number(Binary, SubAcc, Acc, #state{line = Line} = State) ->
  Integer = list_to_integer(lists:reverse(SubAcc)),
  lex(Binary, [{'NUMBER', Line, Integer}|Acc], State).

e(<<$+, Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'EPLUS', Line}|Acc], State);
e(<<$-, Rest/binary>>, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'EMINUS', Line}|Acc], State);
e(Binary, Acc, #state{line = Line} = State) ->
  lex(Binary, [{'EPLUS', Line}|Acc], State).

%% TODO: utf8
string(Binary, Acc, State) ->
  string(Binary, <<>>, 0, Acc, State).

string(<<>>, SubAcc, _Brace, Acc, #state{line = Line} = State) ->
  lex(<<>>, [{'STRING', Line, SubAcc}|Acc], State);
string(<<$", Rest/binary>>, SubAcc, 0, Acc, #state{line = Line} = State) ->
  lex(Rest, [{'STRING', Line, SubAcc}|Acc], State);
string(<<$\n, _/binary>>, _SubAcc, _Brace, _Acc, _State) ->
  exit(newline_before_string_closed);
string(<<$\\, $", Rest/binary>>, SubAcc, Brace, Acc, State) ->
  string(<<Rest/binary>>, <<SubAcc/binary, $">>, Brace, Acc, State);
string(<<$\\, $\n, Rest/binary>>, SubAcc, Brace, Acc, State) ->
  string(<<Rest/binary>>, <<SubAcc/binary, $\n>>, Brace, Acc, State);
string(<<$\\, Rest/binary>>, SubAcc, Brace, Acc, State) ->
  string(<<Rest/binary>>, <<SubAcc/binary, $\\>>, Brace, Acc, State);
string(<<$$, ${, Rest/binary>>, SubAcc, 0, Acc, State) ->
  string(Rest, <<SubAcc/binary, $$, ${>>, 1, Acc, State);
string(<<${, Rest/binary>>, SubAcc, Brace, Acc, State) when Brace > 0 ->
  string(Rest, <<SubAcc/binary, ${>>, Brace+1, Acc, State);
string(<<$}, Rest/binary>>, SubAcc, Brace, Acc, State) when Brace > 0 ->
  string(Rest, <<SubAcc/binary, $}>>, Brace-1, Acc, State);
string(<<C:8, Rest/binary>>, SubAcc, Brace, Acc, State) ->
  string(Rest, <<SubAcc/binary, C:8>>, Brace, Acc, State).
