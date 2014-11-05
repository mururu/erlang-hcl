Nonterminals
  top
  int
  list listitems objectlist
  block number object objectitem
  listitem
  blockId exp frac.

Terminals
  BOOL
  NUMBER
  COMMA COMMAEND IDENTIFIER EQUAL NEWLINE STRING MINUS
  LEFTBRACE RIGHTBRACE LEFTBRACKET RIGHTBRACKET PERIOD
  EPLUS EMINUS.

Rootsymbol
  top.

%% handle blank
top -> objectlist : object_flat({reverse('$1')}).

objectlist -> objectitem : ['$1'].
objectlist -> objectlist objectitem : ['$2'|'$1'].

object -> LEFTBRACE objectlist RIGHTBRACE : [object_flat({reverse('$2')})].
object -> LEFTBRACE RIGHTBRACE : [{[]}].

objectitem -> IDENTIFIER EQUAL number : {value('$1'), '$3'}.
objectitem -> IDENTIFIER EQUAL BOOL : {value('$1'), value('$3')}.
objectitem -> IDENTIFIER EQUAL STRING : {value('$1'), value('$3')}.
objectitem -> IDENTIFIER EQUAL object : {value('$1'), '$3'}.
objectitem -> IDENTIFIER EQUAL list : {value('$1'), '$3'}.
objectitem -> block : '$1'.

block -> blockId object : {'$1', '$2'}.
block -> blockId block : {'$1', [{['$2']}]}.

blockId -> IDENTIFIER : value('$1').
blockId -> STRING : value('$1').

list -> LEFTBRACKET listitems RIGHTBRACKET : reverse('$2').
list -> LEFTBRACKET RIGHTBRACKET : [].

listitems -> listitem : ['$1'].
listitems -> listitems COMMA listitem : ['$3'|'$1'].
listitems -> listitems COMMAEND : '$1'.

listitem -> number : '$1'.
listitem -> STRING : value('$1').

%% TODO: dont use list_to_float
number -> int : '$1'.
number -> int frac : list_to_float(integer_to_list('$1') ++ "." ++ '$2').
number -> int exp : list_to_float(integer_to_list('$1') ++ ".0" ++ '$2').
number -> int frac exp: list_to_float(integer_to_list('$1') ++ "." ++ '$2' ++ '$3').

int -> MINUS int : '$2' * -1.
int -> NUMBER : value('$1').

exp -> EPLUS NUMBER : "e" ++ integer_to_list(value('$2')).
exp -> EMINUS NUMBER : "e-" ++ integer_to_list(value('$2')).

frac -> PERIOD NUMBER : integer_to_list(value('$2')).


Erlang code.

value(Token) -> element(3, Token).

reverse(List) -> lists:reverse(List).

object_flat({ObjectList}) ->
  F = fun
        ({Key, [Tuple|_]}, {ObjectKeys, OtherKeys}) when is_tuple(Tuple) ->
          case sets:is_element(Key, OtherKeys) of
            false ->
              {sets:add_element(Key, ObjectKeys), OtherKeys};
            true ->
              error({duplicate_key_for_different_type_value, Key})
          end;
        ({Key, _}, {ObjectKeys, OtherKeys}) ->
          case sets:is_element(Key, ObjectKeys) of
            false ->
              {ObjectKeys, sets:add_element(Key, OtherKeys)};
            true ->
              error({duplicate_key_for_different_type_value, Key})
          end
      end,
  {ObjectKeys, _} = lists:foldl(F, {sets:new(), sets:new()}, ObjectList),
  object_flat(ObjectList, ObjectKeys, sets:new(), []).

object_flat([], ObjectKeys, _KeyAcc, Acc) ->
  F = fun({Key, Value}) ->
        case sets:is_element(Key, ObjectKeys) of
          true ->
            {Key, lists:reverse(Value)};
          false ->
            {Key, Value}
        end
      end,
  Acc2 = lists:map(F, Acc),
  {lists:reverse(Acc2)};
object_flat([{Key, Value}|Rest], ObjectKeys, KeyAcc, Acc) ->
  case sets:is_element(Key, KeyAcc) of
    true ->
      case sets:is_element(Key, ObjectKeys) of
        true ->
          F = fun
                ({AKey, AValue}) when Key =:= AKey->
                  {Key, Value ++ AValue};
                (Other) ->
                  Other
              end,
          Acc2 = lists:map(F, Acc),
          object_flat(Rest, ObjectKeys, KeyAcc, Acc2);
        false ->
          object_flat(Rest, ObjectKeys, KeyAcc, Acc)
      end;
    false ->
      case sets:is_element(Key, ObjectKeys) of
        true ->
          object_flat(Rest, ObjectKeys, sets:add_element(Key, KeyAcc), [{Key, Value}|Acc]);
        false ->
          object_flat(Rest, ObjectKeys, sets:add_element(Key, KeyAcc), [{Key, Value}|Acc])
      end
  end.
