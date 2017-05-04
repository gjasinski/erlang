%%%-------------------------------------------------------------------
%%% @author gjasinski
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. mar 2017 12:37
%%%-------------------------------------------------------------------
-module(onp).
-author("gjasinski").

%% API
-export([ onp/1,
          calculate_expression/2,
          convert_list/1
]).

onp([]) -> 0;
onp(Expression) -> calculate_expression([], convert_list(string:tokens(Expression, " "))).

convert_list([]) -> [];
convert_list([Head|Tail]) when (Head == "+")  or (Head == "-") or (Head == "*") or (Head == "/") or (Head == "pow") or
  (Head == "sqrt")  or (Head == "sin") or (Head == "cos") or (Head == "tan") ->
    [Head] ++ convert_list(Tail);
convert_list([Head|Tail]) -> [list_to_float(Head)] ++ convert_list(Tail).

calculate_expression([],[]) -> 0;
calculate_expression([ValA, ValB | Tail], ["+" | Rest]) -> calculate_expression([ValB +  ValA] ++ Tail, Rest);
calculate_expression([ValA, ValB | Tail], ["-" | Rest]) -> calculate_expression([ValB -  ValA] ++ Tail, Rest);
calculate_expression([ValA, ValB | Tail], ["*" | Rest]) -> calculate_expression([ValB *  ValA] ++ Tail, Rest);
calculate_expression([ValA, ValB | Tail], ["/" | Rest]) -> calculate_expression([ValB /  ValA] ++ Tail, Rest);
calculate_expression([ValA, ValB | Tail], ["pow" | Rest]) -> calculate_expression([math:pow(ValB, ValA)] ++ Tail, Rest);
calculate_expression([ValA | Tail], ["sqrt" | Rest]) -> calculate_expression([math:sqrt(ValA)] ++ Tail, Rest);
calculate_expression([ValA | Tail], ["sin" | Rest]) -> calculate_expression([math:sin(ValA)] ++ Tail, Rest);
calculate_expression([ValA | Tail], ["cos" | Rest]) -> calculate_expression([math:cos(ValA)] ++ Tail, Rest);
calculate_expression([ValA | Tail], ["tan" | Rest]) -> calculate_expression([math:tan(ValA)] ++ Tail, Rest);
calculate_expression(Stack, [Number | Rest]) -> calculate_expression([Number] ++ Stack, Rest);
calculate_expression([Stack], []) -> Stack.


% 1 + 2 * 3 - 4 / 5 + 6 ==> 2 3 * 1 + 4 5 / - 6 +
% 1 + 2 + 3 + 4 + 5 + 6 * 7 ==> 1 2 3 4 5 6 7 * + + + + +
% ( (4 + 7) / 3 ) * (2 - 19) ==> 4 7 + 3 / 2 19 - *
% 17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1 ==> 17 31 4 + * 26 15 - 2 * 22 - / 1 - ##!! DIV0!!!
