%There are four constraints for the math puzzles:
%1.All the digonals should be same value.
%2.The heading of row holds either the sum or the product of all the digits in that row.
%3.The heading of column holds either the sum or the product of all the digits in that column.
%4.Each row and column contains no repeated digits (from 1 to 9).

:- ensure_loaded(library(clpfd)).

puzzle_solution([Headings|Tails]):- %follow these 4 constraints above.
	constraint_digonal(Tails,1,1,_),
	constraint_column([Headings|Tails]),
	constraint_row(Tails),
	%write(Puzzle).
	maplist(labeling([]),[Headings|Tails]). %Show the puzzle.

constraint_digonal([],_,_,_).  
constraint_digonal([Hs|Ts],Row,Index,Digonal):- %Check whether the digonal is valid solution(except the first row).
							   %The first argument is the remaining rows except the first row.
							   %The second means which row the function is operating on. 
							   %The third is the index of the digonal.
							   %The fourth is the value of digonal.
	digonal_value(Hs,Row,Index,Digonal), %Take the first row of rows.
	Row1 is Row + 1,
	Index1 is Row1,
	constraint_digonal(Ts,Row1,Index1,Digonal).

digonal_value([Head|_],_,0,Head). %Find the value of digonal.
digonal_value([_|Tail],_,Index,Value):-
	Index0 is Index - 1, %Have to subtract 1 when first time since it is the heading. 
	digonal_value(Tail,_,Index0,Value).
	  
constraint_column([]).
constraint_column([Hs|Ts]):- %Transpose columns to rows.
	transpose([Hs|Ts],[_|Rowts]), %Then operate on the remaining rows except the first row.
	constraint_row(Rowts).

constraint_row([]).
constraint_row([Hs|Ts]):- %Operate on the a row of the remaining rows.
	single_row(Hs),
	%maplist(all_distinct,Hs), 
	all_distinct(Hs), %Check whether solutions in the Hs are distinct.
	constraint_row(Ts).

single_row([Value|Tail]):- %Check whether the sum or product of elements in the tail equals to the value.
	sum_row(Value,Tail);
	product_row(Value,Tail).

sum_row(0,[]).
sum_row(Value,[Head|Tail]):- %Sum the elements in the row and leave the result in the value.
	sum_row(Value1,Tail),
	%maplist(Head,Vs), %Vs ins 1..9,
	Head #>= 1, Head #=< 9,
	%Value is Value1 + Head.
	Value #= Value1 + Head.

product_row(1,[]).
product_row(Value,[Head|Tail]):- %Product the elements in the row and leave the result in the value.
	product_row(Value1,Tail),
	Head #>= 1, Head #=< 9,
	%Value is Value1 * Head.
	Value #= Value1 * Head.



