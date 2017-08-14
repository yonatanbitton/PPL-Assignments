not_member(_,[]).
not_member(X,[Y|T]) :-
	X \= Y,
	not_member(X,T).

noDups(List, Set) :-
	noDupsH(List,[],Set).
	% 	  Origin,Acc,Final_ans

noDupsH([],Acc,Acc).
% At base case, transfer acc to final_ans

noDupsH([H|T],Acc,Set) :-
	member(H,Acc),
	noDupsH(T,Acc,Set).
	%Else - , only if H isn't a member

noDupsH([H|T],Acc,Set) :-
	%NewAcc = [H|Acc],
	not_member(H,Acc),
	append(Acc,[H],NewAcc),
	noDupsH(T,NewAcc,Set).
