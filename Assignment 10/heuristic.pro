% FILE: heuristic.pro
% TYPE: Prolog Source
% Line: Crypto problem generation and solution using heuristics 
% DATE: December 8, 2015

:- consult('gv.pro').
:- consult('combosets.pro').

% SEGMENT 1: random crypto problem generation and store it in the knowledge base in the form: problem(numbers(N1,N2,N3,N4,N5),goal(G)) (v1 code)
establishCryptoProblemParameters:- declare(lo,0),
declare(hi,15).
:-establishCryptoProblemParameters.
generateRandomCryptoNumber(R):- valueOf(lo,Lo),
valueOf(hi,Hi),
HiPlus1 is Hi + 1, random(Lo,HiPlus1,R).
generateRandomCryptoProblem:- generateRandomCryptoNumber(N1), generateRandomCryptoNumber(N2), generateRandomCryptoNumber(N3), generateRandomCryptoNumber(N4), generateRandomCryptoNumber(N5), generateRandomCryptoNumber(G), addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G).
addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G):- eraseProblem, declare(problem,problem(numbers(N1,N2,N3,N4,N5),goal(G))).
eraseProblem:- undeclare(problem), fail.
eraseProblem.
%display the problem -- assuming that it has been internalized
displayProblem:- valueOf(problem,problem(numbers(N1,N2,N3,N4,N5),goal(G))), write('Problem: numbers ={'),
write(N1), write(','),
write(N2), write(','),
write(N3), write(','),
write(N4), write(','),
write(N5), write('} and goal = '),
write(G), nl.
% SEGMENT 2: The exhaustive crypto problem solver, orders 2, 3, 4 and 5. The processing is done via parameters rather than by means of knowledge base manipulation (v3 code)
crypto(N1,N2,Goal,ex(N1,+,N2)):- Goal is (N1+N2). crypto(N1,N2,Goal,ex(N1,*,N2)):- Goal is (N1*N2). crypto(N1,N2,Goal,ex(N1,-,N2)):- Goal is (N1-N2). crypto(N1,N2,Goal,ex(N2,-,N1)):- Goal is (N2-N1). crypto(N1,N2,Goal,ex(N1,/,N2)):- N2 > 0, Goal is (N1/N2). crypto(N1,N2,Goal,ex(N2,/,N1)):- N1 > 0, Goal is (N2/N1).
crypto(N1,N2,N3,G,Expr):- combos(set(N1,N2,N3),combo(A,B),extras(C)), crypto(A,B,SG,SGE),
crypto(C,SG,G,UGE), substitute(SGE,SG,UGE,Expr).
crypto(N1,N2,N3,N4,G,Expr):- combos(set(N1,N2,N3,N4),combo(A,B),extras(C,D)), crypto(A,B,SG,SGE),
crypto(C,D,SG,G,UGE), substitute(SGE,SG,UGE,Expr).
crypto(N1,N2,N3,N4,N5,G,Expr):- combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)), crypto(A,B,SG,SGE),
crypto(C,D,E,SG,G,UGE),
substitute(SGE,SG,UGE,Expr).
% key substitution code
substitute(New,Old,ex(Old,O,Z),ex(New,O,Z)). substitute(New,Old,ex(X,O,Old),ex(X,O,New)). substitute(New,Old,ex(X,O,Z),ex(Q,O,Z)):- substitute(New,Old,X,Q). substitute(New,Old,ex(X,O,Z),ex(X,O,Q)):- substitute(New,Old,Z,Q).
% SEGMENT 3: code to display the result of solving the problem
displaySolution:- valueOf(solution, solution(S)), displayResult(S),
nl,
eraseSolution.
displaySolution.
displayResult(ex(A,O,B)):-
number(A),number(B),
write('('),write(A),write(' '),write(O),write(' '),write(B),write(')').
displayResult(ex(A,O,B)):-
number(A), B = ex(A1,O1,B1),
write('('),write(A),write(' '),write(O),write(' '),displayResult(ex(A1,O1,B1)),write(')').
displayResult(ex(A,O,B)):-
number(B), A = ex(A1,O1,B1),
write('('),displayResult(ex(A1,O1,B1)),write(' '),write(O),write(' '),write(B),write(')').
displayResult(ex(A,O,B)):-
A = ex(A1,O1,B1), B = ex(A2,O2,B2), write('('),displayResult(ex(A1,O1,B1)),write(' '),write(O),write('
'),displayResult(ex(A2,O2,B2)),write(')').
% SEGMENT 4: code to solve the crypto problem using exhaustive problem decomposition -- assuming the problem has been internalized
solveProblemDecompositionally:-
valueOf(problem, problem(numbers(N1,N2,N3,N4,N5),goal(G))), crypto(N1,N2,N3,N4,N5,G,Expression), recordSolution(Expression).
solveProblemDecompositionally:- write('No solution to this one!'),nl.
recordSolution(Expression):- eraseSolution, declare(solution,solution(Expression)).
eraseSolution:- undeclare(solution), fail.
eraseSolution.
% SEGMENT 5: program to demo the generation and solving of a random crypto problem of order 5 with numbers in the 0..15 range
demo:- generateRandomCryptoProblem, displayProblem, solveProblemHeuristically, displaySolution.
demo(0). demo(N):-
demo,
K is N-1, demo(K).
% SEGMENT 6: program to solve the specific crypto problem of order 5 with numbers in the 0..15 range
solve(numbers(N1,N2,N3,N4,N5),goal(G)):- establishCryptoProblem(numbers(N1,N2,N3,N4,N5),goal(G)), displayProblem,
solveProblemHeuristically,
displaySolution.
establishCryptoProblem(numbers(N1,N2,N3,N4,N5),goal(G)):- addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G).
rule(1,situation1,action1). rule(2,situation2,action2). rule(3,situation3,action3). rule(4,situation4,action4). rule(5,situation5,action5). rule(6,situation6,action6). rule(7,situation7,action7). rule(8,situation8,action8).
solveProblemHeuristically:-
rule(Number,Situation,Action),
write('considering rule '),write(Number),write(' ...'),nl, Situation,
write('application of rule '),write(Number),write(' produces '), Action.
solveProblemHeuristically.
doubleton:- valueOf(problem,problem(numbers(N1,N2,N3,N4,N5),_)), combos(set(N1,N2,N3,N4,N5),combo(A,B),_),
A=B.
doubleton(doubleton(A,B),rest(C,D,E)):- valueOf(problem,problem(numbers(N1,N2,N3,N4,N5),_)), combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)), A=B.
pair([N1,N2,N3,N4,N5],A):- combos(set(N1,N2,N3,N4,N5),combo(A,B),_), A=B.
twopairs([N1,N2,N3,N4,N5],A,C):- combos(set(N1,N2,N3,N4,N5),combo(A,B),_), A=B,
remove(A,[N1,N2,N3,N4,N5],N), remove(B,N,New),
New = [Q1,Q2,Q3], combos(set(Q1,Q2,Q3),combo(C,D),_), C=D.
other_numbers(special(N1),others(N2,N3,N4,N5)):- valueOf(problem,problem(numbers(N1,N2,N3,N4,N5),goal(_))).
other_numbers(special(N2),others(N1,N3,N4,N5)):- valueOf(problem,problem(numbers(N1,N2,N3,N4,N5),goal(_))).
other_numbers(special(N3),others(N1,N2,N4,N5)):- valueOf(problem,problem(numbers(N1,N2,N3,N4,N5),goal(_))).
other_numbers(special(N4),others(N1,N2,N3,N5)):- valueOf(problem,problem(numbers(N1,N2,N3,N4,N5),goal(_))).
other_numbers(special(N5),others(N1,N2,N3,N4)):- valueOf(problem,problem(numbers(N1,N2,N3,N4,N5),goal(_))).
equalGoal([N1,N2,N3,N4,N5],G,A,B):- combos(set(N1,N2,N3,N4,N5),combo(A,B),_), C is A+B,
C=G.
situation1:- valueOf(problem,problem(Numbers,Goal)), Goal = goal(0),
Numbers = numbers(N1,N2,N3,N4,N5), member(0,[N1,N2,N3,N4,N5]).
action1:-
valueOf(problem,problem(Numbers,_)),
Numbers = numbers(N1,N2,N3,N4,N5), declare(solution,solution(ex(N1,*,ex(N2,*,ex(N3,*,ex(N4,*,N5)))))).
situation2:- valueOf(problem,problem(Numbers,Goal)), Goal = goal(G),
Numbers = numbers(N1,N2,N3,N4,N5), member(G,[N1,N2,N3,N4,N5]), member(0,[N1,N2,N3,N4,N5]),
not(G=0).
action2:-
valueOf(problem,problem(_,goal(G))), other_numbers(special(G),others(A,B,C,D)), declare(solution,solution(ex(G,+,ex(A,*,ex(B,*,ex(C,*,D)))))).
situation3:-
valueOf(problem,problem(_,goal(0))), doubleton.
action3:-
doubleton(doubleton(A,B),rest(C,D,E)), declare(solution,solution(ex(ex(A,-,B),*,ex(C,*,ex(D,*,E))))).
situation4:- valueOf(problem,problem(Numbers,Goal)), Goal = goal(G),
Numbers = numbers(N1,N2,N3,N4,N5), member(0,[N1,N2,N3,N4,N5]), equalGoal([N1,N2,N3,N4,N5],G,_,_).
action4:-
valueOf(problem,problem(Numbers,Goal)),
Goal = goal(G),
Numbers = numbers(N1,N2,N3,N4,N5), equalGoal([N1,N2,N3,N4,N5],G,First,Second), remove(First,[N1,N2,N3,N4,N5],T1),
remove(Second,T1,T2),
remove(0,T2,[W1,W2]), declare(solution,solution(ex(ex(First,+,Second),+,ex(0,*,ex(W1,*,W2))))).
situation5:- valueOf(problem,problem(Numbers,Goal)), Goal = goal(N),
Numbers = numbers(N,N,N,N,N).
action5:-
valueOf(problem,problem(Numbers,_)),
Numbers = numbers(N,N,N,N,N), declare(solution,solution(ex(ex(N,*,ex(N,/,N)),*,ex(N,/,N)))).
situation6:- valueOf(problem,problem(Numbers,Goal)), Goal = goal(G),
Numbers = numbers(N1,N2,N3,N4,N5), member(G,[N1,N2,N3,N4,N5]), remove(G,[N1,N2,N3,N4,N5],Rest),
Rest = [0,0,0,0].
action6:- valueOf(problem,problem(Numbers,_)), Numbers = numbers(N1,N2,N3,N4,N5),
max([N1,N2,N3,N4,N5], Max),
remove(Max,[N1,N2,N3,N4,N5],New),
New = [Q1,Q2,Q3,Q4], declare(solution,solution(ex(ex(ex(ex(Max,+,Q1),+,Q2),+,Q3),+,Q4))).
situation7:- valueOf(problem,problem(Numbers,Goal)), Goal = goal(G),
Numbers = numbers(N1,N2,N3,N4,N5), member(G,[N1,N2,N3,N4,N5]), twopairs([N1,N2,N3,N4,N5],_,_).
action7:-
valueOf(problem,problem(Numbers,goal(G))),
Numbers = numbers(N1,N2,N3,N4,N5), twopairs([N1,N2,N3,N4,N5],First,Second), declare(solution,solution(ex(ex(G,*,ex(First,/,First)),*,ex(Second,/,Second)))).
situation8:- valueOf(problem,problem(Numbers,Goal)), Goal = goal(1),
Numbers = numbers(N1,N2,N3,N4,N5), member(0,[N1,N2,N3,N4,N5]), remove(0,[N1,N2,N3,N4,N5],_), pair([N1,N2,N3,N4,N5],_).
action8:-
valueOf(problem,problem(Numbers,_)),
Numbers = numbers(N1,N2,N3,N4,N5), pair([N1,N2,N3,N4,N5],First),
remove(First,[N1,N2,N3,N4,N5],N),
remove(First,N,New),
New = [Q1,Q2,Q3], declare(solution,solution(ex(ex(First,/,First),+,ex(ex(Q1,*,Q2),*,Q3)))).
