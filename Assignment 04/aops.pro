% FILE: gv.pro
% TYPE: Prolog source
% LINE: very simple global vairable ADT
% DATE: September 25, 2015

:- consult('gv.pro').

add(Variable1,Variable2,Sum) :-
  valueOf(Variable1,Value1),
  valueOf(Variable2,Value2),
  Sum is Value1 + Value2.

sub(Variable1,Variable2,Diff) :-
  valueOf(Variable1,Value1),
  valueOf(Variable2,Value2),
  Diff is Value1 - Value2.

mul(Variable1,Variable2,Prod) :-
  valueOf(Variable1,Value1),
  valueOf(Variable2,Value2),
  Prod is Value1 * Value2.

div(Variable1,Variable2,Quo) :-
  valueOf(Variable1,Value1),
  valueOf(Variable2,Value2),
  Quo is Value1 / Value2.

pow(Variable1,Variable2,Exp) :-
  valueOf(Variable1,Value1),
  valueOf(Variable2,Value2),
  Exp is Value1 ^ Value2.
