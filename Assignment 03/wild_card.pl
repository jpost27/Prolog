%% file proglang.pl
%% line: some knowledge about sports

% sport(S) means S is a sport

sport(football).
sport(baseball).
sport(basketball).

% rule(S,P,A) means sport S requires P players
% and is played on surface A

rule(football,players(11),field).
rule(baseball,players(9),field).
rule(basketball,players(5),court).


% history(L,I,D) means language L was invented by I in city C during the
% year D

history(football,inventor('Walter Camp'),'New Haven, CT',date(1869)).
history(baseball,inventor('Alexander Joy Cartwright'),'New York City, NY',date(1845)).
history(basketball,inventor('James Naismith'),'Springfield, MA',date(1891)).
