%% file proglang.pro
%% line: basic prolog knowledge

%language(X) means X is a language
language(smalltalk).
language(lisp).
language(prolog).

%essence(X,Y,Z) means language X has featured data type Y
%and the featured mechanism of computation is Z
essence(smalltalk,objects,'message passing').
essence(lisp,lists,'recursive functions').
essence(prolog,relations,'logical inferencing').

%history(X,Y,Z) means language X was invented by Y in year Z
history(smalltalk,inventor('Alan Kay'),date(1980)).
history(lisp,inventor('John McCarthy'),date(1959)).
history(prolog,inventor('Alan Colmeraur'),date(1971)).
