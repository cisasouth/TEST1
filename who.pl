

n --> [X], { noun(X) }.

noun(dog).
noun(cat).

noun(N) :-
    name(N, Nchars),
    name(ness, Nn),
    append(Achars, Nn, Nchars),
    name(A, Achars),
    adjective(A).

adjective(flat).
adjective(green).
adjective(blue).

% ?- s(L,[who, did, max, see, thought, fido],[]).
% L =  (max, see, who, thought, fido) .

/*
s((NP,VP)) --> [who, did], np(NP), vp(VP).
s((NP,VP)) --> np(NP), vp(VP).

np(N) --> n(N).
np((who, V, NP)) --> v(V), np(NP).
np((who, V)) --> v(V).

vp((V,N))--> v(V), np(N).
vp((V,S)) --> v(V), s(S).
vp(V) --> v(V). 

n(max) --> [max].
n(joe) --> [joe].
n(bill) --> [bill].
n(fido) --> [fido].

v(saw) --> [saw].
v(said) --> [said].
v(thought) --> [thought].
v(believed) --> [believed].
v(barked) --> [barked].

v(see) --> [see].
v(say) --> [say].
v(think) --> [think].
v(believe) --> [believe].
v(bark) --> [bark].
*/

/*
% undoing syntactic movement
s(In, Out) --> [who,did], np(In, Out1), vp(Out1, Out).
s(In, Out) --> np(In, Out1), vp(Out1, Out).
np([who|Out], Out) --> [].
np(X,Y) --> n(X,Y).

vp(X,Y) --> v(X,Y).
vp(In, Out) --> v(In, Out1), np(Out1, Out).
vp(In, Out) --> v(In, Out1), s(Out1, Out).

n([max|X],X) --> [max].
n([joe|X],X) --> [joe].
n([bill|X],X) --> [bill].
n([fido|X],X) --> [fido].

v([saw|X], X) --> [saw].
v([said|X],X) --> [said].
v([thought|X],X) --> [thought].
v([believed|X], X) --> [believed].
v([barked|X], X) --> [barked].
v([see|X], X) --> [see]. 
v([say|X], X)--> [say].
v([think|X], X) --> [think].
v([believed|X], X) --> [believe]. 
v([bark|X], X) --> [bark].
*/

/*
s(In, Out) --> [who,did], np([who|In], Out1), vp(Out1, Out).
s(In, Out) --> np(In, Out1), vp(Out1, Out).
np([who|Out], Out) --> [].
np(X,X) --> [max]; [joe]; [bill]; [fido].

vp(X,X) --> v.
vp(In, Out) --> v, np(In, Out).
vp(In, Out) --> v, s(In, Out).

v --> [saw];[said];[thought];[believed];[barked].
v --> [see]; [say]; [think]; [believe]; [bark].
*/

/*
%  ?- s([who,did,max,see],[]).
%  ?- s([max,see,who,thought,fido],[]).

s --> [who,did], np, vp.
s --> np, vp.
np --> [who],vp.
np --> n.
n --> [max]; [joe]; [bill]; [fido].

vp --> v.
vp --> v, np.
vp --> v, s.

v --> [saw];[said];[thought];[believed];[barked].
v --> [see]; [say]; [think]; [believe]; [bark].
*/


/*

s --> np, vp.

np --> n.
np --> [who], vp.
n --> [max]; [joe]; [bill]; [fido].

v --> [saw];[said];[thought];[believed];[barked].
v --> [see]; [say]; [think]; [believe]; [bark].
vp --> v.
vp --> v, np.
vp --> v, s.
*/

% ?- s([max,say,joe, believed, fido, barked],[]).
% ?- s([max,say,joe, thought, who, believed, fido, barked],[]).

/*
% building syntactic trees
s(s(NP,VP)) --> np(NP), vp(VP).
np(np(D,N)) --> d(D), n(N).
vp(vp(V,NP)) --> v(V), np(NP).
d(d(the)) --> [the].
n(n(dog)) --> [dog].
n(n(cat)) --> [cat].
v(v(chased)) --> [chased].
v(v(saw)) --> [saw].
*/

/*
% agreement
s --> np(Number), vp(Number).
np(Number) --> d, n(Number).
vp(Number) --> v(Number), np(Number).
d --> [the].
n(single) --> [dog].
n(plural) --> [dogs].
n(single) --> [cat].
n(plural) --> [cats].
v(single) --> [chases].
v(plural) --> [chase].
v(single) --> [sees].
v(plural) --> [see].
*/

/*
s(s(Number,NP,VP)) --> np(Number,NP), vp(Number,VP).
np(Number, np(D,N)) --> d(D), n(Number, N).
vp(Number, vp(V, NP)) --> v(Number,V), np(Number, NP).
d(the) --> [the].
n(single, n(dog)) --> [dog].
n(plural, n(dogs)) --> [dogs].
n(single, n(cat)) --> [cat].
n(plural, n(cats)) --> [cats].
v(single, v(chases)) --> [chases].
v(plural, v(chases)) --> [chase].
v(single, v(sees)) --> [sees].
v(plural, v(see)) --> [see].
*/

/*
% subcategorization
s --> np, vp.
np --> d, n.

d --> [the].
n --> [girl].
n --> [dog].
n --> [cat].

vp --> v(1).
vp --> v(2), np.
vp --> v(3), np, np.
vp --> v(4), s.

v(1) --> [barked]; [slept].
v(2) --> [chased]; [saw].
v(3) --> [gave]; [sold].
v(4) --> [said]; [thought].
*/