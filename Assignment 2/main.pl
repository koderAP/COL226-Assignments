/*Test Cases:
1. hastype(G, and(gret(numeral(5), var(x)), var(y)), boolT).
2. hastype(G, and(gret(numeral(5), var(x)), var(x)), boolT).
3. hastype(G, or(var(x), var(y)), T).
4. hastype([(x, intT)],or(var(x), true), boolT).
5. hastype([(x, intT), (x, boolT)],or(var(x), true), boolT).
    #Note that this is false because its an invalid input.
6. hastype([(x, boolT)],or(var(x), true), boolT).
7. hastype([(x, intT)|R], and(gret(numeral(5), var(x)), var(y)), boolT).

*/

mem(X,[]) :- fail.
%mem(X,[X|R]):- !.
mem((X,Y),[(X,Z)|R]):- !,Y=Z.
mem(X,[_|R]):- mem(X,R).

hastype(G, numeral(N), intT):-  !.
hastype(G, var(X), intT):- mem((X,intT), G),!.
hastype(G, var(X), boolT):- mem((X,boolT), G),!.
hastype(G, var(X), unitT).

hastype(G, add(X,Y), intT):- hastype(G, X, intT), hastype(G, Y, intT),!.
hastype(G, sub(X,Y), intT):- hastype(G, X, intT), hastype(G, Y, intT),!.
hastype(G, mul(X,Y), intT):- hastype(G, X, intT), hastype(G, Y, intT),!.
hastype(G, div(X,Y), intT):- hastype(G, X, intT), hastype(G, Y, intT),!.
hastype(G, gret(X,Y), boolT):- hastype(G, X, intT), hastype(G, Y, intT),!.


hastype(G, true, boolT).
hastype(G, false, boolT):- !.
hastype(G, and(X,Y), boolT):- hastype(G, X, boolT), hastype(G, Y, boolT).
hastype(G, or(X,Y), boolT):- hastype(G, X, boolT), hastype(G, Y, boolT), !.
hastype(G, xor(X,Y), boolT):- hastype(G, X, boolT), hastype(G, Y, boolT), !.
hastype(G, not(X), boolT):- hastype(G,X,boolT).

