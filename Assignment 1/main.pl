
/*It checks if (X,Y) belongs to Reflexive Symmetric Transitive closure of relation R, over S*S or not
Test case : mem((1,5),refsymtranclos([(1,2),(2,3),(3,4),(4,5)],[1,2,3,4,5])). returns true
			mem((2,3),refsymtranclos([(1,2),(2,3),(3,4),(4,5)],[1,2,3,4,5])). returns true
            mem((2,3),refsymtranclos([(1,2),(2,3),(4,5), (5,2)],[1,2,3,4,5])). returns true
            mem((2,7),refsymtranclos([(1,2),(2,3),(4,5), (5,2)],[1,2,3,4,5,6,7])). returns false
            mem((2,7),refsymtranclos([(1,2),(2,3),(4,5),(1,7), (5,2)],[1,2,3,4,5,6,7])). returns true*/

mem((X,Y),refsymtranclos(L,S)):- mem((X,Y),L),!.
mem((X,X),refsymtranclos(L,S)):- mem(X,S), !.
mem((X,Y),refsymtranclos(L,S)):- mem((X,Y),inv(L)),!.
mem((X,Y),refsymtranclos(L,S)):- mem((Z,X),L),diffI(L,[(X,_)],L1),mem((Z,Y),refsymtranclos(L1,S)).
mem((X,Y),refsymtranclos(L,S)):- mem((Z,Y),L),diffI(L,[(Y,_)],L1),mem((Z,X),refsymtranclos(L1,S)).



/*It checks if (X,Y) belongs to Reflexive Transitive closure of relation R, over S*S or not
Test case : mem((1,5),reftranclos([(1,2),(2,3),(3,4),(4,5)],[1,2,3,4,5])). returns true
			mem((2,3),reftranclos([(1,2),(2,3),(3,4),(4,5)],[1,2,3,4,5])). returns true
            mem((3,2),reftranclos([(1,2),(2,3),(4,5), (5,2)],[1,2,3,4,5])). returns false
            mem((2,7),reftranclos([(1,2),(2,3),(4,5), (5,2)],[1,2,3,4,5,6,7])). returns false
            mem((2,7),reftranclos([(1,2),(2,3),(4,5),(1,7), (5,2)],[1,2,3,4,5,6,7])). returns false*/
mem((X,Y),reftranclos(L,S)):- mem((X,Y),L),!.
mem((X,X),reftranclos(L,S)):- mem(X,S), !.
mem((X,Y),reftranclos(L,S)):- mem((X,Z),L),diffI(L,[(X,_)],L1),mem((Z,Y),reftranclos(L1,S)).


/*------------------------------------------------------------------------------------------------------------------------*/




/*chkpoweq compares power sets of two given sets*/
/*Test case : chkpoweq([1,2,3],[2,3,1]), returns true
             chkpoweq([1,2,3],[1,2,3,4]), returns false
             chkpoweq([1,2,3,4],[1,3,2,4]), returns true
             
This method can be used to prove that the powersets obtained 
from the implementation of two different valid representations 
of a set (elements given in different order) are equal. */
chkpoweq(L1,L2):- powerI(L1,P1), powerI(L2,P2), matchset(P1,P2),matchset(P2,P1),!.


/*Compares Two Power Set*/
p_set_eq(P1,P2):- matchset(P1,P2), matchset(P2,P1),!.

/*Checks if a set is member of set of sets, used to check the equality of power set*/
eqsets([],[]):- !.
eqsets([X],[]):- fail.
eqsets([X],[Y|R]):- eqset(X,Y), !.
eqsets([X],[Y|R]):- eqsets([X],R).


/*Checks if a {set of sets} is subset of {set of sets}, used to check the equality of power set*/
matchset([],P):- !.
matchset([X|R],P):- eqsets([X],P), matchset(R,P).

/*Gives a list, which contain all (X,Y) terms, where Y belongs to a list*/
prod(X, [ ], [ ]) :- !.
prod(X, [Y|R], [(X,Y)| Z ]) :- prod(X, R, Z).


/*Checks if a list has duploicate elements
Test Case : nodup([1,2,6,4,9]). retruns true
			nodup([1,2,6,4,4]). returns false
This will be used to check duplicates in UnionI, 
Test Case : unionI([1,2,3,4],[1,2,4,6], L), nodup(L).
			unionI([1,2,3,4,6,7],[1,2,4,6,7,3], L), nodup(L).
*/
nodup([]):- !.
nodup([X|R]):- \+mem(X,R), nodup(R).


/*Calculates cartesion product of two sets
Test case : 
1. cartesionI([1,2,3],[4,5,6],L).
   L = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
2. cartesionI([1,2,3],[1,2,3],L).
	L = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
3. cartesionI([1,2,3],[],L).
	L = []
*/
cartesionI([],S,[]):- !.
cartesionI(S,[],[]):- !.
cartesionI([X|R],S,P):- cartesionI(R,S,P1),prod(X,S,P2), append(P2,P1,P).

/*Calculates Power Set of any set
Test Cases:
1. powerI([],L).
	L = [[]]
2. powerI([1],L).
	L = [[1],[]]
3. powerI([1,2],L).
	L = [[1,2],[1],[2],[]]
4. powerI([1,2,3],L).
	L = [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
*/
powerI([ ], [ [ ] ]) :- !.
powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).

/*Calculates Intersection of two sets
Test Cases:
1.interI([1,2,3],[1,5,9,2],L).
	L = [1,2]
2.interI([1,2,3,9],[1,5,9,2],L).
	l = [1,2,9]
*/
interI([],_,[]):- !.
interI([X|L],L1,[X|L2]):- mem(X,L1), interI(L,L1,L2),!.
interI([X|L],L1,L2):- \+ mem(X,L1), interI(L,L1,L2),!.


/*Implements Set difference
Test Cases:
1.diffI([1,2,3,9],[1,5,9,2],L).
	L = [3]
2. diffI([1,2,3,9],[1,2],L).
	L = [3,9]
3. diffI([1,2,3,9],[],L).
	 L = [1,2,3,9]
*/	
diffI([],_,[]):-!.
diffI(S1,[],S1):-!.
diffI([X|S1],S2,S3):-mem(X,S2),diffI(S1,S2,S3),!.
diffI([X|S1],S2,[X|S3]):- \+mem(X,S2), diffI(S1,S2,S3),!.

/*Membership operations*/
mem((Y,X),inv(R)):- mem((X,Y),R).
mem(X,[]) :- fail.
mem(X,[X|_]) :- !.
mem(X,[_|R]) :- mem(X,R).

subset([],S) :-!.
subset([X|R],S):- mem(X,S),subset(R,S).

eqset(L1,L2) :- subset(L1,L2),subset(L2,L1).

del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.


remdups([ ], [ ]) :- !.
remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).

/*Implements Set Union*/
unionI([ ], S2, S2) :- !.
unionI(S1, [ ], S1) :- !.
unionI([X|R], S2, [X|Z]) :- diffI( S2,[X], S3),  unionI(R, S3, Z).


append( [ ], L, L).
append( [X|R], L, [X|Z]) :- append(R, L, Z).


mapcons(X, [ ], [ ]) :- !.
mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).