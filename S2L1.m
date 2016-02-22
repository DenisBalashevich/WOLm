(* ::Package:: *)

SetDirectory[NotebookDirectory[]]
Import["Functions.m"]


(*Data*)
k=1;
A=Partition[{622,142,-172,191,144,533,111,-182,-172,111,524,142,191,-182,142,655},4]/100+k*IdentityMatrix[4]//N;
b={753,606,805,806}/100//N;
Ab=App[A,b];
AbS=App[Ab,S[Ab]];


(*Result*)
AbS1=Join[AbS,ShowGen[AbS,3]];
AbS2=AbS1[[{5,9,12,13},1;;-2]];
X=Res[AbS2]//Reverse;
x=T[X][[1]];
Join[AbS1,Join[Reverse[IdentityMatrix[4]],X,2]]//G
RowReduce[AbS]//G


r=A.x-b
Norm[r,#]&/@{1,2,Infinity}


Det[A]
Norm[A,#]&/@{1,Infinity}



