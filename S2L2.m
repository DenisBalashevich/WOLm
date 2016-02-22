(* ::Package:: *)

SetDirectory[NotebookDirectory[]]
Import["Functions.m"]


(*Data*)
k=1;
A=Partition[{622,142,-172,191,144,533,111,-182,-172,111,524,142,191,-182,142,655},4]/100+k*IdentityMatrix[4]//N;
b={1,0,0,0}//N;
Ab=App[A,b];
AbS=App[Ab,S[Ab]];


(*Result*)
AbS1=Join[AbS,ShowGen[AbS,3]];
AbS2=AbS1[[{5,9,12,13},1;;-2]];
X=Res[AbS2]//Reverse;
x=T[X][[1]];
rx={x};
Join[AbS1,Join[Reverse[IdentityMatrix[4]],X,2]]//G
RowReduce[Join[A,IdentityMatrix[4],2]]//G


b={0,1,0,0}//N;
Ab=App[A,b];
AbS=App[Ab,S[Ab]];
AbS1=Join[AbS,ShowGen[AbS,3]];
AbS2=AbS1[[{5,9,12,13},1;;-2]];
X=Res[AbS2]//Reverse;
x=T[X][[1]];
rx=Join[rx,{x}];
b={0,0,1,0}//N;
Ab=App[A,b];
AbS=App[Ab,S[Ab]];
AbS1=Join[AbS,ShowGen[AbS,3]];
AbS2=AbS1[[{5,9,12,13},1;;-2]];
X=Res[AbS2]//Reverse;
x=T[X][[1]];
rx=Join[rx,{x}];
b={0,0,0,1}//N;
Ab=App[A,b];
AbS=App[Ab,S[Ab]];
AbS1=Join[AbS,ShowGen[AbS,3]];
AbS2=AbS1[[{5,9,12,13},1;;-2]];
X=Res[AbS2]//Reverse;
x=T[X][[1]];
rx=Join[rx,{x}];


(rx=T[rx])//G


A.rx//G


(Norm[A,#]*Norm[rx,#])&/@{1,Infinity}
