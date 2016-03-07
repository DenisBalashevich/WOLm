(* ::Package:: *)

SetDirectory[NotebookDirectory[]]
Import["Functions.m"]


A={{2,1,0.1},{0.1,5,0.72},{-1.2,3,1.7}};
b={-2.9,-0.7,-9.86};


Ab=App[A,b];
Ab[[3]]=Ab[[3]]*2-Ab[[2]]+Ab[[1]]
Ab


Cb=DiagonalMatrix[{1/Ab[[1,1]],1/Ab[[2,2]],1/Ab[[3,3]]}]
B=IdentityMatrix[3]-Cb.Ab[[1;;3,1;;3]]
Norm[B,#]&/@{1,2,Infinity}


g=Ab[[All,4]]
x[0]:=g;
x[n_]:=B.x[n-1]+g;
Eps1=10^(-4)//N
Eps=(1-Norm[B,Infinity])/Norm[B,Infinity]*Eps1
n=1;
While[Norm[x[n+1]-x[n],Infinity] >= Eps,n++]
n
Norm[x[n+1]-x[n],Infinity]
X=x[n]
A.X-b



