(* ::Package:: *)

SetDirectory[NotebookDirectory[]]
Import["Functions.m"]


A={{2,1,0.1},{0.1,5,0.72},{-1.2,3,1.7}};
b={-2.9,-0.7,-9.86};


Ab=App[A,b];
Ab[[3]]=Ab[[3]]*2-Ab[[2]]+Ab[[1]]
Ab//G


Cb=DiagonalMatrix[{1/Ab[[1,1]],1/Ab[[2,2]],1/Ab[[3,3]]}]
B=IdentityMatrix[3]-Cb.Ab[[1;;3,1;;3]]
Norm[B,#]&/@{1,2,Infinity}
Norm[Inverse[B],#]&/@{1,2,Infinity}


g=Cb.Ab[[All,4]]
x1[0,k_]:=g[[k]];
x1[n_,k_]:=Sum[B[[k,i]]x1[n,i],{i,1,k-1}] + Sum[B[[k,j]]x1[n-1,j],{j,k,3}]+g[[k]];
x[n_]:=Table[x1[n,k],{k,1,3}];
Eps1=1/2*10^(-4)//N
Eps=(1-Norm[B,Infinity])/Norm[B,Infinity]*Eps1
y=x[5];
x1[0,k_]:=y[[k]];
n=1;
While[Norm[x[n+1]-x[n],Infinity] >= Eps,n++]
n
Norm[x[n+1]-x[n],Infinity]
X=x[n]
A.X-b







(*Hard*)
p1=-5;
p2=5;
q1=-3;
q2=3;
B={{p,q},{q,p}};
B1=Inverse[IdentityMatrix[2]-{{0,0},{q,0}}].{{p,q},{0,p}}
Eigenvalues[B1]
RegionPlot[Abs[Eigenvalues[B1][[1]]]<1 && Abs[Eigenvalues[B1][[2]]]<1,{p,p1,p2},{q,q1,q2}]
RegionPlot[Norm[B,1]<1,{p,p1,p2},{q,q1,q2}]
RegionPlot[Norm[B,Infinity]<1,{p,p1,p2},{q,q1,q2}]


B={{p,q},{-q,p}};

B1=Inverse[IdentityMatrix[2]-{{0,0},{-q,0}}].{{p,q},{0,p}};
RegionPlot[Abs[Eigenvalues[B1][[1]]]<1 && Abs[Eigenvalues[B1][[2]]]<1,{p,p1,p2},{q,q1,q2}]
RegionPlot[Norm[B,1]<1,{p,p1,p2},{q,q1,q2}]
RegionPlot[Norm[B,Infinity]<1,{p,p1,p2},{q,q1,q2}]






