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





(*Hard*)
p1=-1;
p2=1;
q1=-1;
q2=1;
B={{p,q},{q,p}};
RegionPlot[Abs[Eigenvalues[B][[1]]]<1 && Abs[Eigenvalues[B][[2]]]<1,{p,p1,p2},{q,q1,q2}]
RegionPlot[Norm[B,1]<1,{p,p1,p2},{q,q1,q2}]
RegionPlot[Norm[B,2]<1,{p,p1,p2},{q,q1,q2}]
RegionPlot[Norm[B,Infinity]<1,{p,p1,p2},{q,q1,q2}]


B={{p,q},{-q,p}};
RegionPlot[Abs[Eigenvalues[B][[1]]]<1 && Abs[Eigenvalues[B][[2]]]<1,{p,p1,p2},{q,q1,q2}]
RegionPlot[Norm[B,1]<1,{p,p1,p2},{q,q1,q2}]
RegionPlot[Norm[B,2]<1,{p,p1,p2},{q,q1,q2}]
RegionPlot[Norm[B,Infinity]<1,{p,p1,p2},{q,q1,q2}]



