(* ::Package:: *)

ClearAll[pi, x0, f, f0, Tzero, A, P, LP, CNP, NP, SG];


$MinPrecision = 8;


(*Initial data*)
pi=SetPrecision[3.141593,8];
x0={0,pi/6,pi/4,pi/3,pi/2};
f[x_]:=Sin[2x];
SetAttributes[f,Listable];
f0=f[x0];
p={pi/12,5pi/12,7pi/24};


(*Chebyshev polynom's zeros*)
Tzero[n_]:=Table[pi/4 Cos[(2 k-1)/(2*n) pi]+pi/4,{k,n}];


(*Barycentric form of Lagrange polynom*)
A[k_,x0_]:=1/Product[x0[[k]]-x0[[i]],{i,Complement[Range[Length[x0]],{k}]}];
P[x_,x0_]:=Sum[(A[i,x0] f0[[i]])/(x-x0[[i]]),{i,Length[x0]}]/Sum[A[i,x0] /(x-x0[[i]]),{i,Length[x0]}];


(*Aitkin algorithm of Lagrange polynom*)
LP[1,i_,x_,x0_]:=1/(x0[[i+1]]-x0[[i]]) Det[({
 {f0[[i]], x0[[i]]-x},
 {f0[[i+1]], x0[[i+1]]-x}
})];
LP[n_,i_,x_,x0_]:=1/(x0[[i+n]]-x0[[i]]) Det[({
 {LP[n-1,i,x,x0], x0[[i]]-x},
 {LP[n-1,i+1,x,x0], x0[[i+n]]-x}
})];


(*Newton polynom*)
CNP[0,i_,x0_]:=f0[[i]];
CNP[1,i_,x0_]:=1/(x0[[i+1]]-x0[[i]]) (f0[[i+1]] - f0[[i]]);
CNP[n_,i_,x0_]:=1/(x0[[i+n]]-x0[[i]]) (CNP[n-1,i+1,x0] - CNP[n-1,i,x0]);
NP[x_,x0_]:=Sum[CNP[j,1,x0]*Product[(x-x0[[k]]),{k,1,j}],{j,0,Length[x0]-1}];


NP[#,x0]&/@x0
f0


(*Grid*)
SG[x_,x0_,F_]:=Grid[Table[Table[F[n,i,x,x0],{n,1,Length[x0]-i}],{i,1,Length[x0]}]]//N;


"import complete"
