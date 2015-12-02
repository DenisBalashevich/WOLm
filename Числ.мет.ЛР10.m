(* ::Package:: *)

ClearAll[x, f, n0, Gen, NewLP, Newf,A];


SetDirectory[NotebookDirectory[]]
Import["\:0414\:0430\:043d\:043d\:044b\:0435.m"]


x[k_,n_]:=-1+k 2/n;
f[x_]:=1/(1+25x^2);
Gen[n_]:=List[Table[x[k,n],{k,0,n}],Table[f[x[k,n]],{k,0,n}],{-1+1/n,-1/n,1/n,1-1/n}];
n0={4,10,20,40};


n0


Gen[4]


Newf[n_,i_]:=f[Gen[n][[3]][[i]]];
NewLP[n_,i_]:=LP[n,1,Gen[n][[3]][[i]],Gen[n][[1]]];


A[n_,i_]:=NewLP[n,i]-Newf[n,i]//N//Abs;


(*Table[A[n0[[j]],#]&/@Range[4],{j,4}]*)


Regen[n_]:=Table[{Gen[n][[1]][[j]],Gen[n][[2]][[j]]},{j,1,n+1}];


IS[n_]:=Interpolation[Regen[n],InterpolationOrder->1];
IP[n_]:=InterpolatingPolynomial[Regen[n],#]&;


LP[4,1,t,Gen[4][[1]]]//Expand


IP[4][1]


Newi[II_,n_,i_]:=II[n][Gen[n][[3]][[i]]];


InterpolatingPolynomial[Regen[4],t]//Expand


P[n_,i_]:=Newi[IP,n,i]-Newf[n,i]//N//Abs;
S[n_,i_]:=Newi[IS,n,i]-Newf[n,i]//N//Abs;


Table[Newi[IP,n0[[j]],#]&/@Range[4],{j,4}]//N//Abs


Table[P[n0[[j]],#]&/@Range[4],{j,4}]


Table[Newi[IS,n0[[j]],#]&/@Range[4],{j,4}]//N//Abs


Table[S[n0[[j]],#]&/@Range[4],{j,4}]
