(* ::Package:: *)

pi=SetPrecision[3.141593,8]


X[k_]:=pi/4 Cos[(2k-1)/10 pi]+pi/4


X0=X/@Range[5]


x0={0,pi/6,pi/4,pi/3,pi/2}
f0=Sin/@(2*X0)
L=Length[x0]
p={pi/12.,5.pi/12,7.pi/24}



x0=X0


A[k_]:=1/Product[x0[[k]]-x0[[i]],{i,Complement[Range[L],{k}]}]

P[x_]:=Sum[(A[i] f0[[i]])/(x-x0[[i]]),{i,L}]/Sum[A[i] /(x-x0[[i]]),{i,L}]


P0=P/@p


Sin0=Sin/@ (2p)


Abs[Sin/@ (2p)-P/@p]



Clear[LP]

LP[1,i_,x_]:=1/(x0[[i+1]]-x0[[i]]) Det[({
 {f0[[i]], x0[[i]]-x},
 {f0[[i+1]], x0[[i+1]]-x}
})]
LP[n_,i_,x_]:=1/(x0[[i+n]]-x0[[i]]) Det[({
 {LP[n-1,i,x], x0[[i]]-x},
 {LP[n-1,i+1,x], x0[[i+n]]-x}
})]


LP[4,1,7pi/24]+0.0


LP[4,1,x]


SetAttributes[LP,Listable]


Col[k_]:=LP[k,Range[L-k],x]


S=Col/@Range[4]


S/.x->p[[1]]


S/.x->p[[2]]


S/.x->p[[3]]


LP[4,1,p]


P0


Sin0



Table[Grid[S/.x->p[[j]]],{j,3}]


P0


Abs[Sin0-P0]


X[k_]:=pi/4 Cos[(2k-1)/10 pi]+pi/4


X0=X/@Range[5]


D[Sin[2x],x]


M=32


R=M/5!*(pi/2)^5/2^9

Product[15 i,{i,0}]



Clear[FN,PFN]
FN[0,i_,x_]:=f0[[i]]
FN[1,i_,x_]:=1/(x0[[i+1]]-x0[[i]]) (f0[[i+1]] - f0[[i]])
FN[n_,i_,x_]:=1/(x0[[i+n]]-x0[[i]]) (FN[n-1,i,x] - FN[n-1,i+1,x])
SetAttributes[FN,Listable]
ColFN[k_]:=LP[k,Range[L-k],x]
F=ColFN/@Range[4]
PFN[x_]:=Sum[FN[j,0,x]*Product[(x-x0[[k]]),{k,1,j+1}],{j,0,L-1}]
PFN/@ p


Table[Grid[F/.x->p[[j]]],{j,3}]
