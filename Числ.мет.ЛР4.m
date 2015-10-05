(* ::Package:: *)

pi=SetPrecision[3.141593,8]


x0={0,pi/6,pi/4,pi/3,pi/2}
f0={0,Sin[2pi/6],Sin[2pi/4],Sin[2pi/3],Sin[2pi/2]}
L=Length[x0]
p={pi/12.,5.pi/12,7.pi/24}


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

LP[1,1,1]


LP[n_,i_,x_]:=1/(x0[[i+n]]-x0[[i]]) Det[({
 {LP[n-1,i,x], x0[[i]]-x},
 {LP[n-1,i+1,x], x0[[i+n]]-x}
})]



LP[4,1,7pi/24]+0.0


LP[4,1,x]


SetAttributes[LP,Listable]


COL[k_]:=LP[k,Range[L-k],x]


S=Col/@Range[4]


S/.x->p[[1]]


S/.x->p[[2]]


S/.x->p[[3]]


LP[4,1,p]


P0


Sin0
