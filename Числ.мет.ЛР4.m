
pi=SetPrecision[3.141592,8];


x0={0,pi/6,pi/4,pi/3,pi/2}
f0={0,Sin[2pi/6],Sin[2pi/4],Sin[2pi/3],Sin[2pi/2]}
L=Length[x0]
p={pi/12,5pi/12,7pi/24}


A[k_]:=1/Product[x0[[k]]-x0[[i]],{i,Complement[Range[L],{k}]}]

P[x_]:=Sum[(A[i] f0[[i]])/(x-x0[[i]]),{i,L}]/Sum[A[i] /(x-x0[[i]]),{i,L}]


P/@p


Sin/@ (2p)


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



LP[4,1,7pi/24]


Precision[1.]


LP[4,1,x]


Plot[%101,{x,0,7.1pi/24}]


P[x]


Simplify[%107]


Plot[%108,{x,0,2}]
