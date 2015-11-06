(* ::Package:: *)

ClearAll[pi, x0, f0, p1,p2, Tzero, A, P, LP, CNP, NP, TSA, TRO];


x0[i_]:=1+0.1i;
f0[i_]:=Sinh[x0[i]];
p1={1.01,1.02,1.03};
p2={1.11,1.12,1.13};


p


(*Newton polynom in the beginning of the table*)
CNP[0,i_]:=f0[i-1];
CNP[1,i_]:=f0[i] - f0[i-1];
CNP[n_,i_]:=CNP[n-1,i] - CNP[n-1,i-1];
NP[t_,L_,num_]:=Sum[CNP[j,1 + num]*Product[(t-k + 1),{k,1,j}]/j!,{j,0,L-1}];


NP[t,5]


((p1-x0[0])/0.1)


NP[#,5,0]&/@((p1-x0[0])/0.1)


NP[#,4,1]&/@((p2-x0[1])/0.1)


Sinh/@Join[p1,p2]



