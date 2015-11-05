(* ::Package:: *)

ClearAll[pi, x0, f0, p, Tzero, A, P, LP, CNP, NP, TSA, TRO];


x0[i_]:=1+0.1i;
f0[i_]:=Sinh[x0[i]];
p={1.01,1.02,1.03,1.11,1.12,1.13};


p


(*Newton polynom in the beginning of the table*)
CNP[0,i_]:=f0[i-1];
CNP[1,i_]:=f0[i] - f0[i-1];
CNP[n_,i_]:=CNP[n-1,i] - CNP[n-1,i-1];
NP[t_,L_]:=Sum[CNP[j,1]*Product[(t-k + 1),{k,1,j}]/j!,{j,0,L-1}];


NP[t,5]


NP[#,5]&/@((p-x0[0])/0.1)


Sinh/@p


Sinh[1.00]
