(* ::Package:: *)

ClearAll[x, f, x0, f0, num, p, CNP, NPB, NPE, TRO];


x[i_]:=1+0.1i;
f[i_]:=Sinh[x[i]];
num=8;
x0 = Table[x[j],{j,0,num}];
f0 = Table[f[j],{j,0,num}];
p={1.01,1.41,1.75};
r={};


p1=((p[[1]]-x0[[1]])/0.1)
p2b=((p[[2]]-x0[[5]])/0.1)
p2e=((p[[2]]-x0[[6]])/0.1)
p3=((p[[3]]-x0[[9]])/0.1)


x0
f0


p


(*Newton polynom in the beginning of the table*)
CNP[0,i_]:=f0[[i]];
CNP[1,i_]:=f0[[i+1]] - f0[[i]];
CNP[n_,i_]:=CNP[n-1,i+1] - CNP[n-1,i];
NPB[t_,L_,num_]:=Sum[CNP[j,1 + num]*Product[(t-k+1),{k,1,j}]/j!,{j,0,L-1}];


(*Newton polynom in the end of the table*)
NPE[t_,L_,num_]:=Sum[CNP[j,1 + num-j]*Product[(t+k-1),{k,1,j}]/j!,{j,0,L-1}];


NPB[t,9,0]


(NPB[p1,9,0]-Sinh[p[[1]]])//Abs


NPB[p2b,5,4]-Sinh[p[[2]]]//Abs


NPE[p2e,6,5]-Sinh[p[[2]]]//Abs


NPE[p3,9,8]-Sinh[p[[3]]]//Abs


TRO[x0_,F_]:=Grid[Table[Table[F[n,i],{n,1,Length[x0]-i}],{i,1,Length[x0]}]]//N;


TRO[x0,CNP]
