(* ::Package:: *)

(*Shorthands*)
Clear[MF, T, App, G, S, Nr];
MF[A_]:=MatrixForm[A];
T[A_]:=Transpose[A];
(*Ab=Join[A,Transpose[{b}],2]*)
App[A_,B_]:=T[Append[T[A],B]];
App[A_]:=Fold[App,A];
G[A_]:=Grid[A,Frame->All];
S[x_]:=Total[x,{2}];


(*S2L1*)
Clear[Gen, ShowGen, Res];
Gen[AbS_]:=Module[
{n=Dimensions[AbS][[1]],gauss,R},
gauss[1]:=AbS[[1]]/AbS[[1,1]];
gauss[m_]:=AbS[[m]]-gauss[1]*AbS[[m,1]];
R=Table[gauss[i],{i,n}];
R=App[R,S[R]-R[[All,-1]]]
];
ShowGen[AbS_,d_]:=Module[
{n=Dimensions[AbS][[1]],R=AbS,sg,Q},
sg[1]=Gen[AbS];
sg[m_]:=Gen[R[[2;;-1,2;;-2]]];
Q=Table[R=sg[i],{i,d}];
Join@@Table[PadLeft[Q[[i]],{n+1-i,7},{Null}],{i,d}]
];
Res[AbS_]:=Module[
{n=Dimensions[AbS][[1]],rg,X},
rg[1]:=AbS[[-1,n+1;;-1]]/AbS[[n,n]];
rg[i_]:=AbS[[-i,n+1;;-1]]-Sum[AbS[[n+1-i,n+1-k]]*rg[k],{k,i-1}];
X=Table[rg[m],{m,n}]
];


"import complete"
