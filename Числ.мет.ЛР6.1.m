(* ::Package:: *)

SetDirectory[NotebookDirectory[]]
Import["\:0414\:0430\:043d\:043d\:044b\:0435.m"]


(*Result 3*)
P[#,x0]&/@p-f[p]//Abs


(*Result 4*)
LP[4,1,#1,x0]&/@p-f[p]//Abs
M=MaxValue[Derivative[Length[x0]][f][x],x]
R=M/5!*(pi/2)^5/2^9


(*Result 5*)
LP[4,1,#1,Tzero[5]]&/@p-f[p]//Abs


(*Result 6*)
NP[#,x0]&/@p-f[p]//Abs
NP[#,Tzero[5]]&/@p-f[p]//Abs


TRO[x0,CNP]


TRO[Append[x0,#1],CNP]&/@p
