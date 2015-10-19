(* ::Package:: *)

SetDirectory[NotebookDirectory[]]
Import["\:0414\:0430\:043d\:043d\:044b\:0435.m"]


P[#,x0]&/@p


Abs[%-f[p]]


LP[4,1,#1,x0]&/@p//N


Abs[%-f[p]]


SG[#,x0,LP]&/@p


D[Sin[2x],x]
M=MaxValue[Derivative[Length[x0]][f][x],x]


R=M/5!*(pi/2)^5/2^9


NP[#,x0]&/@p-f[p]//Abs
f[p]



