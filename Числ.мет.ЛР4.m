(* ::Package:: *)

(* ::Input:: *)
(*pi=SetPrecision[3.141592,8];*)


(* ::Input:: *)
(*x0={0,pi/6,pi/4,pi/3,pi/2}*)
(*f0={0,Sin[2pi/6],Sin[2pi/4],Sin[2pi/3],Sin[2pi/2]}*)
(*L=Length[x0]*)
(*p={pi/12,5pi/12,7pi/24}*)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
(*A[k_]:=1/Product[x0[[k]]-x0[[i]],{i,Complement[Range[L],{k}]}]*)
(**)
(*P[x_]:=Sum[(A[i] f0[[i]])/(x-x0[[i]]),{i,L}]/Sum[A[i] /(x-x0[[i]]),{i,L}]*)
(**)


(* ::Input:: *)
(*P/@p*)


(* ::Input:: *)
(*Sin/@ (2p)*)


(* ::Input:: *)
(*Abs[Sin/@ (2p)-P/@p]*)


(* ::Input:: *)
(**)
(*Clear[LP]*)
(**)
(*LP[1,i_,x_]:=1/(x0[[i+1]]-x0[[i]]) Det[({*)
(* {f0[[i]], x0[[i]]-x},*)
(* {f0[[i+1]], x0[[i+1]]-x}*)
(*})]*)


(* ::Input:: *)
(*LP[1,1,1]*)


(* ::Input:: *)
(*LP[n_,i_,x_]:=1/(x0[[i+n]]-x0[[i]]) Det[({*)
(* {LP[n-1,i,x], x0[[i]]-x},*)
(* {LP[n-1,i+1,x], x0[[i+n]]-x}*)
(*})]*)


(* ::Input:: *)
(*LP[4,1,7pi/24]*)


(* ::Input:: *)
(*Precision[1.]*)


(* ::Input:: *)
(*LP[4,1,x]*)


(* ::Input:: *)
(*Plot[%101,{x,0,7.1pi/24}]*)


(* ::Input:: *)
(*P[x]*)


(* ::Input:: *)
(*Simplify[%107]*)


(* ::Input:: *)
(*Plot[%108,{x,0,2}]*)
