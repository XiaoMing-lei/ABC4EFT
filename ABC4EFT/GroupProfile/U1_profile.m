(* ::Package:: *)

(* ::Input::Initialization:: *)
(* Initialization *)
If[MatchQ[groupList,_List],AppendTo[groupList,"U1"],groupList={"U1"}];
AssocIni[tRep,tOut,tList,tasList,INDEX,tVal,tYDcol,tSimp];
If[!IntegerQ[dummyIndexCount],dummyIndexCount=0];
