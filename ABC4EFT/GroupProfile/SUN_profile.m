(* ::Package:: *)

(* ::Input::Initialization:: *)
(* Initialization *)
If[MatchQ[groupList,_List],AppendTo[groupList,"SUN"],groupList={"SUN"}];
AssocIni[tRep,tOut,tList,tasList,INDEX,tVal,tYDcol,tSimp,tY2M,tM2Y];
tList[SUN]={delN,Tg};


(* ::Input::Initialization:: *)
(* Define invariant tensors *)
AppendTo[tAssumptions,delN\[Element]Arrays[{Ng,Ng},Reals]];
TensorConj[delN[a_,b_]]:=delN[b,a]

AppendTo[tAssumptions,Tg\[Element]Arrays[{Tg^2-1,Tg,Tg},Reals]];
TensorConj[Tg[I_,a_,b_]]:=Tg[I,b,a]


(* ::Input::Initialization:: *)
(*Normalization tr(T^aT^b)=2\[Delta]^ab*)
tSimp[SUN]=Hold[Block[{},
delN[i_,j_]delN[j_,k_]:=delN[i,k];
delN[i_,i_]:=Ng;
delN[a_,c_]Tg[J_,a_,b_]:=Tg[J,c,b];
delN[c_,a_]Tg[J_,b_,a_]:=Tg[J,b,c];
Tg[I_,i_,j_]Tg[I_,k_,l_]:=2delN[l,i]delN[j,k]-2/Ng delN[j,i]delN[l,k];
Tg[i_,j_,j_]:=0;
]]
