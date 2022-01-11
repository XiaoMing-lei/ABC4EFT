(* ::Package:: *)

(* ::Input::Initialization:: *)
(* Initialization *)
If[MatchQ[groupList,_List],AppendTo[groupList,"SU2"],groupList={"SU2"}];
AssocIni[tRep,tOut,tList,tasList,INDEX,tVal,tYDcol,tSimp,tY2M,tM2Y];
tList[SU2]={del2,eps2a,eps2f,\[Tau],del3n,eps3n};
tasList[SU2]={eps2a,eps2f,eps3n};
tVal[SU2]={del2->IdentityMatrix[2],eps2f->LeviCivitaTensor[2],eps2a->LeviCivitaTensor[2],\[Tau]->GellMann[2],del3n->IdentityMatrix[3],eps3n->LeviCivitaTensor[3]};
tYDcol[SU2]=eps2a;
If[!IntegerQ[dummyIndexCount],dummyIndexCount=0];


(* ::Input::Initialization:: *)
(* Define invariant tensors *)
AppendTo[tAssumptions,del2\[Element]Arrays[{2,2},Reals]];
tRep[del2]={{-1},{1}};
tOut[del2]=PrintTensor[<|"tensor"->"\[Delta]","upind"->{#1},"downind"->{#2}|>]&;
TensorConj[del2[a_,b_]]:=del2[b,a]

AppendTo[tAssumptions,eps2a\[Element]Arrays[{2,2},Reals,Antisymmetric[{1,2}]]];
tRep[eps2a]={{-1},{-1}};
tOut[eps2a]=PrintTensor[<|"tensor"->"\[Epsilon]","upind"->{#1,#2}|>]&;
TensorConj[eps2a[x__]]:=eps2f[x]

AppendTo[tAssumptions,eps2f\[Element]Arrays[{2,2},Reals,Antisymmetric[{1,2}]]];
tRep[eps2f]={{1},{1}};
tOut[eps2f]=PrintTensor[<|"tensor"->"\[Epsilon]","downind"->{#1,#2}|>]&;
TensorConj[eps2f[x__]]:=eps2a[x]

AppendTo[tAssumptions,\[Tau]\[Element]Arrays[{3,2,2},Reals]];
tRep[\[Tau]]={{2},{1},{-1}};
tOut[\[Tau]]=PrintTensor[<|"tensor"-> PrintTensor[<|"tensor"->"\[Tau]","upind"->{#1}|>],"upind"->{#3},"downind"->{#2}|>]&;
TensorConj[\[Tau][I_,a_,b_]]:=\[Tau][I,b,a]

AppendTo[tAssumptions,del3n\[Element]Arrays[{3,3},Reals,Symmetric[{1,2}]]];
tRep[del3n]={{2},{2}};
tOut[del3n]=PrintTensor[<|"tensor"->"\[Delta]","upind"->{#1,#2}|>]&;
TensorConj[del3n[x__]]:=del3n[x]

AppendTo[tAssumptions,eps3n\[Element]Arrays[{3,3,3},Reals,Antisymmetric[{1,2,3}]]];
tRep[eps3n]={{2},{2},{2}};
tOut[eps3n]=PrintTensor[<|"tensor"->"\[Epsilon]","upind"->{#1,#2,#3}|>]&;
TensorConj[eps3n[x__]]:=eps3n[x]


(* ::Input::Initialization:: *)
AssociateTo[tY2M,{
\[Tau][a_,j_,k_]\[Tau][b_,k_,m_]:>Module[{dummy=Unique[]},I eps3n[a,b,dummy]\[Tau][dummy,j,m]+del3n[a,b]del2[m,j]]
}];
AssociateTo[tM2Y,{
eps3n[a_,b_,c_]:>Module[{d1=Unique[],d2=Unique[],d3=Unique[]},-(I/4) \[Tau][a,d1,d2](\[Tau][b,d2,d3]\[Tau][c,d3,d1]-\[Tau][c,d2,d3]\[Tau][b,d3,d1])]
}];


(* ::Input::Initialization:: *)
tSimp[SU2]=Hold[Block[{},
del2[i_,j_]del2[j_,k_]:=del2[i,k];
del2[i_,i_]:=2;
del3n[i_,i_]:=3;
del3n[a_,c_]del3n[a_,b_]:=del3n[c,b];
del3n[a_,b_]del3n[b_,c_]:=del3n[a,c];
del3n[a_,c_]del3n[b_,c_]:=del3n[a,b];
del3n[b_,c_]del3n[a_,b_]:=del3n[a,c];
del3n[a_,b_]^2:=3;
del2[a_,c_]\[Tau][J_,a_,b_]:=\[Tau][J,c,b];
del2[c_,a_]\[Tau][J_,b_,a_]:=\[Tau][J,b,c];
\[Tau][i_,j_,j_]:=0;
\[Tau][a_,i_,j_]\[Tau][a_,k_,l_]:=2del2[l,i]del2[j,k]-del2[l,k]del2[j,i];
eps2a[x_,y_] eps2f[w_,z_]:=del2[x,w] del2[y,z]-del2[x,z] del2[y,w];
eps3n[i_,j_,k_]eps3n[l_,m_,n_]:=Det@Outer[del3n,{i,j,k},{l,m,n}];
del3n[a_,d_]eps3n[a_,b_,c_]:=eps3n[d,b,c];
del3n[a_,d_]eps3n[b_,a_,c_]:=eps3n[b,d,c];
del3n[a_,d_]eps3n[c_,b_,a_]:=eps3n[c,b,d];
eps2f[i_,j_]del2[i_,k_]:=eps2f[k,j];
eps2f[i_,j_]del2[j_,k_]:=eps2f[i,k];
eps2a[i_,j_]del2[k_,i_]:=eps2a[k,j];
eps2a[i_,j_]del2[k_,j_]:=eps2a[i,k];
]]


(* ::Input::Initialization:: *)
ConvertToFundamental[model_,groupname_,{0}]:=If[CheckGroup[model,groupname]==SU2,1,Message[ConvertToFundamental::name,groupname,{1}]]
ConvertToFundamental[model_,groupname_,{1}]:=If[CheckGroup[model,groupname]==SU2,{1,eps2f[a[1],aa[1]]},Message[ConvertToFundamental::name,groupname,{1}]]
ConvertToFundamental[model_,groupname_,{2}]:=If[CheckGroup[model,groupname]==SU2,dummyIndexCount++;
\[Tau][A[1],aa[1],dummyIndex[dummyIndexCount]]eps2f[dummyIndex[dummyIndexCount],aa[2]],Message[ConvertToFundamental::name,groupname,{2}]]

CF[{0},num_,ind_]:=1
CF[{1},num_,ind_]:=del2[ind,Subscript[num, 1]]
CF[{-1},num_,ind_]:=eps2f[Subscript[num, 1],ind]
CF[{2},num_,ind_]:=TensorContract[eps2f\[TensorProduct]\[Tau],{{1,5}}][Subscript[num, 1],ind,Subscript[num, 2]]

AssocIni[INDEX[SU2]];
INDEX[SU2][{0}]={};
INDEX[SU2][{1}]={"i","j","k","l","m","n"};
INDEX[SU2][{-1}]={"i","j","k","l","m","n"};
INDEX[SU2][{2}]={"I","J","K","L","M","N"};

TGen[{1}]:=1/2 \[Tau][#1,#2,#3]&
TGen[{-1}]:=-1/2 \[Tau][#1,#3,#2]&
TGen[{2}]:=-I eps3n[#1,#2,#3]&
