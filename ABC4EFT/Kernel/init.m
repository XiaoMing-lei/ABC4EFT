(* ::Package:: *)

If[$DEBUG=!=True,$DEBUG=False];
$AmplitudeBasisDir =DirectoryName[$InputFileName,2];
$CodeFiles=FileNames[__~~".m",FileNameJoin[{$AmplitudeBasisDir,"Code"}]];


BeginPackage["ABC4EFT`"];
$ABC4EFTVersion = "1.1.0";


(* ::Input::Initialization:: *)
(* Read & Write *)
{EnCodeFromFolder,LoadModel,LoadGroup};

(* Amplitude *)
{ab,sb,s,\[Epsilon]p,Mandelstam,SSYT,GetState,GetClass,AmpReduce,SoftEvenComb,SoftOddComb,ExtractCoeff,YPermute,PWExpand};

(* Model Input *)
{ModelIni,AddGroup,AddField,AllTypesR,AllTypesC,GetTypes,CheckType,CheckGroup,SetNflavor,BosonicQ,AssocIni,TotCharge,deltaBL};

(* Lorentz Factor *)
{LorentzList,LorentzBasis,LorentzCount,OperPoly};

(* Gauge Group Factor *)
{GaugeCount,GaugeBasis,TensorConj,TensorInnerProduct,TraceClasses,MultiTrace,traceFullResult,TraceSymGroup,TraceBasis,AllTraces};

(* Formating *)
{Ampform,transform,Present};

(* j-basis *)
{W2,W2Diagonalize,W2Check};

(* Analysis *)
{CheckIndependence,SolveRedundancy,GetBasisForType,CountBasisForType,GetJBasisForType,StatResult,PresentStat,PrintStat,GenerateOperatorList,FindCor,FindYCoord,FindMCoord};

(* Useful Lie groups in GroupMath *)
{U1,SU2,SU3,SU4,SU5,SU6};
{FS,DC,Tg,tr,eps3n,del2,del3,del3n,del8n,\[Tau],\[Lambda],eps2a,eps2f,eps3n,eps3a,eps3f,fabc,dabc,sigma,sigmab,Ga,sigmaT,sigmabT};

(* Group Profile *)
{tAssumptions,tRep,tOut,tVal,tRank,tReduce,TGen,tYDcol,tSimp,tY2M,tM2Y,GellMann,CF,PrintTensor};


permutationBasis="left"; (* or "right" *)
groupList={};

maxTry=30;
h2f=<|-2->CL,-1->FL,-1/2->\[Psi],0->\[Phi],1/2->OverBar[\[Psi]],1->FR,2->CR|>;
LorentzIndex=Join[Join[{"\[Mu]","\[Nu]","\[Lambda]","\[Rho]","\[Eta]","\[Xi]"},Alphabet["Greek"][[19;;-1]]],Append[StringJoin[#,"1"]&/@Alphabet["Greek"],StringJoin[#,"2"]&/@Alphabet["Greek"]]];
FLAVOR={"p","r","s","t","u","v","x","y","z"};



If[!Global`$DEBUG,Begin["`Private`"]];
Do[Get[file],{file,Global`$CodeFiles}];
If[!Global`$DEBUG,End[]];

(*--------------------------- STARTUP MESSAGE -----------------------------*)

Print["                   ========================== "];
Print["                         ABC4EFT ",$ABC4EFTVersion];
Print["                   ==========================\n "];
Print["                     A Mathematica Package for         "];
Print["       Amplitude Basis Construction for Effective Field Theories         \n"];
Print["           Authors: Hao-Lin Li, lihaolin1991@gmail.com"];
Print["                    Zhe Ren, renzhe@itp.ac.cn    "];
Print["                    Ming-Lei Xiao, minglei.xiao@northwestern.edu           "];
Print["                    Jiang-Hao Yu, jhyu@itp.ac.cn          "];
Print["                    Yu-Hui Zheng, zhengyuhui@itp.ac.cn     \n"];
Print["                The package is available at ",Hyperlink["hepforge","https://abc4eft.hepforge.org"]];
Print["               For the latest version, see the ",Hyperlink["GitHub","https://github.com/XiaoMing-lei/ABC4EFT"]];
Print["               If you use this package in your research,          "];
Print["          Please cite: arXiv: 2201.04639, 2005.00008, 2007.07899         "];


EndPackage[]
