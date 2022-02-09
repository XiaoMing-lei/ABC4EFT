(* ::Package:: *)

(* ::Input::Initialization:: *)
ModelIni[SMEFT];

AddGroup[SMEFT,"U1b"]; 
AddGroup[SMEFT,"U1l"]; 
AddGroup[SMEFT,"SU3c",GaugeBoson->"G"];
AddGroup[SMEFT,"SU2w",GaugeBoson->"W"];
AddGroup[SMEFT,"U1y",GaugeBoson->"B"];

nf=3;
AddField[SMEFT,"Q",-1/2,{"SU3c"->{1,0},"SU2w"->{1},"U1y"->1/6,"U1b"->1/3},Flavor->nf];
AddField[SMEFT,"uc",-1/2,{"SU3c"->{0,1},"U1y"->-2/3,"U1b"->-1/3},Flavor->nf];
AddField[SMEFT,"dc",-1/2,{"SU3c"->{0,1},"U1y"->1/3,"U1b"->-1/3},Flavor->nf];
AddField[SMEFT,"L",-1/2,{"SU2w"->{1},"U1y"->-1/2,"U1l"->1},Flavor->nf];
AddField[SMEFT,"ec",-1/2,{"U1y"->1,"U1l"->-1},Flavor->nf];
AddField[SMEFT,"H",0,{"SU2w"->{1},"U1y"->1/2}];
