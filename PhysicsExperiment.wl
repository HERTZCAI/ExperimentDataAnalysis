(* ::Package:: *)

BeginPackage["PhysicsExperiment`"];

(*This is a package developed by HERTZCAI from Fudan University. The philosophy of this package is to semi-automize the data processing and leave more time for you to ponder the essence beneath physics phenomena. Copyright reserved. This package is not allowed to be used for business purpose.*)
UncertaintyA::usage = "UncertaintyA[Data,a]\:7ed9\:51faA+B2\:4e0d\:786e\:5b9a\:5ea6";
UncertaintyB1::usage = "UncertaintyB1[b1,a]\:7ed9\:51faB1+B2\:4e0d\:786e\:5b9a\:5ea6";
xpux::usage = "xpux[Data,a]\:7ed9\:51fa\:5bf9\:5e94\:4e8e\:6570\:636eData\:548cA+B2\:4e0d\:786e\:5b9a\:5ea6\:7684\:7269\:7406\:91cf";
ufunc::usage = "ufuc[fn,argumets]\:7ed9\:51fa\:51fd\:6570fn\:5bf9\:5e94\:7684\:8bef\:5dee\:51fd\:6570,arguments\:4e3a\:81ea\:53d8\:91cf\:5217\:8868";
Uncertainty::usage = "Uncertainty[Db_,a_]\:82e5Db\:4e3a\:6570\:636e\:7ec4\:5219\:7ed9\:51faUncertaintyA,\:82e5Db\:4e3a\:4f30\:8bfb\:4e0d\:786e\:5b9a\:5ea6\:5219\:7ed9\:51faUncertaintyB1";
ToAround::usage = "ToAround[k]\:5c06k\:8f6c\:5316\:4e3aAround\:578b";
ExcelImport::usage = "ExcelImport[filepath_,row1_,row2_,column1_,column2_]\:7ed9\:51fafilepath\:7684row1-row2,column1-column2,\:82e5\:4e00\:5217\:5219\:7701\:7565column2";
ErrorEvaluation::usage = "ErrorEvaluation[experimental_,theoretical_]\:7ed9\:51fa\:5b9e\:9a8c\:4e0e\:7406\:8bba\:7684\:76f8\:5bf9\:8bef\:5dee"
UncertaintyContribution::usage="UncertaintyContribution[fn_,arguments_,value_]\:7ed9\:51fa\:8bef\:5dee\:8d21\:732e"



(*Data Import*)
ExcelImport[filepath_, row1_, row2_, column1_, column2_] :=
    Import[filepath][[1]][[row1 ;; row2, column1 ;; column2]]
ExcelImport[filepath_, row1_, row2_, column1_] :=
    Import[filepath][[1]][[row1 ;; row2, column1]]


(*Uncertainty Evaluation*)
UncertaintyA[Data_, a_] :=
    Sqrt[a^2 / 3 + Sum[(x - Mean[Data])^2, {x, Data}] / (Length[Data] (Length[Data] - 1))]
UncertaintyB1[b1_, a_] :=
    Sqrt[b1^2 + a^2 / 3]
Uncertainty[Db_, a_] :=
    If[ListQ[Db],
        UncertaintyA[Db, a]
        ,
        UncertaintyB1[Db, a]
    ]

xpux[Data_, a_] :=
    Around[Mean[Data], UncertaintyA[Data, a]]
xpux[Data_, a_, b1_] :=
    Around[Data, UncertaintyB1[b1, a]]

UncertaintyContribution[fn_,arguments_,value_]:=
{
UCList={};
For[i=1,i<=Length[arguments],i++,AppendTo[UCList,D[fn,arguments[[i]]]]];
UCList,
arguments=#["Value"]&/@value;
UCList*(#["Uncertainty"]&/@value)
}





(*Function Derivation*)
(*Around\:4e4b\:95f4\:53ef\:4ee5\:76f4\:63a5\:8fd0\:7b97,\:4f46\:82e5\:8981\:5199\:8ba1\:7b97\:8bef\:5dee\:7684\:8fc7\:7a0b,\:7531\:4e0b\:9762\:7684ufunc\:7ed9\:51fa*)
ufunc[fn_, arguments_] := {
    ufun = 0;
    For[i = 1, i <= Length[arguments], i++, 
        ufun += D[fn, arguments[[i]]]^2 "\!\(\*SuperscriptBox[\(u\), \(2\)]\)(" <> ToString[arguments[[i]]] <> ")"
    ];
    FullSimplify[Sqrt[ufun],Assumptions->{#>0&/@arguments}]
}


(*Format Conversion*)
(*\:4eceOrigin\:76f4\:63a5\:590d\:5236\:5305\:542b\[PlusMinus]\:53f7\:7684\:659c\:7387,\:8f6c\:5316\:4e3aAround*)
ToAround[k_] :=
    Around[#[[1]], #[[2]]]&[Level[k, 1]]


(*Error Evaluation*)
ErrorEvaluation[experimental_, theoretical_] :=
{
    errordecimal=(experimental - theoretical) / theoretical;
    Around[PercentForm[errordecimal["Value"]],PercentForm[errordecimal["Uncertainty"]]]
}


EndPackage[];
