(* ::Package:: *)

(* ::Title:: *)
(*QuickErrorPlot*)


(* ::Text:: *)
(*v0.2*)
(*2012 Oct 31*)
(**)
(*Copyright 2012 Tadej Novak*)
(*You may use this file under the terms of the BSD license - see LICENSE.txt*)


(* ::Title:: *)
(*Begin*)


BeginPackage["QuickErrorPlot`"];


QuickErrorPlot::usage =
"QuickErrorPlot[ data , <options> ]:
  Creates a simple ErrorListPlot with a Legend.

Data must contain exactly 4 columns - x, y, \[CapitalDelta]x, \[CapitalDelta]y, and must be
in a form of sheets: {{{x, y, \[CapitalDelta]x, \[CapitalDelta]y}, {...}, ...}, {...}, ...}

This is a list of options that QuickErrorPlot accepts and their
default options. Where multiple options are listed, the first 
is the default:

General:
    Legend           \[Rule] {},
    LegendPosition   \[Rule] {0.75, -0.3},
    RemoveLinesStart -> 0,
    RemoveLinesEnd   -> 0,
    Colors           \[Rule] 1,
    ColorsStart      \[Rule] 1,

For the Plot:
    Labels        \[Rule] {\"\",\"\"},
    Title         \[Rule] \"\",
    PlotRange     \[Rule] Automatic,
    AspectRatio   \[Rule] 1/GoldenRatio";


QuickFit::usage =
"QuickFit[ data , <options> ]:
  Creates simple fits for data.

Data must contain exactly 4 columns - x, y, \[CapitalDelta]x, \[CapitalDelta]y, and must be
in a form of sheets: {{{x, y, \[CapitalDelta]x, \[CapitalDelta]y}, {...}, ...}, {...}, ...}

This is a list of options that QuickFit accepts and their
default options. Where multiple options are listed, the first 
is the default:

General:
    RemoveLinesStart -> 0,
    RemoveLinesEnd   -> 0";


QuickFitPlot::usage =
"QuickFitPlot[ data , <options> ]:
  Creates simple fits for data and plot them.

Data must contain exactly 4 columns - x, y, \[CapitalDelta]x, \[CapitalDelta]y, and must be
in a form of sheets: {{{x, y, \[CapitalDelta]x, \[CapitalDelta]y}, {...}, ...}, {...}, ...}

This is a list of options that QuickFitPlot accepts and their
default options. Where multiple options are listed, the first 
is the default:

General:
    RemoveLinesStart -> 0,
    RemoveLinesEnd   -> 0,
    Colors           \[Rule] 1,
    ColorsStart      \[Rule] 1

For the Plot:
    Select -> All | {1, 2, ...},
    XRange \[Rule] Automatic | {1, 2}";


Begin["`Private`"]


QuickErrorPlot::dataerr :=
  "Data must contain exactly 4 columns - x, y, \[CapitalDelta]x, \[CapitalDelta]y";
QuickFit::dataerr :=
  "Data must contain exactly 4 columns - x, y, \[CapitalDelta]x, \[CapitalDelta]y";
QuickFitPlot::dataerr :=
  "Data must contain exactly 4 columns - x, y, \[CapitalDelta]x, \[CapitalDelta]y";


(* ::Title:: *)
(*QuickErrorPlot*)


(* ::Section:: *)
(*Main QuickErrorPlot functions*)


(* ::Subsection:: *)
(*Options*)


Options[QuickErrorPlot]={
(* general: *)
    Legend           -> {},
    LegendPosition   -> {0.75, -0.3},
    RemoveLinesStart -> 0,
    RemoveLinesEnd   -> 0,
    Colors           -> 1,
    ColorsStart      -> 1,
(* for the plot: *)
    Labels        -> {"",""},
    Title         -> "",
    PlotRange     -> Automatic,
    AspectRatio   -> 1/GoldenRatio
};


(* ::Subsection:: *)
(*QuickErrorPlot*)


QuickErrorPlot[data_,opts___] := 
    InternalQuickErrorPlot[data,opts];


(* ::Subsection:: *)
(*Needs*)


Needs["ErrorBarPlots`"]


(* ::Section:: *)
(*Internal definition for QuickErrorPlot*)


InternalQuickErrorPlot[data_,opts:OptionsPattern[]] := Module[
















(* ::Subsection:: *)
(*Local variables*)


{Opt, QEPlotPoint, QEColors, QELegend,
 plot, sheets,
 errorPlot, errorLegend},
















(* ::Subsection:: *)
(*Internal functions*)


(* ::Subsubsection:: *)
(*Options parsing*)


Opt[x_] := OptionValue[QuickErrorPlot,
    FilterRules[{opts},Options[QuickErrorPlot]],x];


(* ::Subsubsection:: *)
(*Create points*)


QEPlotPoint[x_,y_,dx_,dy_] := {
    {x,y},
    ErrorBar[dx,dy]
};


(* ::Subsubsection:: *)
(*Colors list*)


QEColors[c_, start_] := If[Length[data] == 1,
    ColorData[c, "ColorList"][[start]],
    Table[
        ColorData[c, "ColorList"][[i]],
        {i, start, Length[ColorData[c, "ColorList"]]}
    ]
];


(* ::Subsubsection:: *)
(*Legend data*)


QELegend[names_, color_, start_] := Table[
    {Graphics[{ColorData[color, "ColorList"][[start - 1 + i]], Point[{0,0}]}], names[[i]]},
    {i, 1, Length[names]}
];


(* ::Subsection:: *)
(*Data processing*)


(* ::Subsubsection:: *)
(*Validate data*)


If[Length[data[[1]][[1]]] == 4, True, Message[QuickErrorPlot::dataerr]; False];


(* ::Subsubsection:: *)
(*Create points to plot*)


plot = QEPlotPoint @@@ Drop[#, Opt@RemoveLinesStart] &/@ data;
plot = Drop[#, -Opt@RemoveLinesEnd] &/@ plot;


(* ::Subsection:: *)
(*Construct the error plot*)


errorPlot = ErrorListPlot[
    plot,
    PlotRange -> Opt@PlotRange,
    PlotStyle -> QEColors[Opt@Colors, Opt@ColorsStart],
    GridLines -> Automatic,
    GridLinesStyle -> Directive[Gray, Dotted],
    Frame -> True,
    FrameLabel -> Opt@Labels,
    PlotLabel -> Opt@Title,
    AspectRatio -> Opt@AspectRatio];


(* ::Subsection:: *)
(*Construct the legend*)


If[Length[Opt@Legend] == 0, errorLegend = {},
    errorLegend = {
        QELegend[Opt@Legend, Opt@Colors, Opt@ColorsStart],
        LegendPosition -> Opt@LegendPosition,
        LegendSize -> 0.75,
        LegendShadow -> None}
];


(* ::Subsection:: *)
(*Complete the colorbarplot*)


If[Length[Opt@Legend] == 0,
    errorPlot,
    ShowLegend[errorPlot, errorLegend]
]


]


(* ::Title:: *)
(*QuickFit*)


(* ::Section:: *)
(*Main QuickFit functions*)


(* ::Subsection:: *)
(*Options*)


Options[QuickFit]={
(* general: *)
    IncludeConstantBasis -> True,
    RemoveLinesStart -> 0,
    RemoveLinesEnd   -> 0
};


(* ::Subsection:: *)
(*QuickErrorPlot*)


QuickFit[data_,opts___] := 
    InternalQuickFit[data,opts];


(* ::Section:: *)
(*Internal definition for QuickFit*)


InternalQuickFit[data_,opts:OptionsPattern[]] := Module[
















(* ::Subsection:: *)
(*Local variables*)


{Opt, QFPoints, QFErrors, QFit,
 points, errors, fits},
















(* ::Subsection:: *)
(*Internal functions*)


(* ::Subsubsection:: *)
(*Options parsing*)


Opt[x_] := OptionValue[QuickFit,
    FilterRules[{opts},Options[QuickFit]],x];


(* ::Subsubsection:: *)
(*Create points*)


QFPoints[l_] := Transpose[Transpose[
    Drop[Drop[l, Opt@RemoveLinesStart], -Opt@RemoveLinesEnd]
][[1;;2]]];


(* ::Subsubsection:: *)
(*Create errors*)


QFErrors[l_] := Transpose[
    Drop[Drop[l, Opt@RemoveLinesStart], -Opt@RemoveLinesEnd]
][[4]];


(* ::Subsubsection:: *)
(*Create fit*)


QFit[p_, e_] := LinearModelFit[
    p,
    x,
    x,
    Weights -> 1/e^2,
    VarianceEstimatorFunction -> (1&),
    IncludeConstantBasis -> Opt@IncludeConstantBasis
];


(* ::Subsection:: *)
(*Data processing*)


(* ::Subsubsection:: *)
(*Validate data*)


If[Length[data[[1]][[1]]] == 4, True, Message[QuickFit::dataerr]; False];


(* ::Subsubsection:: *)
(*Create points to plot*)


points = Map[QFPoints, data, 1];
errors = Map[QFErrors, data, 1];


(* ::Subsection:: *)
(*Construct the fits*)


fits = Table[
    QFit[points[[i]], errors[[i]]], {i, 1, Length[data]}
]


]


(* ::Title:: *)
(*QuickFitPlot*)


(* ::Section:: *)
(*Main QuickFitPlot functions*)


(* ::Subsection:: *)
(*Options*)


Options[QuickFitPlot]={
(* general: *)
    IncludeConstantBasis -> True,
    RemoveLinesStart -> 0,
    RemoveLinesEnd   -> 0,
    Colors           -> 1,
    ColorsStart      -> 1,
(* for the plot: *)
    Select  -> All,
    XRange  -> Automatic
};


(* ::Subsection:: *)
(*QuickFitPlot*)


QuickFitPlot[data_,opts___] := 
    InternalQuickFitPlot[data,opts];


(* ::Section:: *)
(*Internal definition for QuickFitPlot*)


InternalQuickFitPlot[data_,opts:OptionsPattern[]] := Module[
















(* ::Subsection:: *)
(*Local variables*)


{Opt, QEColors,
 fits, plot, range, fitPlot},
















(* ::Subsection:: *)
(*Internal functions*)


(* ::Subsubsection:: *)
(*Options parsing*)


Opt[x_] := OptionValue[QuickFitPlot,
    FilterRules[{opts},Options[QuickFitPlot]],x];


(* ::Subsubsection:: *)
(*Colors list*)


QEColors[c_, start_] := If[Length[data] == 1,
    ColorData[c, "ColorList"][[start]],
    Table[
        ColorData[c, "ColorList"][[i]],
        {i, start, Length[ColorData[c, "ColorList"]]}
    ]
];


(* ::Subsection:: *)
(*Data processing*)


(* ::Subsubsection:: *)
(*Validate data*)


If[Length[data[[1]][[1]]] == 4, True, Message[QuickErrorPlot::dataerr]; False];


(* ::Subsubsection:: *)
(*Create fits to plot*)


fits = QuickFit[
    data,
    RemoveLinesStart -> Opt@RemoveLinesStart,
    RemoveLinesEnd -> Opt@RemoveLinesEnd,
    IncludeConstantBasis -> Opt@IncludeConstantBasis
];
plot = Table[fits[[i]][x], {i, 1, Length[fits]}];


(* ::Subsubsection:: *)
(*Define range*)


If[Opt@XRange != Automatic, range = Opt@XRange,
    range = {
        Min[Transpose[Drop[Drop[data, Opt@RemoveLinesStart], -Opt@RemoveLinesEnd]][[1]]],
        Max[Transpose[Drop[Drop[data, Opt@RemoveLinesStart], -Opt@RemoveLinesEnd]][[1]]]
    }
];


(* ::Subsection:: *)
(*Construct the fit plot*)


fitPlot = Plot[
    plot,
    Evaluate[Join[{x}, Opt@XRange]],
    PlotStyle -> QEColors[Opt@Colors, Opt@ColorsStart]
]


]


(* ::Section:: *)
(*End*)


End[];
EndPackage[];
