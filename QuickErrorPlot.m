(* ::Package:: *)

(* ::Title:: *)
(*QuickErrorPlot*)


(* ::Text:: *)
(*v0.1*)
(*2012 Oct 28*)
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
    Legend      \[Rule] {},
    LegendPosition \[Rule] {0.75, -0.3},
    RemoveLines \[Rule] 0,
    Colors      \[Rule] 1,
    ColorsStart \[Rule] 1,

For the Plot:
    Labels        \[Rule] {\"\",\"\"},
    Title         \[Rule] \"\",
    PlotRange     \[Rule] Automatic,
    AspectRatio   \[Rule] 1/GoldenRatio";


Begin["`Private`"]


QuickErrorPlot::dataerr :=
  "Data must contain exactly 4 columns - x, y, \[CapitalDelta]x, \[CapitalDelta]y";


(* ::Title:: *)
(*Package*)


(* ::Section:: *)
(*Main QuickErrorPlot functions*)


(* ::Subsection:: *)
(*Options*)


Options[QuickErrorPlot]={
(* general: *)
    Legend      -> {},
    LegendPosition -> {0.75, -0.3},
    RemoveLinesStart -> 0,
    RemoveLinesEnd -> 0,
    Colors      -> 1,
    ColorsStart -> 1,
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
Needs["PlotLegends`"]


(* ::Section:: *)
(*Internal definition for QuickErrorPlot*)


InternalQuickErrorPlot[data_,opts:OptionsPattern[]] := Module[




(* ::Subsection:: *)
(*Local variables*)


{Opt, QEPoint, QEColors, QELegend,
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


QEPoint[x_,y_,dx_,dy_] := {
    {x,y},
    ErrorBar[dx,dy]
};


(* ::Subsubsection:: *)
(*Colors list*)


QEColors[c_, start_] := Table[
    ColorData[c, "ColorList"][[i]],
    {i, start, Length[ColorData[c, "ColorList"]]}
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


If[Length[data[[1]]] == 4, True, Message[QuickErrorPlot::dataerr]; False];


(* ::Subsubsection:: *)
(*Create points to plot*)


plot = QEPoint @@@ Drop[#, Opt@RemoveLinesStart] &/@ data;
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


(* ::Section:: *)
(*End*)


End[];
EndPackage[];
