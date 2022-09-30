(* ::Package:: *)

(* ::Abstract:: *)
(*This package contains wrappers for the common plotting functions of Mathematica which are meant to make the plots suitable for usage in a PowerPoint presentation:*)
(*- thicker lines*)
(*- larger font size*)
(*- axes label placement*)


(* ::Input::Initialization:: *)
BeginPackage["presentablePlots`"]
exportPlot::usage="exportPlot[plot] exports the given <plot> to a png with default ImageResolution";
modListPlot::usage="modListPlot[data,opts] invokes ListPlot with changed default options and extra options (AxesFontSize)";
modListLogPlot::usage="modListLogPlot[data,opts] invokes ListLogPlot with changed default options and extra options (AxesFontSize)";
modListStepPlot::usage="modListStepPlot[data,opts] invokes ListStepPlot with changed default options and extra options (AxesFontSize)";
modPlot::usage="modPlot[data,opts] invokes Plot with changed default options and extra options (AxesFontSize)";
modParametricPlot::usage="modParametricPlot[data,opts] invokes ParametricPlot with changed default options and extra options (AxesFontSize)";
modListPointPlot3D::usage="modListPointPlot3D[data,opts] invokes ListPointPlot3D with changed default options and extra options (AxesFontSize)";
modListPlot3D::usage="modListPlot3D[data,opts] invokes ListPlot3D with changed default options and extra options (AxesFontSize)";
modPlot3D::usage="modPlot3D[args,opts] invokes Plot3D with changed default options and extra options (AxesFontSize)";
Begin["`Private`"]
defaultAxesFontSize=14;
exportPlot[plot_,opts:OptionsPattern[{ImageResolution->600,Export}]]:=Export[FileNameJoin[{NotebookDirectory[],FileBaseName[NotebookFileName[]]<>"_"<>ToString@RandomInteger[10000]<>".png"}],plot,ImageResolution->OptionValue[ImageResolution],(* this trick ensures that the remainder of options is passed to Export[], but restricted to the options known to Export[] *)Evaluate@(Sequence@@FilterRules[{opts},Options[Export]])]
(*SetOptions[#,BaseStyle\[Rule]{FontFamily\[Rule]"Helvetica",FontSize\[Rule]14},PlotStyle\[Rule]Thick,AxesStyle\[Rule]Directive[Thickness[0.008],Darker[Gray]]]&/@{Plot,ListPlot,ListPlot3D,Plot3D,ListPointPlot3D};*)
modListPlot[data__,opts:OptionsPattern[{PlotMarkers->"OpenMarkers",AxesFontSize->defaultAxesFontSize,GridLines->Automatic,ListPlot}]]:=
ListPlot[data,Axes->None,Frame->{True,True,False,False},FrameStyle->Directive[Thick,Darker[Gray],FontSize->OptionValue[AxesFontSize]],FrameLabel->OptionValue[AxesLabel],PlotStyle->Thick,PlotMarkers->OptionValue[PlotMarkers],GridLines->OptionValue[GridLines](* to use the default from the OptionsPattern, the options must be explicitly used here, otherwise the default from ListPlot[] is used *),(* this trick ensures that the remainder of options is passed to ListPlot[], but restricted to the options known to ListPlot[] *)Evaluate@(Sequence@@FilterRules[{opts},Options[ListPlot]])]
modListLogPlot[data__,opts:OptionsPattern[{PlotMarkers->"OpenMarkers",AxesFontSize->defaultAxesFontSize,GridLines->Automatic,ListLogPlot}]]:=
ListLogPlot[data,Axes->None,Frame->{True,True,False,False},FrameStyle->Directive[Thick,Darker[Gray],FontSize->OptionValue[AxesFontSize]],FrameLabel->OptionValue[AxesLabel],PlotStyle->Thick,PlotMarkers->OptionValue[PlotMarkers],GridLines->OptionValue[GridLines](* to use the default from the OptionsPattern, the options must be explicitly used here, otherwise the default from ListPlot[] is used *),(* this trick ensures that the remainder of options is passed to ListPlot[], but restricted to the options known to ListPlot[] *)Evaluate@(Sequence@@FilterRules[{opts},Options[ListLogPlot]])]
modListStepPlot[data__,opts:OptionsPattern[{AxesFontSize->defaultAxesFontSize,GridLines->Automatic,ListStepPlot}]]:=
ListStepPlot[data,Axes->None,Frame->{True,True,False,False},FrameStyle->Directive[Thick,Darker[Gray],FontSize->OptionValue[AxesFontSize]],FrameLabel->OptionValue[AxesLabel],PlotStyle->Thick,GridLines->OptionValue[GridLines](* to use the default from the OptionsPattern, the options must be explicitly used here, otherwise the default from ListStepPlot[] is used *),(* this trick ensures that the remainder of options is passed to ListStepPlot[], but restricted to the options known to ListStepPlot[] *)Evaluate@(Sequence@@FilterRules[{opts},Options[ListStepPlot]])]
modPlot[args__,opts:OptionsPattern[{AxesFontSize->defaultAxesFontSize,GridLines->Automatic,Plot}]]:=Plot[args,Axes->None,Frame->{True,True,False,False},FrameStyle->Directive[Thick,Darker[Gray],FontSize->OptionValue[AxesFontSize]],FrameLabel->OptionValue[AxesLabel],PlotStyle->Thick,GridLines->OptionValue[GridLines](* to use the default from the OptionsPattern, the options must be explicitly used here, otherwise the default from Plot[] is used *),(* this trick ensures that the remainder of options is passed to Plot[], but restricted to the options known to Plot[] *)Evaluate@(Sequence@@FilterRules[{opts},Options[Plot]])]
modParametricPlot[args__,opts:OptionsPattern[{AxesFontSize->defaultAxesFontSize,GridLines->Automatic,ParametricPlot}]]:=ParametricPlot[args,Axes->None,Frame->{True,True,False,False},FrameStyle->Directive[Thick,Darker[Gray],FontSize->OptionValue[AxesFontSize]],FrameLabel->OptionValue[AxesLabel],PlotStyle->Thick,GridLines->OptionValue[GridLines](* to use the default from the OptionsPattern, the options must be explicitly used here, otherwise the default from Plot[] is used *),(* this trick ensures that the remainder of options is passed to Plot[], but restricted to the options known to Plot[] *)Evaluate@(Sequence@@FilterRules[{opts},Options[ParametricPlot]])]
modListPointPlot3D[data__,opts:OptionsPattern[{AxesFontSize->defaultAxesFontSize,AxesLabel->{Automatic,Automatic,Automatic},ListPointPlot3D}]]:=
ListPointPlot3D[data,BoxStyle->Directive[Thickness[0.001],Dashed,Gray],AxesStyle->Directive[Thick,Darker[Gray],FontSize->OptionValue[AxesFontSize]],AxesLabel->OptionValue[AxesLabel],PlotStyle->Directive[PointSize[Scaled[0.03]]],(* this trick ensures that the remainder of options is passed to ListPointPlot3D[], but restricted to the options known to ListPointPlot3D[] *)Evaluate@(Sequence@@FilterRules[{opts},Options[ListPointPlot3D]])]
modListPlot3D[data__,opts:OptionsPattern[{AxesFontSize->defaultAxesFontSize,AxesLabel->{Automatic,Automatic,Automatic},ListPlot3D}]]:=
ListPlot3D[data,BoxStyle->Directive[Thickness[0.001],Dashed,Gray],AxesStyle->Directive[Thick,Darker[Gray],FontSize->OptionValue[AxesFontSize]],AxesLabel->OptionValue[AxesLabel],PlotStyle->Directive[PointSize[Scaled[0.03]]],(* this trick ensures that the remainder of options is passed to ListPlot3D[], but restricted to the options known to ListPlot3D[] *)Evaluate@(Sequence@@FilterRules[{opts},Options[ListPlot3D]])]
modPlot3D[data__,opts:OptionsPattern[{AxesFontSize->defaultAxesFontSize,AxesLabel->{Automatic,Automatic,Automatic},Plot3D}]]:=
Plot3D[data,BoxStyle->Directive[Thickness[0.001],Dashed,Gray],AxesStyle->Directive[Thick,Darker[Gray],FontSize->OptionValue[AxesFontSize]],AxesLabel->OptionValue[AxesLabel],PlotStyle->Directive[PointSize[Scaled[0.03]]],(* this trick ensures that the remainder of options is passed to Plot3D[], but restricted to the options known to Plot3D[] *)Evaluate@(Sequence@@FilterRules[{opts},Options[Plot3D]])]
End[]
EndPackage[]
