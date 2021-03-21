(* ::Package:: *)

(* ::Abstract:: *)
(*This package contains wrappers for the common plotting functions of Mathematica which are meant to make the plots suitable for usage in a PowerPoint presentation:*)
(*- thicker lines*)
(*- larger font size*)
(*- axes label placement*)


(* ::Input::Initialization:: *)
BeginPackage["presentablePlots`"]
exportPlot::usage="exportPlot[plot] exports the given <plot> to a png";
modListPlot::usage="modListPlot[data,opts] nicely plots the list <data> with options <opts>";
modPlot::usage="modPlot[data,opts] nicely plots the list <data> with options <opts>";
modListPointPlot3D::usage="modListPointPlot3D[data,opts] nicely plots the list <data> with options <opts>";
modListPlot3D::usage="modListPlot3D[data,opts] nicely plots the list <data> with options <opts>";
modPlot3D::usage="modPlot3D[data,opts] nicely plots the list <data> with options <opts>";
Begin["`Private`"]
exportPlot[plot_]:=Export[FileNameJoin[{NotebookDirectory[],FileBaseName[NotebookFileName[]]<>"_"<>ToString@RandomInteger[10000]<>".png"}],plot,ImageResolution->600]
(*SetOptions[#,BaseStyle\[Rule]{FontFamily\[Rule]"Helvetica",FontSize\[Rule]14},PlotStyle\[Rule]Thick,AxesStyle\[Rule]Directive[Thickness[0.008],Darker[Gray]]]&/@{Plot,ListPlot,ListPlot3D,Plot3D,ListPointPlot3D};*)
modListPlot[data__,OptionsPattern[{AxesLabel->{Automatic,Automatic},PlotMarkers->{"OpenMarkers",Thick,Medium},ListPlot}]]:=
ListPlot[data,PlotRange->Full,Axes->None,Frame->{True,True,False,False},FrameStyle->Directive[Thick,Darker[Gray],FontSize->14],FrameLabel->OptionValue[AxesLabel]]
modPlot[args__,OptionsPattern[{AxesLabel->{None,None},GridLines->Automatic,Plot}]]:=
Plot[args,PlotRange->Full,Axes->None,Frame->{True,True,False,False},FrameStyle->Directive[Thick,Darker[Gray],FontSize->14],FrameLabel->OptionValue[AxesLabel],PlotStyle->Thick,GridLines->OptionValue[GridLines],PlotLabel->OptionValue[PlotLabel]]
modListPointPlot3D[data__,OptionsPattern[{AxesLabel->{Automatic,Automatic},PlotMarkers->{"OpenMarkers",Thick,Medium},ListPointPlot3D}]]:=
ListPointPlot3D[data,PlotRange->Full,BoxStyle->Directive[Thickness[0.001],Dashed,Gray],AxesStyle->Directive[Thick,Darker[Gray],FontSize->14],AxesLabel->OptionValue[AxesLabel],PlotStyle->Directive[PointSize[Scaled[0.03]]]]
modListPlot3D[data__,OptionsPattern[{AxesLabel->{Automatic,Automatic},PlotMarkers->{"OpenMarkers",Thick,Medium},ListPlot3D}]]:=
ListPlot3D[data,PlotRange->Full,BoxStyle->Directive[Thickness[0.001],Dashed,Gray],AxesStyle->Directive[Thick,Darker[Gray],FontSize->16],AxesLabel->OptionValue[AxesLabel],PlotStyle->Directive[PointSize[Scaled[0.03]]]]
modPlot3D[data__,OptionsPattern[{AxesLabel->{Automatic,Automatic},PlotMarkers->{"OpenMarkers",Thick,Medium},Plot3D}]]:=
Plot3D[data,PlotRange->Full,BoxStyle->Directive[Thickness[0.001],Dashed,Gray],AxesStyle->Directive[Thick,Darker[Gray],FontSize->16],AxesLabel->OptionValue[AxesLabel],PlotStyle->Directive[PointSize[Scaled[0.03]]]]
End[]
EndPackage[]
