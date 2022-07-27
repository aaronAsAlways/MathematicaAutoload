(* ::Package:: *)

(* ::Text:: *)
(*Note that the setup must be an initialization cell, otherwise it won't be executed on loading the package.*)


(* ::Input::Initialization:: *)
BeginPackage["shareSymbols`",{"setupEnvironments`","utilities`"}]
If[FailureQ[NotebookDirectory[]],Echo[FileBaseName[$InputFileName]<>" must only be loaded from a saved notebook!"]];
listDataFiles::usage="listDataFiles[] returns a list of all files within the default data directory of the current project";
symbolNameString::usage="symbolNameString[<symbol>] returns a string 'symbol'";
saveSymbol::usage="saveSymbol[symbol,subdir] writes <symbol> to a file bearing its name with ending <expressionFileEnding> in subdirectory <subdir> (default: \"\") of the default data directory of the current project";
loadSymbol::usage="loadSymbol[symbol,subdir] loads <symbol> from a file bearing its name with ending <expressionFileEnding> in subdirectory <subdir> (default: \"\") in the default data directory of the current project and returns its value (<subdir> can be several arguments according to the subdirectory tree reversed)";
deleteSymbol::usage="deleteSymbol[symbol,subdir] deletes the file containing <symbol> from the subdirectory <subdir> (default: \"\") in the default data directory of the current project";
listSavedSymbols::usage="listSavedSymbols[] returns a list of saved symbols and subdirectories in string form by scanning the default data directory; the result can be evaluated using loadSymbol@@@listSavedSymbols[]";
saveSymbolsToMXFile::usage="saveSymbolsToMXFile[fileBaseName,symbols,subdir] saves a symbol or list of symbols to a file <fileBaseName.mx> in the subdirectory <subdir> of the default data directory of the current project";
loadSymbolsFromMXFile::usage="loadSymbolsFromMXFile[fileBaseName,subdir] loads symbols saved in the file <fileBaseName.mx> in the subdirectory <subdir> of the default data directory of the current project";
saveListToCSVFile::usage="saveListToCSVFile[fileBaseName,list] saves a list to a data file fileBaseName.csv in the default data directory of the current project";
loadListFromCSVFile::usage="loadListFromCSVFile[fileBaseName] returns the list saved in the file fileBaseName.csv in the default data directory of the current project";
listDataDirectoryFiles::usage="listDataDirectoryFiles[] returns a list of all files within the default data directory of the current project";
expressionFileEnding::usage="expressionFileEnding holds the file ending under which symbols are stored";
createModelicaLikeString::usage="createModelicaLikeString[expression] converts expression into a string that is compatible with Modelica notation; setting appendSemicolon->False avoids the semicolon at the end";


(* ::Input::Initialization:: *)
Begin["`Private`"]
listDataFiles[]:=FileNameTake[#,-1]&/@FileNames["*",setupEnvironments`projectDataDirectory[]];
expressionFileEnding="sym";
SetAttributes[symbolNameString,HoldFirst];
symbolNameString[symbol_]:=ToString@HoldForm@symbol;
SetAttributes[saveSymbol,HoldFirst];
saveSymbol[symbol_,subdir_:""]:=Export[FileNameJoin[{setupEnvironments`projectDataDirectory[],subdir,symbolNameString[symbol]<>"."<>expressionFileEnding}],symbol,"MathML"];
SetAttributes[loadSymbol,HoldFirst]
loadSymbol[symbol_,subdir___:""]:=ToExpression@Import[FileNameJoin[Flatten@{setupEnvironments`projectDataDirectory[],Reverse@{subdir},symbolNameString[symbol]<>"."<>expressionFileEnding}],"MathML"];
deleteSymbol[symbol_,subdir_:""]:=DeleteFile[FileNameJoin[{setupEnvironments`projectDataDirectory[],subdir,symbolNameString[symbol]<>"."<>expressionFileEnding}]];
SetAttributes[saveSymbolsToMXFile,HoldRest](* this is required to avoid evaluation of the symbol name(s) before they arrive inside DumpSave *);
saveSymbolsToMXFile[fileBaseName_String, symbols_,subdir_:""]:=DumpSave[FileNameJoin[{setupEnvironments`projectDataDirectory[],subdir,fileBaseName<>".mx"}],symbols];
loadSymbolsFromMXFile[fileBaseName_String,subdir_:""]:=DumpGet[FileNameJoin[{setupEnvironments`projectDataDirectory[],subdir,fileBaseName<>".mx"}]];
saveListToCSVFile[fileBaseName_String,list_,headers_:{}]:=Export[FileNameJoin[{setupEnvironments`projectDataDirectory[],fileBaseName<>".csv"}],list,"TableHeadings"->If[headers==={},None,ExportString[ToString[#],"String"(* this proved very important for exporting Greek letters as symbols, so change with care *)]&/@headers]];
loadListFromCSVFile[fileBaseName_String]:=Import[FileNameJoin[{setupEnvironments`projectDataDirectory[],StringDelete[fileBaseName,".csv"(* avoid duplicate csv ending if function is called with ending *)]<>".csv"}](* omitting "Table" or "Data" here proved very important for exporting Greek letters as symbols, so change with care *)];
listDataDirectoryFiles[]:=FileNameTake[#,-1]&/@FileNames["*",setupEnvironments`projectDataDirectory[],\[Infinity]];
listSavedSymbols[]:=Reverse@FileNameSplit@StringDelete["."<>expressionFileEnding]@FileNameTake[#,{Length@FileNameSplit[projectDataDirectory[]]+1,-1}]&/@FileNames["*."<>expressionFileEnding,projectDataDirectory[],\[Infinity]];
createModelicaLikeString[expression_,OptionsPattern[appendSemicolon->True]]:=StringReplace[expression//InputForm//ToString,{"Sqrt"->"sqrt","["->"(","]"->")","=="->"=","\[Rho]"->"rho","\[Delta]"->"delta","\[Alpha]"->"alpha","\[Beta]"->"beta","\[Gamma]"->"gamma","m"~~"\[LetterSpace]"...~~"flow"->"m_flow","*^"->"e"}]<>If[OptionValue[appendSemicolon],";",""]
End[]


(* ::Input::Initialization:: *)
EndPackage[]
