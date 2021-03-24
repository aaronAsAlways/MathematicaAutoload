(* ::Package:: *)

(* ::Text:: *)
(*Note that the setup must be an initialization cell, otherwise it won't be executed on loading the package.*)


(* ::Input::Initialization:: *)
BeginPackage["shareSymbols`",{"setupEnvironments`","utilities`"}]
symbolNameString::usage="symbolNameString[<symbol>] returns a string 'symbol'";
saveSymbol::usage="saveSymbol[symbol,subdir] writes <symbol> to a file bearing its name with ending <expressionFileEnding> in subdirectory <subdir> (default: \"\") of the default data directory of the current project";
loadSymbol::usage="loadSymbol[symbol,subdir] loads <symbol> from a file bearing its name with ending <expressionFileEnding> in subdirectory <subdir> (default: \"\") in the default data directory of the current project and returns its value";
listSavedSymbols::usage="listSavedSymbols[] returns a list of saved symbols and subdirectories in string form by scanning the default data directory; the result can be evaluated using loadSymbol@@@listSavedSymbols[]";
saveSymbolsToFile::usage="saveSymbolsToFile[fileBaseName, symbols] saves a symbol or list of symbols to a file fileBaseName.mx in the default data directory of the current project";
loadSymbolsFromFile::usage="loadSymbolsFromFile[fileBaseName] loads symbols saved in the file fileBaseName.mx in the default data directory of the current project";
saveListToFile::usage="saveListToFile[fileBaseName,list] saves a list to a data file fileBaseName.csv in the default data directory of the current project";
loadListFromFile::usage="loadListFromFile[fileBaseName] returns the list saved in the file fileBaseName.csv in the default data directory of the current project";
listDataDirectoryFiles::usage="listDataDirectoryFiles[] returns a list of all files within the default data directory of the current project";
If[FailureQ[NotebookDirectory[]],Echo[FileBaseName[$InputFileName]<>" must only be loaded from a saved notebook!"]];
expressionFileEnding::usage="expressionFileEnding holds the file ending under which symbols are stored";


(* ::Input::Initialization:: *)
Begin["`Private`"]
expressionFileEnding="sym";
SetAttributes[symbolNameString,HoldFirst];
symbolNameString[symbol_]:=ToString@HoldForm@symbol;
SetAttributes[saveSymbol,HoldFirst];
saveSymbol[symbol_,subdir_:""]:=Export[FileNameJoin[{setupEnvironments`projectDataDirectory[],subdir,symbolNameString[symbol]<>"."<>expressionFileEnding}],symbol,"MathML"];
SetAttributes[loadSymbol,HoldFirst]
loadSymbol[symbol_,subdir_:""]:=ToExpression@Import[FileNameJoin[{setupEnvironments`projectDataDirectory[],subdir,symbolNameString[symbol]<>"."<>expressionFileEnding}],"MathML"];
SetAttributes[saveSymbolsToFile,HoldRest](* this is required to avoid evaluation of the symbol name(s) before they arrive inside DumpSave *);
saveSymbolsToFile[fileBaseName_String, symbols_]:=DumpSave[FileNameJoin[{setupEnvironments`projectDataDirectory[],fileBaseName<>".mx"}],symbols];
loadSymbolsFromFile[fileBaseName_String]:=DumpGet[FileNameJoin[{setupEnvironments`projectDataDirectory[],fileBaseName<>".mx"}]];
saveListToFile[fileBaseName_String,list_]:=Export[FileNameJoin[{setupEnvironments`projectDataDirectory[],fileBaseName<>".csv"}],list];
loadListFromFile[fileBaseName_String]:=Import[FileNameJoin[{setupEnvironments`projectDataDirectory[],fileBaseName<>".csv"}],"Data"];
listDataDirectoryFiles[]:=FileNameTake[#,-1]&/@FileNames["*",setupEnvironments`projectDataDirectory[],\[Infinity]];
listSavedSymbols[]:=Reverse@FileNameSplit@StringDelete["."<>expressionFileEnding]@FileNameTake[#,{Length@FileNameSplit[projectDataDirectory[]]+1,-1}]&/@FileNames["*."<>expressionFileEnding,projectDataDirectory[],\[Infinity]];
End[]


(* ::Input::Initialization:: *)
EndPackage[]
