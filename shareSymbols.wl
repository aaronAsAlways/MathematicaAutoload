(* ::Package:: *)

(* ::Text:: *)
(*Note that the setup must be an initialization cell, otherwise it won't be executed on loading the package.*)


(* ::Input::Initialization:: *)
BeginPackage["shareSymbols`",{"setupEnvironments`","utilities`"}]
listDataFiles::usage="listDataFiles[] returns a list of all files within the default data directory of the current project";
symbolNameString::usage="symbolNameString[<symbol>] returns a string 'symbol'";
saveSymbol::usage="saveSymbol[symbol,subdir] writes <symbol> to a file bearing its name with ending <expressionFileEnding> in subdirectory <subdir> (default: \"\") of the default data directory of the current project";
loadSymbol::usage="loadSymbol[symbol,subdir] loads <symbol> from a file bearing its name with ending <expressionFileEnding> in subdirectory <subdir> (default: \"\") in the default data directory of the current project and returns its value";
listSavedSymbols::usage="listSavedSymbols[] returns a list of saved symbols and subdirectories in string form by scanning the default data directory; the result can be evaluated using loadSymbol@@@listSavedSymbols[]";
saveSymbolsToMXFile::usage="saveSymbolsToMXFile[fileBaseName, symbols] saves a symbol or list of symbols to a file fileBaseName.mx in the default data directory of the current project";
loadSymbolsFromMXFile::usage="loadSymbolsFromMXFile[fileBaseName] loads symbols saved in the file fileBaseName.mx in the default data directory of the current project";
saveListToCSVFile::usage="saveListToCSVFile[fileBaseName,list] saves a list to a data file fileBaseName.csv in the default data directory of the current project";
loadListFromCSVFile::usage="loadListFromCSVFile[fileBaseName] returns the list saved in the file fileBaseName.csv in the default data directory of the current project";
listDataDirectoryFiles::usage="listDataDirectoryFiles[] returns a list of all files within the default data directory of the current project";
If[FailureQ[NotebookDirectory[]],Echo[FileBaseName[$InputFileName]<>" must only be loaded from a saved notebook!"]];
expressionFileEnding::usage="expressionFileEnding holds the file ending under which symbols are stored";


(* ::Input::Initialization:: *)
Begin["`Private`"]
listDataFiles[]:=FileNameTake[#,-1]&/@FileNames["*",setupEnvironments`projectDataDirectory[]];
expressionFileEnding="sym";
SetAttributes[symbolNameString,HoldFirst];
symbolNameString[symbol_]:=ToString@HoldForm@symbol;
SetAttributes[saveSymbol,HoldFirst];
saveSymbol[symbol_,subdir_:""]:=Export[FileNameJoin[{setupEnvironments`projectDataDirectory[],subdir,symbolNameString[symbol]<>"."<>expressionFileEnding}],symbol,"MathML"];
SetAttributes[loadSymbol,HoldFirst]
loadSymbol[symbol_,subdir_:""]:=ToExpression@Import[FileNameJoin[{setupEnvironments`projectDataDirectory[],subdir,symbolNameString[symbol]<>"."<>expressionFileEnding}],"MathML"];
SetAttributes[saveSymbolsToMXFile,HoldRest](* this is required to avoid evaluation of the symbol name(s) before they arrive inside DumpSave *);
saveSymbolsToMXFile[fileBaseName_String, symbols_]:=DumpSave[FileNameJoin[{setupEnvironments`projectDataDirectory[],fileBaseName<>".mx"}],symbols];
loadSymbolsFromMXFile[fileBaseName_String]:=DumpGet[FileNameJoin[{setupEnvironments`projectDataDirectory[],fileBaseName<>".mx"}]];
saveListToCSVFile[fileBaseName_String,list_,headers_:{}]:=Export[FileNameJoin[{setupEnvironments`projectDataDirectory[],fileBaseName<>".csv"}],list,"TableHeadings"->If[headers==={},None,ExportString[ToString[#],"String"(* this proved very important for exporting Greek letters as symbols, so change with care *)]&/@headers]];
loadListFromCSVFile[fileBaseName_String]:=Import[FileNameJoin[{setupEnvironments`projectDataDirectory[],fileBaseName<>".csv"}](* omitting "Table" or "Data" here proved very important for exporting Greek letters as symbols, so change with care *)];
listDataDirectoryFiles[]:=FileNameTake[#,-1]&/@FileNames["*",setupEnvironments`projectDataDirectory[],\[Infinity]];
listSavedSymbols[]:=Reverse@FileNameSplit@StringDelete["."<>expressionFileEnding]@FileNameTake[#,{Length@FileNameSplit[projectDataDirectory[]]+1,-1}]&/@FileNames["*."<>expressionFileEnding,projectDataDirectory[],\[Infinity]];
End[]


(* ::Input::Initialization:: *)
EndPackage[]
