(* ::Package:: *)

(* ::Abstract:: *)
(*This package can be put into a directory that is parsed by Mathematica on startup. In this way, it can be called from any notebook (<<setupEnvironments`) in order to add the project directory to the search path.*)


(* ::Text:: *)
(*Note that the setup must be an initialization cell, otherwise it won't be executed on loading the package.*)


(* ::Input::Initialization:: *)
BeginPackage["setupEnvironments`",{"utilities`"}]
projectDirectory::usage="projectDirectory[] returns the project's root directory projectName by finding a .git directory";
projectDataDirectory::usage="projectDataDirectory[] returns the project's default data directory where data files are stored automatically";
saveSymbolsToFile::usage="saveSymbolsToFile[fileBaseName, symbols] saves a symbol or list of symbols to a file fileBaseName.mx in the default data directory of the current project";
loadSymbolsFromFile::usage="loadSymbolsFromFile[fileBaseName] loads symbols saved in the file fileBaseName.mx in the default data directory of the current project";
saveListToFile::usage="saveListToFile[fileBaseName,list] saves a list to a data file fileBaseName.csv in the default data directory of the current project";
loadListFromFile::usage="loadListFromFile[fileBaseName] returns the list saved in the file fileBaseName.csv in the default data directory of the current project";
listDataFiles::usage="listDataFiles[] returns a list of all files within the default data directory of the current project";
If[FailureQ[NotebookDirectory[]],Echo[FileBaseName[$InputFileName]<>" must only be loaded from a saved notebook!"]];


(* ::Input::Initialization:: *)
Begin["`Private`"]
setProjectPath[]:=Block[{dirName=utilities`recursiveDirectoryNameSearch[".git"]},Print[dirName];If[FailureQ[dirName],Echo["Could not find .git directory. Package only works from within a git repository."];Return[$Failed],If[Not@MemberQ[$Path,dirName],AppendTo[$Path, dirName];AppendTo[$Path, FileNameJoin[{dirName,"MathematicaPackages"}]];Print["Directory "<>dirName<>" added to $Path."],Print["Directory "<>dirName<>" already added to $Path."]];dirName]];
projectDirectory[]=setProjectPath[];
dataDirectoryName="DataStorage";
projectDataDirectory[]:=FileNameJoin[{projectDirectory[],dataDirectoryName}];
SetAttributes[saveSymbolsToFile,HoldRest](* this is required to avoid evaluation of the symbol name(s) before they arrive inside DumpSave *);
saveSymbolsToFile[fileBaseName_String, symbols_]:=DumpSave[FileNameJoin[{projectDataDirectory[],fileBaseName<>".mx"}],symbols];
loadSymbolsFromFile[fileBaseName_String]:=DumpGet[FileNameJoin[{projectDataDirectory[],fileBaseName<>".mx"}]];
saveListToFile[fileBaseName_String,list_]:=Export[FileNameJoin[{projectDataDirectory[],fileBaseName<>".csv"}],list];
loadListFromFile[fileBaseName_String]:=Import[FileNameJoin[{projectDataDirectory[],fileBaseName<>".csv"}],"Data"];
listDataFiles[]:=FileNameTake[#,-1]&/@FileNames["*",projectDataDirectory[]];
End[]


(* ::Input::Initialization:: *)
EndPackage[]
