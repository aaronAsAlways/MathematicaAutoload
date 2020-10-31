(* ::Package:: *)

(* ::Abstract:: *)
(*This package can be put into a directory that is parsed by Mathematica on startup. In this way, it can be called from any notebook (<<setupEnvironments`) in order to add the project directory to the search path.*)


(* ::Text:: *)
(*Note that the setup must be an initialization cell, otherwise it won't be executed on loading the package.*)


(* ::Input::Initialization:: *)
BeginPackage["setupEnvironments`",{"utilities`"}]
projectDirectory::usage="projectDirectory[] returns the project's root directory projectName by finding a .git directory";
saveSymbolsToFile::usage="saveSymbolsToFile[fileBaseName, symbols] saves a symbol or list of symbols to a file fileBaseName.mx in the default data directory of current project";
loadSymbolsFromFile::usage="loadSymbolsToFile[fileBaseName] loads symbols saved in the file fileBaseName.mx in the default data directory of current project";
If[FailureQ[NotebookDirectory[]],Echo[FileBaseName[$InputFileName]<>" must only be loaded from a saved notebook!"]];


(* ::Input::Initialization:: *)
Begin["`Private`"]
setProjectPath[]:=Block[{dirName=utilities`recursiveDirectoryNameSearch[".git"]},Print[dirName];If[FailureQ[dirName],If[Not@MemberQ[$Path,dirName],Echo["Could not find .git directory. Package only works from within a git repository."];Return[$Failed],AppendTo[$Path, dirName];AppendTo[$Path, FileNameJoin[{dirName,"MathematicaPackages"}]];Print["Directory "<>dirName<>" added to $Path."],Print["Directory "<>dirName<>" already added to $Path."]];dirName]];
projectDirectory[]=setProjectPath[];
dataDirectoryName="MathematicaDataStorage";
SetAttributes[saveSymbolsToFile,HoldAll](* this is required to avoid evaluation of the symbol name(s) before they arrive inside DumpSave; since there is not HoldLast, we use HoldAll *);
saveSymbolsToFile[fileBaseName_, symbols_]:=DumpSave[FileNameJoin[{projectDirectory[],dataDirectoryName,fileBaseName<>".mx"}],symbols];
loadSymbolsFromFile[fileBaseName_]:=DumpGet[FileNameJoin[{projectDirectory[],dataDirectoryName,fileBaseName<>".mx"}]];
End[]


(* ::Input::Initialization:: *)
EndPackage[]
