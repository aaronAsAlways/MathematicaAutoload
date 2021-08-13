(* ::Package:: *)

(* ::Abstract:: *)
(*This package can be put into a directory that is parsed by Mathematica on startup. In this way, it can be called from any notebook (<<setupEnvironments`) in order to add the project directory to the search path.*)


(* ::Text:: *)
(*Note that the setup must be an initialization cell, otherwise it won't be executed on loading the package.*)


(* ::Input::Initialization:: *)
BeginPackage["setupEnvironments`",{"utilities`"}]
projectDirectory::usage="projectDirectory[] returns the project's root directory projectName by finding a .git directory";
projectDataDirectory::usage="projectDataDirectory[] returns the project's default data directory where data files are stored automatically";
If[FailureQ[NotebookDirectory[]],Echo[FileBaseName[$InputFileName]<>" must only be loaded from a saved notebook!"]];


(* ::Input::Initialization:: *)
Begin["`Private`"]
setProjectPath[]:=Block[{dirName=utilities`recursiveDirectoryNameSearch[".git"]},Print[dirName];If[FailureQ[dirName],Echo["Could not find .git directory. Package only works from within a git repository."];Return[$Failed],If[Not@MemberQ[$Path,dirName],AppendTo[$Path, dirName];AppendTo[$Path, FileNameJoin[{dirName,"MathematicaPackages"}]];Print["Directory "<>dirName<>" added to $Path."],Print["Directory "<>dirName<>" already added to $Path."]];dirName]];
projectDirectory[]=setProjectPath[];
dataDirectoryName="DataStorage";
projectDataDirectory[]:=FileNameJoin[{projectDirectory[],dataDirectoryName}];
End[]


(* ::Input::Initialization:: *)
EndPackage[]
