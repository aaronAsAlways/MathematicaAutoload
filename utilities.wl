(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["utilities`"]
cleanNotebooks::usage = "Argument-free function that opens a dialog for cleaning one or multplie notebooks for git usage";
simplifyRule::usage="function that evaluates lists of rules such that the right-hand sides do not change anymore due to other rules from the list";
convertToSInumbers::usage="function to turn Quantites into numbers after converting to SI";
mapFunctionToRuleValues::usage="mapFunctionToRuleValues[function,ruleList] maps function over right-hand sides of list of rules ruleList";
convertToSInumbersRuleList::usage="function to convert right-hand sides of list of rules to SI-based numbers";
convertToSINumericExpression::usage="function to turn expressions containing Quantity and QuantityVariable into mixed symbolic/numeric expressions after converting to SI";
entityTypeNotYetRegistered::usage="entityTypeNotYetRegistered[type] returns True if the given entity type has not yet been registered";
entityTypeAlreadyRegistered::usage="entityTypeAlreadyRegistered[type] returns True if the given entity type has already been registered";
multiply2DCoordinatesBySeparateFactors::usage="multiply2DCoordinatesBySeparateFactors[coordsList,factorList] multiplies the elements of coordsList by the respective factors in factorList";
recursiveDirectoryNameSearch::usage="recursiveDirectoryNameSearch[nameToFind,pathLengthToStopAt] searches for the file/directory name nameToFind starting from the current notebook's directory and stopping when the remaining path becomes pathLengthToStopAt long, returning $Failed in case it did not find anything";
convertStringToRealNumber::usage="convertStringToRealNumber[string] applies a JSON-based conversion to a string to return a real number";
functionQ::usage="functionQ[symbol] returns True if <symbol> is a function and False otherwise";
reverseRule::usage="reverseRule[<rule>] reverses the order of the given rule such that x\[Rule]y becomes y\[Rule]x";
interpolateInAssociation::usage="interpolateInAssociation[assoc,xKey,xValue,yKey,yContainerKey] returns a value for the property <yKey> interpolated at the x position <xValue> of property <xKey> within the association <assoc> (which contains of repeated list of rules with equal keys); if the <yKey>->yValue rules are stored under yet another (optional) key <yContainerKey>, they will be looked up";
padSublistsWithConstants::usage="padSublistsWithConstants[list,parameters] pads list's sublists with constants by joining the list parameters with each of them";
readColumnwiseFile::usage="readColumnwiseFile[file,nHeaderLines,nColumns] reads a text file with nColumns numeric columns and skips nHeaderLines header lines";
numericallyEqualQ::usage="numericallyEqualQ[a,b,absTol] returns true if real numbers a and b are numerically equal within absolute tolerance absTol (optional, default 1e-5)";
Begin["`Private`"]
cleanNotebook=Function[{nb},Module[{nbObj=NotebookOpen[nb],nbRaw,nbText},(*If[nb\[Equal]NotebookFileName[],Return[]];*)nbRaw=NotebookGet[nbObj];nbRaw=DeleteCases[nbRaw,HoldPattern[CellChangeTimes->{___}],\[Infinity]];
nbRaw=Replace[nbRaw,_String?(StringContainsQ["In["~~NumberString~~"]:="]):>"",\[Infinity]];NotebookPut[nbRaw,nbObj];SetOptions[nbObj,NotebookEventActions->{"WindowClose"->FrontEndExecute[FrontEndToken["DeleteGeneratedCells"]]},PrivateNotebookOptions->{"FileOutlineCache"->False},"TrackCellChangeTimes"->False,CreateCellID->False];NotebookSave[nbObj];NotebookClose[nbObj];nbText=Import[nb,"Text"];(*nbText=StringReplace[nbText,"\""~~Shortest[x___]~~"\""/;(StringLength/@StringSplit[StringDelete[x,"\\\n"(* ignore line breaks within the text *)],"-"])===(* compare to defined length of UUID substrings *){8,4,4,4,12}\[RuleDelayed]"\"\""];*) (* since it uses a condition (/;) it is much slower than the solution below, though the latter is a bit less specific *)
nbText=StringReplace[nbText,"\""~~Shortest[WordCharacter..~~"-"~~WordCharacter..~~"-"~~WordCharacter..~~"-"~~WordCharacter..~~"-"~~WordCharacter..]~~"\"":>"\"\""];Export[nb,nbText,"Text"];]];
cleanNotebooks[]:=Module[{cleanupChoice=ChoiceDialog["What to clean?",{"All notebooks below a specific directory"->1,"A specific notebook"->2}]},
Switch[cleanupChoice,1,With[{directory=SystemDialogInput["Directory",NotebookDirectory[]]},cleanNotebook/@FileNames["*.nb",directory,Infinity]],2,With[{notebook=SystemDialogInput["FileOpen",{NotebookDirectory[],{"Wolfram Notebook"->{"*.nb"},"All files"->{"*"}}}]},If[Head[notebook]===List,cleanNotebook/@notebook,cleanNotebook@notebook]]]];
simplifyRule=Thread[Rule[#[[All,1]],#[[All,2]]//.#]]&;
convertToSInumbers=QuantityMagnitude[UnitConvert[#]]&;
mapFunctionToRuleValues[function_,ruleList_]:=Normal[Map[function,Association[ruleList]]];
convertToSInumbersRuleList[ruleList_]:=mapFunctionToRuleValues[convertToSInumbers,ruleList];
convertToSINumericExpression[expr_]:=Module[{convertQuantityVariableToExpression},
convertQuantityVariableToExpression[x_]:=ToExpression[x];
convertQuantityVariableToExpression[x_,y_]:=convertQuantityVariableToExpression[x];
expr/.{QuantityVariable->convertQuantityVariableToExpression,Quantity[x_,y_]->convertToSInumbers[Quantity[x,y]]}]
entityTypeNotYetRegistered[type_String]:=MissingQ[Entity[type]["EntityStore"]];
entityTypeAlreadyRegistered[type_String]:=Not[entityTypeNotYetRegistered[type]];
multiply2DCoordinatesBySeparateFactors[coordsList_,factorList_]:=Map[{factorList[[1]] #[[1]] , factorList[[2]]#[[2]]}&,coordsList];
recursiveDirectoryNameSearch[nameToFind_,pathLengthToStopAt_:2]:=Block[{p=NotebookDirectory[]},Catch[While[FileNames[nameToFind,p]==={},p=ParentDirectory[p];If[Length@FileNameSplit[p]==pathLengthToStopAt,Throw[$Failed]]];p]];
convertStringToRealNumber=N@ImportString[#,"JSON"]&;
functionQ=Not[DownValues[#]==={}]&;
reverseRule=ReplaceAll[#,Rule[arg1_,arg2_]:>Rule[arg2,arg1]]&;
interpolateInAssociation[assoc_,xKey_,xValue_,yKey_,yContainerKey_:"unlikelyKeyName",interpolationOrder_:1]:=Interpolation[Transpose[{Lookup[xKey]@assoc,Lookup[yKey]@With[{found=Lookup[yContainerKey]@assoc},If[Head[found[[1]](* a list is always returned *)]===Missing,assoc,found]]}],xValue,InterpolationOrder->interpolationOrder];
padSublistsWithConstants[list_List,parameters_List]:=Join[#,parameters]&/@list;
readColumnwiseFile=Function[{file,nHeaderLines,nColumns},Module[{str=OpenRead[file],out},
Skip[str,String,nHeaderLines];
out=ReadList[str,ConstantArray[Number,nColumns]];
Close[str];out]];
numericallyEqualQ[a_,b_,absTol_:10^-5]:=Abs[a-b]<absTol;
End[]
EndPackage[]
