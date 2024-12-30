BeginPackage["JerryI`Notebook`Debugger`", {

}]

Begin["`Private`"]

root = $InputFileName // DirectoryName // ParentDirectory;

{utilsAddListeners, utilsTransition} = Get[ FileNameJoin[{root, "src", "Utils.wl"}] ];



End[]
EndPackage[]