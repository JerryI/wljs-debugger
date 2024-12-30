BeginPackage["JerryI`Notebook`Debugger`", {
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`", 
    "JerryI`Notebook`", 
    "JerryI`WLX`",
    "JerryI`WLX`Importer`",
    "JerryI`WLX`WebUI`",
    "JerryI`Misc`WLJS`Transport`",
    "JerryI`Notebook`AppExtensions`",
    "JerryI`Notebook`Kernel`",
    "KirillBelov`HTTPHandler`",
    "KirillBelov`HTTPHandler`Extensions`",
    "KirillBelov`Internal`"
}]

Begin["`Private`"]

root = $InputFileName // DirectoryName // ParentDirectory;

{utilsAddListeners, utilsTransition} = Get[ FileNameJoin[{root, "src", "Utils.wl"}] ];

gui  = ImportComponent[{root, "templates", "GUI.wlx"}];
gui  = gui[{utilsAddListeners, utilsTransition}];

With[{http = AppExtensions`HTTPHandler},
    http["MessageHandler", "Debugger"] = AssocMatchQ[<|"Path" -> ("/debugger/"~~___)|>] -> gui;
];


End[]
EndPackage[]