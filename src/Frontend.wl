BeginPackage["JerryI`Notebook`Debugger`", {
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`", 
    "JerryI`Notebook`", 
    "JerryI`WLX`",
    "JerryI`WLX`Importer`",
    "JerryI`WLX`WebUI`",
    "JerryI`WLX`WLJS`",
    "JerryI`Misc`WLJS`Transport`",
    "JerryI`Notebook`AppExtensions`",
    "JerryI`Notebook`Kernel`",
    "KirillBelov`HTTPHandler`",
    "KirillBelov`HTTPHandler`Extensions`",
    "KirillBelov`Internal`",
    "Notebook`Editor`Snippets`"
}]

Begin["`Private`"]

root = $InputFileName // DirectoryName // ParentDirectory;

{utilsAddListeners, utilsResetListeners, utilsTransition} = Get[ FileNameJoin[{root, "src", "Utils.wl"}] ];

gui  = ImportComponent[FileNameJoin @ {root, "templates", "GUI.wlx"}];
gui  = gui[{utilsAddListeners, utilsResetListeners, utilsTransition}];

With[{http = AppExtensions`HTTPHandler},
    http["MessageHandler", "Debugger"] = AssocMatchQ[<|"Path" -> ("/debugger/"~~___)|>] -> gui;
];

getNotebook[controls_] := EventFire[controls, "NotebookQ", True] /. {{___, n_Notebook, ___} :> n};


listener[OptionsPattern[] ] := 
With[{
    Controls = OptionValue["Controls"],
    Modals = OptionValue["Modals"],
    Messanger = OptionValue["Messanger"],
    Path = If[DirectoryQ[#], #, DirectoryName[#] ] &@ OptionValue["Path"],
    Type = OptionValue["Type"]
},
    EventHandler[EventClone[Controls], {
        "open_debugger" -> Function[Null, 
            With[{
                notebook = getNotebook[Controls],
                cli = Global`$Client
            },
                If[!MatchQ[notebook, _Notebook],
                    EventFire[Messanger, "Warning", "Notebook not found"];
                    Return[];
                ]; 

                If[!(notebook["Evaluator"]["Kernel"]["State"] === "Initialized") || !TrueQ[notebook["WebSocketQ"] ],
                    EventFire[Messanger, "Warning", "Kernel is not ready"];  
                    Return[];
                ];

                If[TrueQ[notebook["Evaluator"]["Kernel"]["DebuggerWorking"] ],
                    EventFire[Messanger, "Warning", "Debugger is already attached to this Kernel"];  
                    Return[];
                ];

                WebUILocation[StringJoin["/debugger/", URLEncode[ BinarySerialize[<|"Notebook"->notebook, "Origin"->cli, "Messanger"->Messanger|>] // BaseEncode ]  ], cli, "Target"->_];
            ]
        ] 
    }];

    ""
];

Options[listener] = {"Path"->"", "Type"->"", "Parameters"->"", "Modals"->"", "AppEvent"->"", "Controls"->"", "Messanger"->""}
AppExtensions`TemplateInjection["AppTopBar"] = listener;


SnippetsCreateItem[
    "openDebugger", 

    "Template"->ImportComponent[ FileNameJoin @ {root, "templates", "Ico.wlx"} ] , 
    "Title"->"Debug"
];

(* just fwd *)
EventHandler[SnippetsEvents, {
    "openDebugger" -> Function[assoc, EventFire[assoc["Controls"], "open_debugger", True] ]
}];


End[]
EndPackage[]