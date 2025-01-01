BeginPackage["JerryI`Notebook`Debugger`Utils`", {
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`", 
    "JerryI`Notebook`", 
    "JerryI`Notebook`Kernel`"
}]

Begin["`Internal`"]

addListeners[kernel_] := With[{},
    If[!MemberQ[k["Properties"], "DebuggerStreamer"],
        Echo["Clonning events from kernel..."];

        kernel["DebuggerStreamer"] = attachStreamer[kernel["StandardOutput"] ];

        kernel["DebuggerStateChannel"] = CreateUUID[];
    ];
]

resetListeners[kernel_] := With[{},
    kernel["DebuggingMode"] = "Normal";
]

transition[ Rule[k_, target_] ] := Module[{
    mode = If[MemberQ[k["Properties"], "DebuggingMode"], k["DebuggingMode"], "Normal"],
    promise = Promise[]
},
    transition[promise, k, mode, target];
    promise
]

transition[ k_ ] := Module[{
    mode = If[MemberQ[k["Properties"], "DebuggingMode"], k["DebuggingMode"], "Normal"],
    promise = Promise[]
},
    transition[promise, k, mode];
    promise
]

(* Oh God Jesus, I need to implement EventOnce[] *)

attachStreamer[event_] := With[{uid = (CreateUUID[] // Hash), cloned = EventClone[event]},
    streamer[uid]["Handlers"] = {};
    streamer[uid]["Event"] = cloned;

    EventHandler[cloned, {
        any_ :> Function[Null, #[any]& /@ (streamer[uid]["Handlers"]) ]
    }];

    streamer[uid]
]

streamer[stream_]["Destroy"] := With[{},
    Unset[streamer[uid]["Handlers"] ];
    EventRemove[streamer[uid]["Event"] ];
    Unset[streamer[uid]["Event"] ];
]

streamer[stream_][r_RuleDelayed] := With[{pattern = r[[1]], handler = Unique[]},
    streamer[stream]["Handlers"] = Append[streamer[stream]["Handlers"], handler];

    handler[data_] := With[{},
        If[MatchQ[data, pattern],
            data /. r;
            streamer[stream]["Handlers"] = streamer[stream]["Handlers"] /. {handler -> Nothing};
            Echo["Streamer >> Match!"];
        ];
    ];
]

store = <||>;

streamer[stream_][r_RuleDelayed, uid_] := With[{pattern = r[[1]], handler = Unique[]},
    streamer[stream]["Handlers"] = Append[streamer[stream]["Handlers"], handler];

    handler[data_] := With[{},
        If[MatchQ[data, pattern],
            data /. r;
            Echo["Streamer >> Match!"];
        ];
    ];

    store[uid] = handler;
]

streamer[stream_][Nothing, uid_] := With[{sym = store[uid]},
    streamer[stream]["Handlers"] = streamer[stream]["Handlers"] /. {sym -> Nothing};
    store[uid] = .;
]

transition[promise_, kernel_, mode_] := (
    kernel["DebuggingMode"] = mode;
    EventFire[kernel["DebuggerStateChannel"], "State", mode ];
    EventFire[promise, Resolve, mode ];
);

transition[promise_, kernel_, "Normal", "Normal"] := EventFire[promise, Resolve, True];
transition[promise_, kernel_, "Normal", "Aborted"] := With[{},

    kernel["DebuggerStreamer"][ReturnTextPacket["$Aborted"] :> With[{},
        Echo["Debugger >> Got $Aborted"];
        kernel["DebuggingMode"] = "Normal";
        EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];
        EventFire[promise, Resolve, True];        
    ] ];

    kernel["DebuggingMode"] = "In transition";
    Echo["Debugger >> Link Interrupt"];
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];
    LinkInterrupt[kernel["Link"], 3];
    LinkWrite[kernel["Link"], EnterTextPacket["$Aborted"] ];
]

transition[promise_, kernel_, "Normal", "Inspect"] := With[{},

    kernel["DebuggerStreamer"][MenuPacket[1, _] :> With[{},
        Echo["Debugger >> Menu 1"];   
        
        kernel["DebuggerStreamer"][MenuPacket[0, _] :> With[{},
            Echo["Debugger >> Menu 0"];   
           
            kernel["DebuggerStreamer"][TextPacket[_] :> With[{},
                Echo["Debugger >> First text packet captured!"];

                kernel["DebuggerStreamer"][_BeginDialogPacket :> With[{},
                    Echo["Debugger >> Begin dialog!"];

                    kernel["DebuggerStreamer"][_EndDialogPacket :> With[{},
                        kernel["DebuggingMode"] = "Normal";
                        EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];
                        Echo["Debugger >> End Dialog (not from transition)"];
                    ] ];

                    kernel["DebuggingMode"] = "Inspect";
                    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

                    EventFire[promise, Resolve, True];
                ] ];

                LinkWrite[kernel["Link"], TextPacket["i"] ];
            ] ];

        ] ];

        LinkWrite[kernel["Link"], MenuPacket[1] ];
    ] ];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
]

transition[promise_, kernel_, "Inspect", "Normal"] := With[{},
    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Return[]"];

    LinkWrite[kernel["Link"], EnterTextPacket["Return[]"] ];

    EventFire[promise, Resolve, True];
];

transition[promise_, kernel_, c_, t_] := (
    Echo[StringTemplate["Transition from `` to `` is not possible"][c,t] ];
    EventFire[promise, Reject, StringTemplate["Transition from `` to `` is not possible"][c,t] ];
)

transition[promise_, kernel_, "Normal", "Trace"] := With[{},
    kernel["DebuggerStreamer"][MenuPacket[1, _] :> With[{},
        Echo["Debugger >> Menu 1"];   
        
        kernel["DebuggerStreamer"][MenuPacket[0, _] :> With[{},
            Echo["Debugger >> Menu 0"];   
           
            kernel["DebuggerStreamer"][TextPacket[_] :> With[{},
                Echo["Debugger >> First text packet captured!"];

                LinkWrite[kernel["Link"], TextPacket["t"] ];

                With[{uid = CreateUUID[]},
                    kernel["DebuggerStreamer"][TextPacket[msg_] :> With[{},
                        EventFire[uid, msg];
                    ], uid];

                    kernel["DebuggingTraceStream"] = uid;

                    kernel["DebuggingMode"] = "Trace";
                    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];
                    EventFire[promise, Resolve, uid];
                ];
            ] ];

        ] ];

        LinkWrite[kernel["Link"], MenuPacket[1] ];
    ] ];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
];

transition[promise_, kernel_, "Normal", "Show"] := With[{},
    kernel["DebuggerStreamer"][MenuPacket[1, _] :> With[{},
        Echo["Debugger >> Menu 1"];   
        
        kernel["DebuggerStreamer"][MenuPacket[0, _] :> With[{},
            Echo["Debugger >> Menu 0"];   
           
            kernel["DebuggerStreamer"][TextPacket[_] :> With[{},
                Echo["Debugger >> First text packet captured!"];

                LinkWrite[kernel["Link"], TextPacket["s"] ];

                With[{uid = CreateUUID[]},
                    kernel["DebuggerStreamer"][TextPacket[msg_] :> With[{},
                        EventFire[promise, Resolve, msg];
                    ] ];

                    kernel["DebuggingMode"] = "Normal";
                    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];
                ];
            ] ];

        ] ];

        LinkWrite[kernel["Link"], MenuPacket[1] ];
    ] ];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
];

transition[promise_, kernel_, "Trace", "Normal"] := With[{},
    kernel["DebuggerStreamer"][MenuPacket[1, _] :> With[{},
        Echo["Debugger >> Menu 1"];   
        
        kernel["DebuggerStreamer"][MenuPacket[0, _] :> With[{},
            Echo["Debugger >> Menu 0"];   
           
            kernel["DebuggerStreamer"][TextPacket[_] :> With[{},
                Echo["Debugger >> First text packet captured!"];

                LinkWrite[kernel["Link"], TextPacket["c"] ];

                kernel["DebuggingMode"] = "Normal";
                EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];
                EventFire[promise, Resolve, True];
            ] ];

        ] ];

        LinkWrite[kernel["Link"], MenuPacket[1] ];
    ] ];

    With[{traceStream = kernel["DebuggingTraceStream"]},
        kernel["DebuggerStreamer"][Nothing, traceStream];
        EventRemove[traceStream]
    ];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
];

End[];
EndPackage[];

{JerryI`Notebook`Debugger`Utils`Internal`addListeners, JerryI`Notebook`Debugger`Utils`Internal`resetListeners, JerryI`Notebook`Debugger`Utils`Internal`transition}