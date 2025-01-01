BeginPackage["JerryI`Notebook`Debugger`Utils`", {
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`", 
    "JerryI`Notebook`", 
    "JerryI`Notebook`Kernel`"
}]

Begin["`Internal`"]


addBreak[kernel_, {"Assert", ev_String}] := With[{},
    LinkWrite[kernel["Link"], EnterTextPacket["On[Assert];"] ];
    LinkWrite[kernel["Link"], EnterTextPacket[StringJoin["$AssertFunction = With[{msg = {##}}, EventFire[Internal`Kernel`Stdout[\"", ev, "\"], \"Assert\", msg] ]&;"] ] ];
];

removeBreak[kernel_, {"Assert", _}] := removeBreak[kernel, "Assert"];
removeBreak[kernel_, "Assert"] := With[{},
    LinkWrite[kernel["Link"], EnterTextPacket["Off[Assert];"] ];
    LinkWrite[kernel["Link"], EnterTextPacket["$AssertFunction = Automatic;"] ];
]

addBreak[kernel_, {"Symbol", name_String, ev_String}] := With[{},
    LinkWrite[kernel["Link"], EnterTextPacket[ StringJoin["Experimental`ValueFunction[", name, "] = Function[{y,x}, EventFire[Internal`Kernel`Stdout[\"", ev, "\"], \"", name, "\", ToString[x//Short, StandardForm] ] ] ];"] ] ];
];

removeBreak[kernel_, {"Symbol", name_String, ev_String}] := removeBreak[kernel, {"Symbol", name}];
removeBreak[kernel_, {"Symbol", name_String}] := With[{},
    LinkWrite[kernel["Link"], EnterTextPacket[StringJoin["Experimental`ValueFunction[\"", name, "\"] // Unset;"] ] ];
]

addListeners[kernel_] := With[{},
    If[!MemberQ[k["Properties"], "AsyncStreamer"],
        Echo["Clonning events from kernel..."];

        kernel["AsyncStreamer"] = attachStreamer[kernel["StandardOutput"] ];

        kernel["AsyncStateChannel"] = CreateUUID[];
    ];

    <|"AsyncStreamer" -> kernel["AsyncStreamer"], "AsyncStateChannel" -> kernel["AsyncStateChannel"]|>
]

resetKernel[kernel_] := With[{},
    If[kernel["State"] === "Initialized",
        transition[kernel -> "Normal"];
    ];
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
    EventFire[kernel["AsyncStateChannel"], "State", mode ];
    EventFire[promise, Resolve, mode ];
);

transition[promise_, kernel_, "Normal", "Normal"] := EventFire[promise, Resolve, True];
transition[promise_, kernel_, "Normal", "Aborted"] := With[{},

    kernel["AsyncStreamer"][ReturnTextPacket["$Aborted"] :> With[{},
        Echo["Debugger >> Got $Aborted"];
        kernel["DebuggingMode"] = "Normal";
        EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];
        EventFire[promise, Resolve, True];        
    ] ];

    kernel["DebuggingMode"] = "In transition";
    Echo["Debugger >> Link Interrupt"];
    EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];
    LinkInterrupt[kernel["Link"], 3];
    LinkWrite[kernel["Link"], EnterTextPacket["$Aborted"] ];
]

transition[promise_, kernel_, "Normal", "Inspect"] := With[{},

    kernel["AsyncStreamer"][MenuPacket[1, _] :> With[{},
        Echo["Debugger >> Menu 1"];   
        
        kernel["AsyncStreamer"][MenuPacket[0, _] :> With[{},
            Echo["Debugger >> Menu 0"];   
           
            kernel["AsyncStreamer"][TextPacket[_] :> With[{},
                Echo["Debugger >> First text packet captured!"];

                kernel["AsyncStreamer"][_BeginDialogPacket :> With[{},
                    Echo["Debugger >> Begin dialog!"];

                    kernel["AsyncStreamer"][_EndDialogPacket :> With[{},
                        kernel["DebuggingMode"] = "Normal";
                        EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];
                        Echo["Debugger >> End Dialog (not from transition)"];
                    ] ];

                    kernel["DebuggingMode"] = "Inspect";
                    EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];

                    EventFire[promise, Resolve, True];
                ] ];

                LinkWrite[kernel["Link"], TextPacket["i"] ];
            ] ];

        ] ];

        LinkWrite[kernel["Link"], MenuPacket[1] ];
    ] ];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
]

transition[promise_, kernel_, "Inspect", "Normal"] := With[{},
    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Return[]"];

    LinkWrite[kernel["Link"], EnterTextPacket["Return[]"] ];

    EventFire[promise, Resolve, True];
];

transition[promise_, kernel_, c_, t_] := (
    Echo[StringTemplate["Transition from `` to `` is not possible"][c,t] ];
    EventFire[promise, Reject, StringTemplate["Transition from `` to `` is not possible"][c,t] ];
)

transition[promise_, kernel_, "Normal", "Trace"] := With[{},
    kernel["AsyncStreamer"][MenuPacket[1, _] :> With[{},
        Echo["Debugger >> Menu 1"];   
        
        kernel["AsyncStreamer"][MenuPacket[0, _] :> With[{},
            Echo["Debugger >> Menu 0"];   
           
            kernel["AsyncStreamer"][TextPacket[_] :> With[{},
                Echo["Debugger >> First text packet captured!"];

                LinkWrite[kernel["Link"], TextPacket["t"] ];

                With[{uid = CreateUUID[]},
                    kernel["AsyncStreamer"][TextPacket[msg_] :> With[{},
                        EventFire[uid, msg];
                    ], uid];

                    kernel["DebuggingTraceStream"] = uid;

                    kernel["DebuggingMode"] = "Trace";
                    EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];
                    EventFire[promise, Resolve, uid];
                ];
            ] ];

        ] ];

        LinkWrite[kernel["Link"], MenuPacket[1] ];
    ] ];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
];

transition[promise_, kernel_, "Normal", "Show"] := With[{},
    kernel["AsyncStreamer"][MenuPacket[1, _] :> With[{},
        Echo["Debugger >> Menu 1"];   
        
        kernel["AsyncStreamer"][MenuPacket[0, _] :> With[{},
            Echo["Debugger >> Menu 0"];   
           
            kernel["AsyncStreamer"][TextPacket[_] :> With[{},
                Echo["Debugger >> First text packet captured!"];

                LinkWrite[kernel["Link"], TextPacket["s"] ];

                With[{uid = CreateUUID[]},
                    kernel["AsyncStreamer"][TextPacket[msg_] :> With[{},
                        EventFire[promise, Resolve, msg];
                    ] ];

                    kernel["DebuggingMode"] = "Normal";
                    EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];
                ];
            ] ];

        ] ];

        LinkWrite[kernel["Link"], MenuPacket[1] ];
    ] ];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
];

transition[promise_, kernel_, "Trace", "Normal"] := With[{},
    kernel["AsyncStreamer"][MenuPacket[1, _] :> With[{},
        Echo["Debugger >> Menu 1"];   
        
        kernel["AsyncStreamer"][MenuPacket[0, _] :> With[{},
            Echo["Debugger >> Menu 0"];   
           
            kernel["AsyncStreamer"][TextPacket[_] :> With[{},
                Echo["Debugger >> First text packet captured!"];

                LinkWrite[kernel["Link"], TextPacket["c"] ];

                kernel["DebuggingMode"] = "Normal";
                EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];
                EventFire[promise, Resolve, True];
            ] ];

        ] ];

        LinkWrite[kernel["Link"], MenuPacket[1] ];
    ] ];

    With[{traceStream = kernel["DebuggingTraceStream"]},
        kernel["AsyncStreamer"][Nothing, traceStream];
        EventRemove[traceStream]
    ];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["AsyncStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
];

End[];
EndPackage[];

{JerryI`Notebook`Debugger`Utils`Internal`addListeners, JerryI`Notebook`Debugger`Utils`Internal`resetKernel, JerryI`Notebook`Debugger`Utils`Internal`transition, JerryI`Notebook`Debugger`Utils`Internal`addBreak, JerryI`Notebook`Debugger`Utils`Internal`removeBreak}