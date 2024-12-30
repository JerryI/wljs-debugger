BeginPackage["JerryI`Notebook`Debugger`Utils`", {

}]

Begin["`Internal`"]

addListeners[kernel_] := With[{},
    If[!MemberQ[k["Properties"], "DebuggerEvents"],
        Echo["Clonning events from kernel..."];
        kernel["DebuggerEvents"] = EventClone[kernel["StandardOutput"] ];
        kernel["DebuggerStateChannel"] = CreateUUID[];
    ];
]

transition[ Rule[k_, target_] ] := Module[{
    mode = If[MemberQ[k["Properties"], "DebuggingMode"], k["DebuggingMode"], "Normal"],
    promise = Promise[]
},
    transition[promise, k, mode, target];
    promise
]

(* Oh God Jesus, I need to implement EventOnce[] *)

transition[promise_, kernel_, "Normal", "Normal"] := EventFire[promise, Resolve, True];
transition[promise_, kernel_, "Normal", "Aborted"] := With[{},
    EventHandler[kernel["DebuggerEvents"], {
        ReturnTextPacket["$Aborted"] :> Function[Null,
        
            kernel["DebuggingMode"] = "Normal";
            EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];
            EventFire[promise, Resolve, True];

            EventHandler[kernel["DebuggerEvents"], {
                ReturnTextPacket["$Aborted"] :> Null
            }]; 
        ]
    }];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];
    LinkInterrupt[kernel["Link"], 3];
    LinkWrite[kernel["Link"], EnterTextPacket["$Aborted"] ];
]

transition[promise_, kernel_, "Normal", "Inspect"] := With[{},
    EventHandler[kernel["DebuggerEvents"], {
        MenuPacket[1, _] :> Function[Null,
        
            Echo["Debugger >> Menu 1"];

            EventHandler[kernel["DebuggerEvents"], {
                MenuPacket[1, _] :> Null
            }]; 

            EventHandler[kernel["DebuggerEvents"], {
                MenuPacket[0, _] :> Function[Null,

                    Echo["Debugger >> Menu 0. Got it!"];

                    EventHandler[kernel["DebuggerEvents"], {
                        MenuPacket[0, _] :> Null
                    }];


                    EventHandler[kernel["DebuggerEvents"], {
                        TextPacket[p_] :> Function[Null,
                            Echo["Debugger >> First text packet captured!"];
                            EventHandler[kernel["DebuggerEvents"], {
                                TextPacket[p_] :> Null
                            }];

                            EventHandler[kernel["DebuggerEvents"], {
                                _BeginDialogPacket -> Function[Null,

                                    Echo["Debugger >> Dialog started!"];

                                    EventHandler[kernel["DebuggerEvents"], {
                                        _BeginDialogPacket -> Null,
                                        _EndDialogPacket -> Function[Null,

                                            EventHandler[kernel["DebuggerEvents"], {
                                                _EndDialogPacket -> Null 
                                            }];

                                            kernel["DebuggingMode"] = "Normal";
                                            EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];
                                            Echo["Debugger >> End Dialog (not from transition)"];
                                        ]                                        
                                    }];

                                    kernel["DebuggingMode"] = "Inspect";
                                    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

                                    EventFire[promise, Resolve, True];
                                ]
                            }];

                            LinkWrite[kernel["Link"], TextPacket["i"] ];  
                        ]
                    }];

                ]
            }];

            LinkWrite[kernel["Link"], MenuPacket[1] ];
        ]
    }];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
]

transition[promise_, kernel_, "Inspect", "Normal"] := With[{},
    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Return[]"];

    EventHandler[kernel["DebuggerEvents"], {
        _EndDialogPacket -> Function[Null,

            EventHandler[kernel["DebuggerEvents"], {
                _EndDialogPacket -> Null 
            }];

            kernel["DebuggingMode"] = "Normal";
            EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

            Echo["Debugger >> End Dialog"];
            EventFire[promise, Resolve, True];
        ]
    }];

    LinkWrite[link, EnterTextPacket["Return[]"] ];
];

transition[promise_, kernel_, c_, t_] := EventFire[promise, Reject, StringTemplate["Transition from `` to `` is not possible"][c,t] ];

transition[promise_, kernel_, "Normal", "Trace"] := With[{},
    EventHandler[kernel["DebuggerEvents"], {
        MenuPacket[1, _] :> Function[Null,
        
            Echo["Debugger >> Menu 1"];

            EventHandler[kernel["DebuggerEvents"], {
                MenuPacket[1, _] :> Null
            }]; 

            EventHandler[kernel["DebuggerEvents"], {
                MenuPacket[0, _] :> Function[Null,

                    Echo["Debugger >> Menu 0. Got it!"];

                    EventHandler[kernel["DebuggerEvents"], {
                        MenuPacket[0, _] :> Null
                    }];


                    EventHandler[kernel["DebuggerEvents"], {
                        TextPacket[p_] :> Function[Null,
                            Echo["Debugger >> First text packet captured!"];
                            EventHandler[kernel["DebuggerEvents"], {
                                TextPacket[p_] :> Null
                            }];


                            LinkWrite[kernel["Link"], TextPacket["t"] ];  

                            With[{ds = CreateDataStructure["Stack"]},
                                kernel["DebuggingMode"] = "Trace";
                                EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

                                Echo["Debugger >> Collecting trace"];

                                EventHandler[kernel["DebuggerEvents"], {
                                    TextPacket[p_] :> Function[Null, ds["Push", p] ]
                                }];

                                EventFire[promise, Resolve, ds];
                            ];
                        ]
                    }];

                ]
            }];

            LinkWrite[kernel["Link"], MenuPacket[1] ];
        ]
    }];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];
    
    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
];

transition[promise_, kernel_, "Trace", "Normal"] := With[{},
    EventHandler[kernel["DebuggerEvents"], {
        MenuPacket[1, _] :> Function[Null,
        
            Echo["Debugger >> Menu 1"];

            EventHandler[kernel["DebuggerEvents"], {
                MenuPacket[1, _] :> Null
            }]; 

            EventHandler[kernel["DebuggerEvents"], {
                MenuPacket[0, _] :> Function[Null,

                    Echo["Debugger >> Menu 0. Got it!"];

                    EventHandler[kernel["DebuggerEvents"], {
                        MenuPacket[0, _] :> Null
                    }];


                    EventHandler[kernel["DebuggerEvents"], {
                        TextPacket[p_] :> Function[Null,
                            Echo["Debugger >> First text packet captured!"];
                            EventHandler[kernel["DebuggerEvents"], {
                                TextPacket[p_] :> Null
                            }];


                            LinkWrite[kernel["Link"], TextPacket["c"] ];  
                            kernel["DebuggingMode"] = "Normal";
                            EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

                            EventFire[promise, Resolve, True];
                        ]
                    }];

                ]
            }];

            LinkWrite[kernel["Link"], MenuPacket[1] ];
        ]
    }];

    kernel["DebuggingMode"] = "In transition";
    EventFire[kernel["DebuggerStateChannel"], "State", kernel["DebuggingMode"] ];

    Echo["Debugger >> Link Interrupt 6"];
    LinkInterrupt[kernel["Link"], 6];
];

End[];
EndPackage[];

{JerryI`Notebook`Debugger`Utils`Internal`addListeners, JerryI`Notebook`Debugger`Utils`Internal`transition}