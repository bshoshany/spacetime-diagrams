(* An interactive illustration of various effects in special relativity using spacetime diagrams. *)
(*                                       v1.0.0, 2022-06-05                                       *)
(*               Copyright (c) 2022 Barak Shoshany. Licensed under the MIT license.               *)
(*                     baraksh@gmail.com | baraksh.com | github.com/bshoshany                     *)

DynamicModule[
    {
        (* The arrival time of tachyon 2, i.e. its intersection with the t axis. *)
        ArrivalTime = 0,
        (* The sizes of the arrowheads. *)
        ArrowSize = 0.03,
        (* The color of the barn in the barn paradox illustration. *)
        BarnColor = Brown,
        (* The length, in grid units, of the barn in the barn paradox illustration. *)
        BarnLength = 2,
        (* The color of the pole in the barn paradox illustration. *)
        BarnPoleColor = Gray,
        (* The length, in grid units, of the pole in the barn paradox illustration. *)
        BarnPoleLength = 3,
        (* The time, in grid units, of the end of the pole in the barn paradox illustration. *)
        BarnPoleTime = 0,
        (* A function to generate a colored button to toggle a value. Defined in the module body. *)
        ColorButton,
        (* The range of the diagram, negative to positive. *)
        DiagramRange = 2,
        (* The size of the diagram, both width and height. *)
        DiagramSize = 700,
        (* A quick and dirty function to allow division even if the numerator is zero - in which case it will simply return a very large number with the correct sign. Defined in the module body. *)
        DivideNoZero,
        (* A function to draw a clock at a particular spacetime point. Defined in the module body. *)
        DrawClock,
        (* The color of the highlighted future region. *)
        FutureColor = Yellow,
        (* The opacity of the highlighted future region. *)
        FutureOpacity = 0.3,
        (* Whether to show the highlighted future region. *)
        FutureShow = False,
        (* The number of squares in the grid for each frame. *)
        GridNumber = 8,
        (* The opacity of the coordinate grid of each frame. *)
        GridOpacity = 0.5,
        (* The color of the coordinate grid of frame S. *)
        GridSColor = Gray,
        (* The color of the x grid lines of frame S'. *)
        GridSPrimeColorX = Blue,
        (* The color of the t grid lines of frame S'. *)
        GridSPrimeColorT = Orange,
        (* Whether to show the coordinate grid of frame S. *)
        GridSShow = True,
        (* Whether to show the coordinate grid of frame S'. *)
        GridSPrimeShow = False,
        (* Whether to show the ticks for the S' axes. *)
        GridSPrimeTicksShow = False,
        (* A factor to multiply DiagramRange by in order to increase the spacing of the labels from the axes. *)
        LabelMultiplier = 1.04,
        (* The size of the squares used to indicate the color of each part of the plot in the legend. *)
        LegendSize = 25,
        (* The color of the length contraction illustration. *)
        LengthContColor = RGBColor[0, 0.5, 0],
        (* The proper distance, in grid units, in the length contraction illustration. *)
        LengthContDistance = 3,
        (* The font size of the labels. *)
        LetterSize = 16,
        (* The color of the light cone edges. *)
        LightConeEdgeColor = Pink,
        (* Whether to show the light cone edges. *)
        LightConeEdgeShow = False,
        (* The color of the light cone interiors. *)
        LightConeInteriorColor = Pink,
        (* The opacity of the light cone interiors. *)
        LightConeInteriorOpacity = 0.3,
        (* Whether to show the light cone interior. *)
        LightConeInteriorShow = False,
        (* The Lorentz factor corresponding to the relative velocity between the frames. *)
        LorentzFactor = 1,
        (* A function to Lorentz-transform the x and t coordinates. Defined in the module body. *)
        LorentzTransform,
        (* The inverse of the Lorentz transformation. Defined in the module body. *)
        LorentzTransformInverse,
        (* A function to format and pad the numbers displayed above the plot, including plus signs. Defined in the module body. *)
        PadNumber,
        (* A function to format and pad the numbers displayed above the plot, without plus signs. Defined in the module body. *)
        PadNumberPositive,
        (* Tachyon 1's velocity in the S frame. *)
        Path1V = 0,
        (* Tachyon 2's velocity in the S' frame. *)
        Path2V = 0,
        (* The color of the simultaneity illustration. *)
        SimultaneityColor = Purple,
        (* The separation, in grid units, between each event in the simultaneity illustration and the origin of S. *)
        SimultaneitySep = 2,
        (* Spaces to insert between elements in the menu. *)
        Spaces = "     ",
        (* The t coordinate of the S' station in the S frame. *)
        SPrimeT = 0,
        (* The velocity of the S' station in the S frame. *)
        SPrimeV = 0,
        (* The x coordinate of the S' station in the S frame. *)
        SPrimeX = 0,
        (* The color of the time dilation illustration. *)
        TimeDilationColor = Red,
        (* The duration, in grid units, of proper time in the time dilation illustration. *)
        TimeDilationDuration = 3,
        (* The color of the tachyon paths. *)
        TachyonColor = Blue,
        (* The color of the twin paradox illustration. *)
        TwinColor = RGBColor[0, 0.75, 1, 1],
        (* The distance, in grid units, between the Earth and the distant star in the twin paradox. *)
        TwinDistance = 3,
        (* Which demonstration to show. *)
        WhichDemo = "No Demo",
        (* Which observer's perspective to show for the demonstrations. 0 means S, 1 means S'. *)
        WhichObserver = 0,
        (* A function to update the second path and ensure it is always consistent. Defined in the module body. *)
        UpdateSecondPath
    },
    (* A function to generate a colored button to toggle a value. *)
    ColorButton[val_, color_, text_] := Row[{
        Toggler[
            Dynamic[val],
            {
                False -> Graphics[{White, Disk[{0, 0}, 1], EdgeForm[Thickness[0.03]], Disk[{0, 0}, 0.9]}],
                True -> If[Head[color] =!= List,
                    Graphics[{color, Disk[{0, 0}, 1], EdgeForm[Thickness[0.03]], Disk[{0, 0}, 0.9]}],
                    Graphics[{{color[[1]], Disk[{0, 0}, 1, {Pi / 2, 3 * Pi / 2}], EdgeForm[Thickness[0.03]], Disk[{0, 0}, 0.9, {Pi / 2, 3 * Pi / 2}]}, {color[[2]], Disk[{0, 0}, 1, {3 * Pi / 2, 5 * Pi / 2}], EdgeForm[Thickness[0.03]], Disk[{0, 0}, 0.9, {3 * Pi / 2, 5 * Pi / 2}]}}]
                ]
            },
            ImageSize -> LegendSize
        ], " ", text
    }];
    SetAttributes[ColorButton, HoldFirst];
    (* A quick and dirty function to allow division even if the numerator is zero - in which case it will simply return a very large number with the correct sign. *)
    DivideNoZero[numerator_, denominator_] := If[denominator == 0, Sign[numerator] * 100000., numerator / denominator];
    (* A function to draw a clock at a particular spacetime point. *)
    DrawClock[{x_, t_}, time_] := {
        Circle[{x, t}, DiagramRange / GridNumber * r],
        Thickness[0.002],
        Gray,
        Table[Line[({x, t} + # * {Sin[a], Cos[a]} * DiagramRange / GridNumber * r) & /@ {0.8, 0.9}], {a, 0, 2 Pi, Pi / 6}],
        Black,
        Line[{{x, t}, {x, t} + 0.7 * ({Sin[a], Cos[a]} /. a -> (1 + time / DiagramRange * GridNumber / (GridNumber - 1)) * Pi) * DiagramRange / GridNumber * r}]
    } /. r -> 0.6;
    (* A function to Lorentz-transform the x and t coordinates. More precisely, this is an inhomogeneous Lorentz transformation, a.k.a. a Poincare transformation, since we are also translating in spacetime. *)
    LorentzTransform[{x_, t_}, sign_: 1] := {
        LorentzFactor * (x + sign * SPrimeV * t) + SPrimeX,
        LorentzFactor * (t + sign * SPrimeV * x) + SPrimeT
    };
    (* The inverse of the Lorentz transformation. *)
    LorentzTransformInverse[{x_, t_}, sign_: 1] := {
        LorentzFactor * ((x - SPrimeX) - sign * SPrimeV * (t- SPrimeT)),
        LorentzFactor * ((t- SPrimeT) - sign * SPrimeV * (x - SPrimeX))
    };
    (* A function to format and pad the numbers displayed above the plot, including plus signs. *)
    PadNumber[n_] := PaddedForm[N[n], {4, 2}, NumberPadding -> {"", "0"}, NumberSigns -> {"-", "+"}];
    (* A function to format and pad the numbers displayed above the plot, without plus signs. *)
    PadNumberPositive[n_] := PaddedForm[N[n], {4, 2}, NumberPadding -> {"", "0"}];
    (* A function to update the second path and ensure it is always consistent. *)
    UpdateSecondPath[] := (
        (* After changing some parameters, the previous arrival time may no longer be physically possible. If that is the case, it will be moved up to the minimal possible time. *)
        ArrivalTime = Max[ArrivalTime, SPrimeT - SPrimeX * SPrimeV];
        (* Calculate the new velocity of the second path. Since this may result in division by zero, we suppress error messages. *)
        Path2V = Quiet[(SPrimeX + (ArrivalTime - SPrimeT) * SPrimeV) / (SPrimeT - (ArrivalTime + SPrimeX * SPrimeV))];
        (* If there was a division by zero, then Path2V will be Indeterminate. We set it to 0 instead. *)
        If[Path2V === Indeterminate, Path2V = 0];
    );
    Deploy[Column[{
        Dynamic[Style[Grid[{
            (* Controls to select which demonstration to show. *)
            {Row[{
                Setter[Dynamic[WhichDemo], "No Demo"],
                " ",
                Setter[Dynamic[WhichDemo], "Clocks"],
                " ",
                Setter[Dynamic[WhichDemo], "Simultaneity"],
                " ",
                Setter[Dynamic[WhichDemo], "Time Dilation"],
                " ",
                Setter[Dynamic[WhichDemo], "Length Contraction"],
                " ",
                Setter[Dynamic[WhichDemo, (WhichDemo = #; {SPrimeX, SPrimeT} = {TwinDistance * DiagramRange / GridNumber, 0};) &], "Twin \"Paradox\""],
                " ",
                Setter[Dynamic[WhichDemo, (WhichDemo = #; {SPrimeX, SPrimeT} = {0, 0};) &], "Barn \"Paradox\""],
                " ",
                Setter[Dynamic[WhichDemo, (WhichDemo = #; SPrimeT = Max[SPrimeT, 0];) &], "Time Travel"]
            }]},
            (* Controls to enable or disable the various parts of the plot. *)
            {Row[{
                ColorButton[GridSShow, GridSColor, "S grid"],
                " ",
                ColorButton[GridSPrimeShow, {GridSPrimeColorX, GridSPrimeColorT}, "S' grid"],
                " ",
                ColorButton[GridSPrimeTicksShow, Black, "S' ticks"],
                " ",
                ColorButton[FutureShow, FutureColor, "S' future region"],
                " ",
                ColorButton[LightConeEdgeShow, LightConeEdgeColor, "Cone edges"],
                " ",
                ColorButton[LightConeInteriorShow, LightConeInteriorColor, "Cone interiors"]
            }]},
            (* Display the position, velocity, and Lorentz factor of S'.*)
            {Row[{
                "S': (t = ",
                Dynamic[PadNumber[SPrimeT * GridNumber / DiagramRange]],
                ", x = ",
                Dynamic[PadNumber[SPrimeX * GridNumber / DiagramRange]],
                ")",
                Button["Reset", SPrimeT = SPrimeX = 0],
                Spaces,
                "v = ",
                Dynamic[PadNumber[SPrimeV]],
                "c ",
                Button["Reset", SPrimeV = 0; LorentzFactor = 1],
                Spaces,
                "\[Gamma] = ",
                Dynamic[PadNumberPositive[LorentzFactor]]
            }]},
            If[WhichDemo != "No Demo", {Row[{
                Setter[Dynamic[WhichObserver], 0,
                    If[WhichDemo != "Simultaneity",
                        "View from the perspective of S",
                        "Events simultaneous in S"
                    ]
                ],
                " ",
                Setter[Dynamic[WhichObserver], 1,
                    If[WhichDemo != "Simultaneity",
                        "View from the perspective of S'",
                        "Events simultaneous in S'"
                    ]
                ]
            }]}, Nothing],
            (* Information for the relativity of simultaneity demonstration. *)
            If[WhichDemo == "Simultaneity", {Style[Row[{
                Row[If[WhichObserver == 0, {
                    Subscript["x", "A"],
                    " = ",
                    Dynamic[PadNumber[-SimultaneitySep]],
                    ", ",
                    Subscript["x", "B"],
                    " = ",
                    Dynamic[PadNumber[ SimultaneitySep]],
                    Spaces,
                    Subscript["t", "A"],
                    " = ",
                    Subscript["t", "B"],
                    " = 0"
                }, {
                    Subscript["x", "A"],
                    " = ",
                    Dynamic[PadNumber[LorentzTransform[{-SimultaneitySep * DiagramRange / GridNumber, 0}][[1]] / DiagramRange * GridNumber]],
                    ", ",
                    Subscript["x", "B"],
                    " = ",
                    Dynamic[PadNumber[LorentzTransform[{ SimultaneitySep * DiagramRange / GridNumber, 0}][[1]] / DiagramRange * GridNumber]],
                    Spaces,
                    Subscript["t", "A"],
                    " = ",
                    Dynamic[PadNumber[LorentzTransform[{-SimultaneitySep * DiagramRange / GridNumber, 0}][[2]] / DiagramRange * GridNumber]],
                    ", ",
                    Subscript["t", "B"],
                    " = ",
                    Dynamic[PadNumber[LorentzTransform[{ SimultaneitySep * DiagramRange / GridNumber, 0}][[2]] / DiagramRange * GridNumber]]
                }]],
                Spaces,
                Row[If[WhichObserver == 0, {
                    Subsuperscript["x", "A", "\[Prime]"],
                    " = ",
                    Dynamic[PadNumber[LorentzTransformInverse[{-SimultaneitySep * DiagramRange / GridNumber, 0}][[1]] / DiagramRange * GridNumber]],
                    ", ",
                    Subsuperscript["x", "B", "\[Prime]"],
                    " = ",
                    Dynamic[PadNumber[LorentzTransformInverse[{ SimultaneitySep * DiagramRange / GridNumber, 0}][[1]] / DiagramRange * GridNumber]],
                    Spaces,
                    Subsuperscript["t", "A", "\[Prime]"],
                    " = ",
                    Dynamic[PadNumber[LorentzTransformInverse[{-SimultaneitySep * DiagramRange / GridNumber, 0}][[2]] / DiagramRange * GridNumber]],
                    ", ",
                    Subsuperscript["t", "B", "\[Prime]"],
                    " = ",
                    Dynamic[PadNumber[LorentzTransformInverse[{ SimultaneitySep * DiagramRange / GridNumber, 0}][[2]] / DiagramRange * GridNumber]]
                }, {
                    Subsuperscript["x", "A", "\[Prime]"],
                    " = ",
                    Dynamic[PadNumber[-SimultaneitySep]],
                    ", ",
                    Subsuperscript["x", "B", "\[Prime]"],
                    " = ",
                    Dynamic[PadNumber[ SimultaneitySep]],
                    Spaces,
                    Subsuperscript["t", "A", "\[Prime]"],
                    " = ",
                    Subsuperscript["t", "B", "\[Prime]"],
                    " = 0"
                }]]
            }], FontColor -> SimultaneityColor]}, Nothing],
            (* Information for the time dilation demonstration. *)
            If[WhichDemo == "Time Dilation", {Style[Row[{
                "\[CapitalDelta]t = ",
                If[WhichObserver == 0,
                    Dynamic[PadNumberPositive[TimeDilationDuration * LorentzFactor]],
                    Dynamic[PadNumberPositive[TimeDilationDuration]]
                ],
                If[WhichObserver == 0,
                    Nothing,
                    " (proper time)"
                ],
                Spaces,
                "\[CapitalDelta]t' = ",
                If[WhichObserver == 0,
                    Dynamic[PadNumberPositive[TimeDilationDuration]],
                    Dynamic[PadNumberPositive[TimeDilationDuration * LorentzFactor]]
                ],
                If[WhichObserver == 1,
                    Nothing,
                    " (proper time)"
                ]
            }], FontColor -> TimeDilationColor]}, Nothing],
            (* Information for the length contraction demonstration. *)
            If[WhichDemo == "Length Contraction", {Style[Row[{
                "\[CapitalDelta]x = ",
                If[WhichObserver == 0,
                    Dynamic[PadNumberPositive[LengthContDistance / LorentzFactor]],
                    Dynamic[PadNumberPositive[LengthContDistance]]
                ],
                If[WhichObserver == 0,
                    Nothing,
                    " (proper length)"
                ],
                Spaces,
                "\[CapitalDelta]x' = ",
                If[WhichObserver == 0,
                    Dynamic[PadNumberPositive[LengthContDistance]],
                    Dynamic[PadNumberPositive[LengthContDistance / LorentzFactor]]
                ],
                If[WhichObserver == 0,
                    " (proper length)",
                    Nothing
                ]
            }], FontColor -> LengthContColor]}, Nothing],
            (* Information for the twin paradox demonstration. *)
            If[WhichDemo == "Twin \"Paradox\"", {Style[Row[{
                "For each leg of the trip: ",
                Row[If[WhichObserver == 0,
                    {
                        "\[CapitalDelta]x = ",
                        PadNumberPositive[TwinDistance],
                        Spaces,
                        "\[CapitalDelta]t = ",
                        PadNumberPositive[If[Abs[SPrimeV] < 0.01, \[Infinity], Abs[TwinDistance / SPrimeV]]],
                        Spaces,
                        "total aging = ",
                        PadNumberPositive[If[Abs[SPrimeV] < 0.01, \[Infinity], Abs[2 * TwinDistance / SPrimeV]]]
                    }, {
                        "\[CapitalDelta]x' = ",
                        PadNumberPositive[TwinDistance / LorentzFactor],
                        Spaces,
                        "\[CapitalDelta]t' = ",
                        PadNumberPositive[If[Abs[SPrimeV] < 0.01, \[Infinity], Abs[TwinDistance / SPrimeV / LorentzFactor]]],
                        Spaces,
                        "total aging = ",
                        PadNumberPositive[If[Abs[SPrimeV] < 0.01, \[Infinity], Abs[2 * TwinDistance / SPrimeV / LorentzFactor]]]
                    }
                ]]
            }], FontColor -> TwinColor]}, Nothing],
            (* Information for the barn paradox demonstration. *)
            If[WhichDemo == "Barn \"Paradox\"", {Style[Row[{
                "Barn length = ",
                If[WhichObserver == 0,
                    PadNumberPositive[BarnLength],
                    PadNumberPositive[BarnLength / LorentzFactor]
                ],
                Spaces,
                "Pole length = ",
                If[WhichObserver == 0,
                    PadNumberPositive[BarnPoleLength / LorentzFactor],
                    PadNumberPositive[BarnPoleLength]
                ],
                Spaces,
                Style[
                    If[BarnPoleLength / LorentzFactor <= BarnLength,
                        "Pole fits!",
                        "Pole doesn't fit!"
                    ],
                    Bold
                ]
            }], FontColor -> BarnColor]}, Nothing],
            (* Information for the time travel demonstration. *)
            If[WhichDemo == "Time Travel", {Style[Row[{
                Subscript["v", 1],
                " = ",
                Dynamic[If[Path1V === ComplexInfinity || Path1V > 99.99, "   \[Infinity]   ", Row[{PadNumber[Path1V], "c"}]]],
                Spaces,
                Subscript["v", 2],
                " = ",
                Dynamic[If[Path2V === ComplexInfinity || Path2V > 99.99, "   \[Infinity]   ", Row[{PadNumber[Path2V], "c"}]]],
                Spaces,
                "Arrival time = ",
                Dynamic[PadNumber[ArrivalTime]]
            }], FontColor -> TachyonColor]}, Nothing]
        }], FontSize -> 12]],
        (* Now we display the actual plot. The graphics constructs are given as nested lists, in order for the directives, such as opacity, to only apply to that specific construct. *)
        Dynamic[Graphics[{
            (* Draw the axes for the S frame. *)
            {
                Black,
                Arrowheads[{-ArrowSize, ArrowSize}],
                Arrow[{
                    {-DiagramRange, 0},
                    { DiagramRange, 0}
                }],
                Arrow[{
                    {0, -DiagramRange},
                    {0,  DiagramRange}
                }]
            },
            (* Draw the axis labels for the S frame. *)
            {
                Text[
                    Style["x", FontSize -> LetterSize],
                    {DiagramRange * LabelMultiplier, 0}
                ],
                Text[
                    Style["t", FontSize -> LetterSize],
                    {0, DiagramRange * LabelMultiplier}
                ]
            },
            (* Draw the light cone edges. *)
            If[LightConeEdgeShow, {
                Thickness[0.0025],
                LightConeEdgeColor,
                Line[{
                    {-DiagramRange, -DiagramRange},
                    { DiagramRange,  DiagramRange}
                }],
                Line[{
                    {-DiagramRange,  DiagramRange},
                    { DiagramRange, -DiagramRange}
                }]
            }],
            (* Draw the light cone interiors. *)
            If[LightConeInteriorShow, {
                Opacity[LightConeInteriorOpacity],
                LightConeInteriorColor,
                Triangle[{
                    {-DiagramRange, -DiagramRange},
                    {0, 0},
                    { DiagramRange, -DiagramRange}
                }],
                Triangle[{
                    {-DiagramRange,  DiagramRange},
                    {0, 0},
                    { DiagramRange,  DiagramRange}
                }]
            }],
            (* Draw the axes for the S' frame. *)
            If[WhichDemo != "Twin \"Paradox\"", {
                Black,
                Arrowheads[{-ArrowSize, ArrowSize}],
                If[WhichDemo != "Barn \"Paradox\"", Arrow[{
                    LorentzTransform[{-DiagramRange, 0}],
                    LorentzTransform[{ DiagramRange, 0}]
                }]],
                Arrow[{
                    LorentzTransform[{0, -DiagramRange}],
                    LorentzTransform[{0,  DiagramRange}]
                }]
            }],
            (* Draw the axis labels for the S' frame. *)
            If[WhichDemo != "Twin \"Paradox\"", {
                Text[
                    Style["x'", FontSize -> LetterSize],
                    Min[{#, DiagramRange}] & /@ (LorentzTransform[{DiagramRange / LorentzFactor, 0}] - {SPrimeX, DiagramRange / GridNumber / 2 + SPrimeX * SPrimeV})
                ],
                Text[
                    Style["t'", FontSize -> LetterSize],
                    Min[{#, DiagramRange}] & /@ (LorentzTransform[{0, DiagramRange / LorentzFactor}] - {DiagramRange / GridNumber / 2 + SPrimeT * SPrimeV, SPrimeT})
                ]
            }],
            (* Draw the labels for the S and S' frames. *)
            If[SPrimeX != 0 || SPrimeT != 0, {
                    Text[
                    Style["S", FontSize -> 1.5 LetterSize],
                    0.05 * DiagramRange * {-1, -1}
                ],
                Text[
                    Style["S'", FontSize -> 1.5 LetterSize],
                    0.05 * DiagramRange * { 1, -1} + {SPrimeX, SPrimeT}
                ]
            }],
            (* Draw the light cone edges for the S' frame. *)
            If[LightConeEdgeShow, {
                LightConeEdgeColor,
                Line[{
                    {SPrimeX - DiagramRange, SPrimeT - DiagramRange},
                    {SPrimeX + DiagramRange, SPrimeT + DiagramRange}
                }],
                Line[{
                    {SPrimeX - DiagramRange, SPrimeT + DiagramRange},
                    {SPrimeX + DiagramRange, SPrimeT - DiagramRange}
                }]
            }],
            (* Draw the light cone interiors for the S' frame. *)
            If[LightConeInteriorShow, {
                Opacity[LightConeInteriorOpacity],
                LightConeInteriorColor,
                Triangle[{
                    {SPrimeX - DiagramRange, SPrimeT - DiagramRange},
                    {SPrimeX, SPrimeT},
                    {SPrimeX + DiagramRange, SPrimeT - DiagramRange}
                }],
                Triangle[{
                    {SPrimeX - DiagramRange, SPrimeT + DiagramRange},
                    {SPrimeX, SPrimeT},
                    {SPrimeX + DiagramRange, SPrimeT + DiagramRange}
                }]
            }],
            (* Draw the future regions for the S' frame as a Lorentz-transformed rectangle above the x' axis. *)
            If[FutureShow, {
                Opacity[FutureOpacity],
                FutureColor,
                Polygon[{
                    LorentzTransform[{-DiagramRange, 0}],
                    LorentzTransform[{ DiagramRange, 0}],
                    LorentzTransform[{ DiagramRange, DiagramRange}],
                    LorentzTransform[{-DiagramRange, DiagramRange}]
                }]
            }],
            (* Draw the tachyon paths. *)
            If[WhichDemo == "Time Travel", {
                TachyonColor,
                Thickness[0.005],
                Arrowheads[ArrowSize],
                Arrow[{{0, 0}, {SPrimeX, SPrimeT}}],
                Arrow[{{SPrimeX, SPrimeT}, {0, ArrivalTime}}]
            }],
            (* Draw the coordinate grid for frame S. *)
            If[GridSShow, {
                Opacity[GridOpacity],
                GridSColor,
                Thickness[0.0015],
                Table[
                    Line[{{x, -DiagramRange}, {x, DiagramRange}}],
                    {x, -DiagramRange, DiagramRange, DiagramRange / GridNumber}
                ],
                Table[
                    Line[{{-DiagramRange, t}, {DiagramRange, t}}],
                    {t, -DiagramRange, DiagramRange, DiagramRange / GridNumber}
                ]
            }],
            (* Draw the clocks example. *)
            If[WhichDemo == "Clocks",
                Table[
                    DrawClock[LorentzTransform[{x, t}], If[WhichObserver == 0, LorentzTransform[{x, t}][[2]], t]],
                    {x, -DiagramRange + DiagramRange / GridNumber, DiagramRange, DiagramRange / GridNumber * 2},
                    {t, -DiagramRange + DiagramRange / GridNumber, DiagramRange, DiagramRange / GridNumber * 2}
                ]
            ],
            (* Draw the relativity of simultaneity example. *)
            If[WhichDemo == "Simultaneity", {
                SimultaneityColor,
                PointSize[0.02],
                If[WhichObserver == 0, {
                        Point[{-SimultaneitySep * DiagramRange / GridNumber, 0}],
                        Point[{ SimultaneitySep * DiagramRange / GridNumber, 0}],
                        Text[
                            Style["A", FontSize -> LetterSize],
                            {-SimultaneitySep * DiagramRange / GridNumber, -DiagramRange * 0.05}
                        ],
                        Text[
                            Style["B", FontSize -> LetterSize],
                            { SimultaneitySep * DiagramRange / GridNumber, -DiagramRange * 0.05}
                        ]
                    }, {
                        Point[LorentzTransform[{-SimultaneitySep * DiagramRange / GridNumber, 0}]],
                        Point[LorentzTransform[{ SimultaneitySep * DiagramRange / GridNumber, 0}]],
                        Text[
                            Style["A", FontSize -> LetterSize],
                            LorentzTransform[{-SimultaneitySep * DiagramRange / GridNumber, -DiagramRange * 0.05}]
                        ],
                        Text[
                            Style["B", FontSize -> LetterSize],
                            LorentzTransform[{ SimultaneitySep * DiagramRange / GridNumber, -DiagramRange * 0.05}]
                        ]
                }],
                Dashed,
                If[WhichObserver == 0, {
                    Line[LorentzTransform /@ {{-DiagramRange, #}, {DiagramRange, #}} & [LorentzTransformInverse[{-SimultaneitySep * DiagramRange / GridNumber, 0}][[2]]]],
                    Line[LorentzTransform /@ {{-DiagramRange, #}, {DiagramRange, #}} & [LorentzTransformInverse[{ SimultaneitySep * DiagramRange / GridNumber, 0}][[2]]]],
                    Line[LorentzTransform /@ {{#, -DiagramRange}, {#, DiagramRange}} & [LorentzTransformInverse[{-SimultaneitySep * DiagramRange / GridNumber, 0}][[1]]]],
                    Line[LorentzTransform /@ {{#, -DiagramRange}, {#, DiagramRange}} & [LorentzTransformInverse[{ SimultaneitySep * DiagramRange / GridNumber, 0}][[1]]]]
                }, {
                    Line[{{-DiagramRange, #}, {DiagramRange, #}}] & [LorentzTransform[{-SimultaneitySep * DiagramRange / GridNumber, 0}][[2]]],
                    Line[{{-DiagramRange, #}, {DiagramRange, #}}] & [LorentzTransform[{ SimultaneitySep * DiagramRange / GridNumber, 0}][[2]]],
                    Line[{{#, -DiagramRange}, {#, DiagramRange}}] & [LorentzTransform[{-SimultaneitySep * DiagramRange / GridNumber, 0}][[1]]],
                    Line[{{#, -DiagramRange}, {#, DiagramRange}}] & [LorentzTransform[{ SimultaneitySep * DiagramRange / GridNumber, 0}][[1]]]
                }]
            }],
            (* Draw the time dilation example. *)
            If[WhichDemo == "Time Dilation", {
                {
                    TimeDilationColor,
                    Thickness[0.005],
                    Arrowheads[{-1, 1} * 0.025],
                    If[WhichObserver == 0,
                        Line[LorentzTransform /@ {{0, 0}, {0, TimeDilationDuration * DiagramRange / GridNumber}}],
                        Arrow[LorentzTransform /@ {{0, 0}, {0, TimeDilationDuration * DiagramRange / GridNumber * LorentzFactor}}]
                    ],
                    If[WhichObserver == 0,
                        Arrow[{{0, LorentzTransform[{0, 0}][[2]]}, {0, LorentzTransform[{0, TimeDilationDuration * DiagramRange / GridNumber}][[2]]}}],
                        Line[{{0, SPrimeT - SPrimeX * SPrimeV}, {0, SPrimeT - SPrimeX * SPrimeV + TimeDilationDuration * DiagramRange / GridNumber}}]
                    ]
                },
                {
                    Dashed,
                    If[WhichObserver == 0,
                        {
                            Line[{{-DiagramRange, #}, {DiagramRange, #}}] & [LorentzTransform[{0, TimeDilationDuration * DiagramRange / GridNumber}][[2]]],
                            Line[{{-DiagramRange, #}, {DiagramRange, #}}] & [SPrimeT]
                        },
                        {
                            Line[{LorentzTransform[{-DiagramRange, #}], LorentzTransform[{DiagramRange, #}]}] & [TimeDilationDuration * DiagramRange / GridNumber * LorentzFactor]
                        }
                    ]
                }
            }],
            (* Draw the length contraction example. *)
            If[WhichDemo == "Length Contraction", {
                {
                    LengthContColor,
                    Thickness[0.005],
                    Arrowheads[{-1, 1} * 0.025],
                    If[WhichObserver == 0,
                        Line[LorentzTransform /@ {{0, 0}, {LengthContDistance * DiagramRange / GridNumber, 0}}],
                        Arrow[LorentzTransform /@ {{0, 0}, {LengthContDistance * DiagramRange / GridNumber / LorentzFactor, 0}}]
                    ],
                    If[WhichObserver == 0,
                        Arrow[{{SPrimeX - SPrimeT * SPrimeV, 0}, {SPrimeX - SPrimeT * SPrimeV + LengthContDistance * DiagramRange / GridNumber / LorentzFactor, 0}}],
                        Line[{{LorentzTransform[{0, 0}][[1]], 0}, {LorentzTransform[{LengthContDistance * DiagramRange / GridNumber / LorentzFactor, 0}][[1]], 0}}]
                    ]
                },
                {
                    Dashed,
                    If[WhichObserver == 0,
                        {
                            Line[LorentzTransform /@ {{#, -DiagramRange}, {#, DiagramRange}} & [LengthContDistance * DiagramRange / GridNumber]]
                        },
                        {
                            Line[{{#, -DiagramRange#}, {#, DiagramRange}}] & [LorentzTransform[{LengthContDistance * DiagramRange / GridNumber / LorentzFactor, 0}][[1]]],
                            Line[{{#, -DiagramRange}, {#, DiagramRange}}] & [SPrimeX]
                        }
                    ]
                }
            }],
            (* Draw the twin paradox example. *)
            If[WhichDemo == "Twin \"Paradox\"", {
                {
                    TwinColor,
                    Thickness[0.005],
                    Arrow[{{0, DivideNoZero[-TwinDistance * DiagramRange / GridNumber, Abs[SPrimeV]]}, {TwinDistance * DiagramRange / GridNumber * Sign[SPrimeV], 0}}],
                    Arrow[{{TwinDistance * DiagramRange / GridNumber * Sign[SPrimeV], 0}, {0, DivideNoZero[TwinDistance * DiagramRange / GridNumber, Abs[SPrimeV]]}}]
                },
                {
                    Dashed,
                    If[WhichObserver == 0,
                        ({
                            Line[{{-DiagramRange, -#}, {DiagramRange, -#}}],
                            Line[{{-DiagramRange,  #}, {DiagramRange,  #}}]
                        }) & [DivideNoZero[TwinDistance * DiagramRange / GridNumber, SPrimeV]],
                        ({
                            Line[LorentzTransform /@ {{-DiagramRange, #}, {DiagramRange, #}}] & [LorentzTransformInverse[{0, DivideNoZero[-TwinDistance * DiagramRange / GridNumber, Abs[SPrimeV]]}][[2]]],
                            Line[LorentzTransform /@ {{-DiagramRange, #}, {DiagramRange, #}}] & [LorentzTransformInverse[{SPrimeX, 0}][[2]]],
                            Line[LorentzTransform[#, -1] & /@ {{-DiagramRange, #}, {DiagramRange, #}}] & [LorentzTransformInverse[{0, DivideNoZero[TwinDistance * DiagramRange / GridNumber, Abs[SPrimeV]]}, -1][[2]]],
                            Line[LorentzTransform[#, -1] & /@ {{-DiagramRange, #}, {DiagramRange, #}}] & [LorentzTransformInverse[{SPrimeX, 0}, -1][[2]]]
                        })
                    ]
                }
            }],
            (* Draw the barn paradox example. *)
            If[WhichDemo == "Barn \"Paradox\"", {
                {
                    Thickness[0.005],
                    BarnPoleColor,
                    If[WhichObserver == 0,
                        Line[{#, {#[[1]] - BarnPoleLength / LorentzFactor, #[[2]]}} & [LorentzTransform[{0, BarnPoleTime}]] * DiagramRange / GridNumber],
                        Line[LorentzTransform /@ {{0, BarnPoleTime}, {-BarnPoleLength, BarnPoleTime}} * DiagramRange / GridNumber]
                    ],
                    Opacity[0.3],
                    If[BarnPoleLength / LorentzFactor <= BarnLength,
                        Polygon[LorentzTransform /@ {{0, -DiagramRange}, {0, DiagramRange}, {-BarnPoleLength * DiagramRange / GridNumber, DiagramRange}, {-BarnPoleLength * DiagramRange / GridNumber, -DiagramRange}}],
                        If[WhichObserver == 0,
                            Polygon[LorentzTransform /@ {{0, -DiagramRange}, {0, 0}, LorentzTransformInverse[{-BarnPoleLength * DiagramRange / GridNumber / LorentzFactor, 0}], {-BarnPoleLength * DiagramRange / GridNumber, -DiagramRange}}],
                            Polygon[LorentzTransform /@ {{0, -DiagramRange}, {0, LorentzTransformInverse[{-BarnLength * DiagramRange / GridNumber, 0}][[2]]}, {-BarnPoleLength * DiagramRange / GridNumber, LorentzTransformInverse[{-BarnLength * DiagramRange / GridNumber, 0}][[2]]}, {-BarnPoleLength * DiagramRange / GridNumber, -DiagramRange}}]
                        ]
                    ],
                    BarnColor,
                    Polygon[{{0, -DiagramRange}, {0, DiagramRange}, {-BarnLength * DiagramRange / GridNumber, DiagramRange}, {-BarnLength * DiagramRange / GridNumber, -DiagramRange}}],
                    Thickness[0.01],
                    Dotted,
                    Opacity[1],
                    Black,
                    Line[{{0, -DiagramRange}, {0, 0}}],
                    Line[{{-BarnLength * DiagramRange / GridNumber, DiagramRange}, {-BarnLength * DiagramRange / GridNumber, 0}}]
                },
                {
                    Dashed,
                    Line[{{#, -DiagramRange}, {#, DiagramRange}}] & [-BarnLength * DiagramRange / GridNumber],
                    Line[LorentzTransform /@ {{#, -DiagramRange}, {#, DiagramRange}}] & [-BarnPoleLength * DiagramRange / GridNumber]
                },
                If[BarnPoleLength / LorentzFactor > BarnLength && ((Abs[BarnPoleTime] <= 0.01 && WhichObserver == 0) || (Abs[LorentzTransformInverse[{-BarnLength, 0}][[2]] - BarnPoleTime] <= 0.01 && WhichObserver == 1)), {
                    Red,
                    Thickness[0.003],
                    Table[Line[({-BarnLength * DiagramRange / GridNumber, 0} + # * {Sin[a], Cos[a]} &) /@ {0.03, 0.07}], {a, 0, 2 * Pi, Pi / 5}]
                }]
            }],
            (* Draw the ticks for frame S'. *)
            If[GridSPrimeTicksShow, {
                If[WhichDemo != "Twin \"Paradox\"", {
                    Black,
                    Thickness[0.003],
                    Table[
                        Line[LorentzTransform /@ {{x, -s}, {x, s}} /. s -> DiagramRange / GridNumber * 0.1],
                        {x, -DiagramRange, DiagramRange, DiagramRange / GridNumber}
                    ],
                    Table[
                        Line[LorentzTransform /@ {{-s, t}, {s, t}} /. s -> DiagramRange / GridNumber * 0.1],
                        {t, -DiagramRange, DiagramRange, DiagramRange / GridNumber}
                    ]
                }, {
                    Black,
                    Thickness[0.003],
                    Table[
                        Line[LorentzTransform /@ {{-s, t}, {s, t}} /. s -> DiagramRange / GridNumber * 0.1],
                        {t, -DiagramRange, 0, DiagramRange / GridNumber}
                    ],
                    Table[
                        Line[LorentzTransform[#, -1] & /@ {{-s, t}, {s, t}} /. s -> DiagramRange / GridNumber * 0.1],
                        {t, 0, DiagramRange, DiagramRange / GridNumber}
                    ],
                    If[GridSPrimeShow, {
                        Table[
                            Line[LorentzTransform /@ {{x, -s}, {x, s}} /. s -> DiagramRange / GridNumber * 0.1],
                            {x, -DiagramRange, DiagramRange, DiagramRange / GridNumber}
                        ],
                        Table[
                            Line[LorentzTransform[#, -1] &  /@ {{x, -s}, {x, s}} /. s -> DiagramRange / GridNumber * 0.1],
                            {x, -DiagramRange, DiagramRange, DiagramRange / GridNumber}
                        ]
                    }]
                }]
            }],
            (* Draw the coordinate grid for frame S'. *)
            If[GridSPrimeShow, {
                Opacity[GridOpacity],
                If[WhichDemo != "Twin \"Paradox\"",
                    {
                        GridSPrimeColorT,
                        Thickness[0.0015],
                        Table[
                            Line[LorentzTransform /@ {{x, -DiagramRange}, {x, DiagramRange}}],
                            {x, -DiagramRange, DiagramRange, DiagramRange / GridNumber}
                        ],
                        GridSPrimeColorX,
                        Table[
                            Line[LorentzTransform /@ {{-DiagramRange, t}, {DiagramRange, t}}],
                            {t, -DiagramRange, DiagramRange, DiagramRange / GridNumber}
                        ]
                    }, {
                        GridSPrimeColorT,
                        Thickness[0.0015],
                        Table[
                            Line[LorentzTransform /@ {{x, -DiagramRange}, {x, 0}}],
                            {x, -DiagramRange, DiagramRange, DiagramRange / GridNumber}
                        ],
                        Table[
                            Line[LorentzTransform[#, -1] &  /@ {{x, 0}, {x, DiagramRange}}],
                            {x, -DiagramRange, DiagramRange, DiagramRange / GridNumber}
                        ],
                        GridSPrimeColorX,
                        Table[
                            Line[LorentzTransform /@ {{-DiagramRange, t}, {DiagramRange, t}}],
                            {t, -DiagramRange, 0, DiagramRange / GridNumber}
                        ],
                        Table[
                            Line[LorentzTransform[#, -1] & /@ {{-DiagramRange, t}, {DiagramRange, t}}],
                            {t, 0, DiagramRange, DiagramRange / GridNumber}
                        ]
                    }
                ]
            }],
            (* Draw locators so that the user can interact directly with the diagram. Note that this has to be the last item, otherwise other graphics constructs may be painted on top of the locators, and the user will not be able to interact with them. *)
            {
                (* Locator to specify the velocity of S'. *)
                Locator[
                    Dynamic[
                        LorentzTransform[{0, DiagramRange / LorentzFactor}] - {SPrimeX, SPrimeT},
                        (
                            SPrimeV = Round[Max[{Min[{#[[1]] / #[[2]], 0.99}], -0.99}], 0.005];
                            LorentzFactor = 1 / Sqrt[1 - SPrimeV^2];
                            UpdateSecondPath[];
                            If[WhichDemo == "Twin \"Paradox\"", {SPrimeX, SPrimeT} = {TwinDistance * DiagramRange / GridNumber * Sign[SPrimeV], 0}];
                            If[WhichDemo == "Barn \"Paradox\"" && BarnPoleLength / LorentzFactor > BarnLength,
                                If[WhichObserver == 0,
                                        BarnPoleTime = Min[0, BarnPoleTime],
                                        BarnPoleTime = Min[LorentzTransformInverse[{-BarnLength, 0}][[2]], BarnPoleTime]

                                ];
                            ];
                        ) &
                    ]
                ],
                (* Locator to specify the position of S'. *)
                If[WhichDemo != "Twin \"Paradox\"" && WhichDemo != "Barn \"Paradox\"", Locator[
                    Dynamic[
                        {SPrimeX, SPrimeT},
                        (
                            {SPrimeX, SPrimeT} = {Round[#[[1]] * GridNumber / DiagramRange, 0.05], Round[#[[2]] * GridNumber / DiagramRange, 0.05]} / GridNumber * DiagramRange;
                            If[WhichDemo == "Time Travel", SPrimeT = Max[SPrimeT, 0]];
                            Path1V = Quiet[SPrimeX / SPrimeT];
                            UpdateSecondPath[];
                        ) &
                    ]
                ]],
                (* Locator to change the spatial separation in the simultaneity illustration. *)
                If[WhichDemo == "Simultaneity", Locator[
                    Dynamic[
                        If[WhichObserver == 0,
                                {SimultaneitySep * DiagramRange / GridNumber, 0},
                                LorentzTransform[{SimultaneitySep * DiagramRange / GridNumber, 0}]
                        ],
                        (
                            If[WhichObserver == 0,
                                SimultaneitySep = Round[#[[1]] * GridNumber / DiagramRange, 0.05],
                                SimultaneitySep = Round[LorentzTransformInverse[{#[[1]], #[[2]]}][[1]] * GridNumber / DiagramRange, 0.05]
                            ]
                        ) &
                    ]
                ]],
                (* Locator to change the duration of time in S' for the time dilation illustration. *)
                If[WhichDemo == "Time Dilation", Locator[
                    If[WhichObserver == 0,
                        Dynamic[
                            LorentzTransform[{0, TimeDilationDuration * DiagramRange / GridNumber}],
                            (
                                TimeDilationDuration = Round[LorentzTransformInverse[{#[[1]], #[[2]]}][[2]] * GridNumber / DiagramRange, 0.05];
                            ) &
                        ],
                        Dynamic[
                            {0, SPrimeT - SPrimeX * SPrimeV + TimeDilationDuration * DiagramRange / GridNumber},
                            (
                                TimeDilationDuration = Round[#[[2]] * GridNumber / DiagramRange, 0.05];
                            ) &
                        ]
                    ]
                ]],
                (* Locator to change the distance in S' for the length contraction illustration. *)
                If[WhichDemo == "Length Contraction", Locator[
                    If[WhichObserver == 0,
                        Dynamic[
                            LorentzTransform[{LengthContDistance * DiagramRange / GridNumber, 0}],
                            (
                                LengthContDistance = Round[LorentzTransformInverse[{#[[1]], #[[2]]}][[1]] * GridNumber / DiagramRange, 0.05];
                            ) &
                        ],
                        Dynamic[
                            {LorentzTransform[{LengthContDistance * DiagramRange / GridNumber / LorentzFactor, 0}][[1]], 0},
                            (
                                LengthContDistance = Round[#[[1]] * GridNumber / DiagramRange, 0.05];
                            ) &
                        ]
                    ]
                ]],
                (* Locator to change the trip distance (and thus the trip duration, since the velocity is constant) in the twin paradox illustration. *)
                If[WhichDemo == "Twin \"Paradox\"", Locator[
                    Dynamic[
                        {TwinDistance * DiagramRange / GridNumber * Sign[SPrimeV], 0},
                        (
                            SPrimeX = Round[#[[1]] * GridNumber / DiagramRange, 0.05] / GridNumber * DiagramRange;
                            TwinDistance = Abs[SPrimeX * GridNumber / DiagramRange];
                            If[Sign[SPrimeV] != Sign[#[[1]]], SPrimeV = -SPrimeV];
                        ) &
                    ]
                ]],
                (* Locator to move the pole in the barn paradox illustration. *)
                If[WhichDemo == "Barn \"Paradox\"", Locator[
                    Dynamic[
                        LorentzTransform[{0, BarnPoleTime * DiagramRange / GridNumber}],
                        (
                            BarnPoleTime = Round[LorentzTransformInverse[{#[[1]], #[[2]]}][[2]] * GridNumber / DiagramRange, 0.05];
                            If[BarnPoleLength / LorentzFactor > BarnLength,
                                If[WhichObserver == 0,
                                    BarnPoleTime = Min[0, BarnPoleTime],
                                    BarnPoleTime = Min[LorentzTransformInverse[{-BarnLength, 0}][[2]], BarnPoleTime]
                                  ]
                            ];
                        ) &
                    ]
                ]],
                (* Locator to specify the arrival time of tachyon 2. *)
                If[WhichDemo == "Time Travel", Locator[
                    Dynamic[
                        {0, ArrivalTime},
                        (
                            ArrivalTime = Round[#[[2]], 0.05];
                            UpdateSecondPath[];
                        ) &
                    ]
                ]]
            }
        },
        ImageSize -> {DiagramSize, DiagramSize},
        PlotRange -> {{-DiagramRange, DiagramRange}, {-DiagramRange, DiagramRange}} * LabelMultiplier^2
        ]]
    }, Alignment -> Center]]
]
