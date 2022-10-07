type t =
  | RGB of float * float * float
  | Hex of string
  | Lavender
  | Thistle
  | Plum
  | Violet
  | Orchid
  | Fuchsia
  | Magenta
  | MediumOrchid
  | MediumPurple
  | BlueViolet
  | DarkViolet
  | DarkOrchid
  | DarkMagenta
  | Purple
  | Indigo
  | DarkSlateBlue
  | SlateBlue
  | MediumSlateBlue
  | Pink
  | LightPink
  | HotPink
  | DeepPink
  | MediumVioletRed
  | PaleVioletRed
  | Aqua
  | Cyan
  | LightCyan
  | PaleTurquoise
  | Aquamarine
  | Turquoise
  | MediumTurquoise
  | DarkTurquoise
  | CadetBlue
  | SteelBlue
  | LightSteelBlue
  | PowderBlue
  | LightBlue
  | SkyBlue
  | LightSkyBlue
  | DeepSkyBlue
  | DodgerBlue
  | CornflowerBlue
  | RoyalBlue
  | Blue
  | MediumBlue
  | DarkBlue
  | Navy
  | MidnightBlue
  | IndianRed
  | LightCoral
  | Salmon
  | DarkSalmon
  | LightSalmon
  | Red
  | Crimson
  | FireBrick
  | DarkRed
  | GreenYellow
  | Chartreuse
  | LawnGreen
  | Lime
  | LimeGreen
  | PaleGreen
  | LightGreen
  | MediumSpringGreen
  | SpringGreen
  | MediumSeaGreen
  | SeaGreen
  | ForestGreen
  | Green
  | DarkGreen
  | YellowGreen
  | OliveDrab
  | Olive
  | DarkOliveGreen
  | MediumAquamarine
  | DarkSeaGreen
  | LightSeaGreen
  | DarkCyan
  | Teal
  | Coral
  | Tomato
  | OrangeRed
  | DarkOrange
  | Orange
  | Gold
  | Yellow
  | LightYellow
  | LemonChiffon
  | LightGoldenrodYellow
  | PapayaWhip
  | Moccasin
  | PeachPuff
  | PaleGoldenrod
  | Khaki
  | DarkKhaki
  | Cornsilk
  | BlanchedAlmond
  | Bisque
  | NavajoWhite
  | Wheat
  | BurlyWood
  | Tan
  | RosyBrown
  | SandyBrown
  | Goldenrod
  | DarkGoldenrod
  | Peru
  | Chocolate
  | SaddleBrown
  | Sienna
  | Brown
  | Maroon
  | White
  | Snow
  | Honeydew
  | MintCream
  | Azure
  | AliceBlue
  | GhostWhite
  | WhiteSmoke
  | Seashell
  | Beige
  | OldLace
  | FloralWhite
  | Ivory
  | AntiqueWhite
  | Linen
  | LavenderBlush
  | MistyRose
  | Gainsboro
  | LightGrey
  | Silver
  | DarkGray
  | Gray
  | DimGray
  | LightSlateGray
  | SlateGray
  | DarkSlateGray
  | Black

let to_string = function
  | RGB (r, g, b)        -> Printf.sprintf "c = [%f, %f, %f]" r g b
  | Hex s                -> Printf.sprintf "\"%s\"" s
  | Lavender             -> "\"Lavender\""
  | Thistle              -> "\"Thistle\""
  | Plum                 -> "\"Plum\""
  | Violet               -> "\"Violet\""
  | Orchid               -> "\"Orchid\""
  | Fuchsia              -> "\"Fuchsia\""
  | Magenta              -> "\"Magenta\""
  | MediumOrchid         -> "\"MediumOrchid\""
  | MediumPurple         -> "\"MediumPurple\""
  | BlueViolet           -> "\"BlueViolet\""
  | DarkViolet           -> "\"DarkViolet\""
  | DarkOrchid           -> "\"DarkOrchid\""
  | DarkMagenta          -> "\"DarkMagenta\""
  | Purple               -> "\"Purple\""
  | Indigo               -> "\"Indigo\""
  | DarkSlateBlue        -> "\"DarkSlateBlue\""
  | SlateBlue            -> "\"SlateBlue\""
  | MediumSlateBlue      -> "\"MediumSlateBlue\""
  | Pink                 -> "\"Pink\""
  | LightPink            -> "\"LightPink\""
  | HotPink              -> "\"HotPink\""
  | DeepPink             -> "\"DeepPink\""
  | MediumVioletRed      -> "\"MediumVioletRed\""
  | PaleVioletRed        -> "\"PaleVioletRed\""
  | Aqua                 -> "\"Aqua\""
  | Cyan                 -> "\"Cyan\""
  | LightCyan            -> "\"LightCyan\""
  | PaleTurquoise        -> "\"PaleTurquoise\""
  | Aquamarine           -> "\"Aquamarine\""
  | Turquoise            -> "\"Turquoise\""
  | MediumTurquoise      -> "\"MediumTurquoise\""
  | DarkTurquoise        -> "\"DarkTurquoise\""
  | CadetBlue            -> "\"CadetBlue\""
  | SteelBlue            -> "\"SteelBlue\""
  | LightSteelBlue       -> "\"LightSteelBlue\""
  | PowderBlue           -> "\"PowderBlue\""
  | LightBlue            -> "\"LightBlue\""
  | SkyBlue              -> "\"SkyBlue\""
  | LightSkyBlue         -> "\"LightSkyBlue\""
  | DeepSkyBlue          -> "\"DeepSkyBlue\""
  | DodgerBlue           -> "\"DodgerBlue\""
  | CornflowerBlue       -> "\"CornflowerBlue\""
  | RoyalBlue            -> "\"RoyalBlue\""
  | Blue                 -> "\"Blue\""
  | MediumBlue           -> "\"MediumBlue\""
  | DarkBlue             -> "\"DarkBlue\""
  | Navy                 -> "\"Navy\""
  | MidnightBlue         -> "\"MidnightBlue\""
  | IndianRed            -> "\"IndianRed\""
  | LightCoral           -> "\"LightCoral\""
  | Salmon               -> "\"Salmon\""
  | DarkSalmon           -> "\"DarkSalmon\""
  | LightSalmon          -> "\"LightSalmon\""
  | Red                  -> "\"Red\""
  | Crimson              -> "\"Crimson\""
  | FireBrick            -> "\"FireBrick\""
  | DarkRed              -> "\"DarkRed\""
  | GreenYellow          -> "\"GreenYellow\""
  | Chartreuse           -> "\"Chartreuse\""
  | LawnGreen            -> "\"LawnGreen\""
  | Lime                 -> "\"Lime\""
  | LimeGreen            -> "\"LimeGreen\""
  | PaleGreen            -> "\"PaleGreen\""
  | LightGreen           -> "\"LightGreen\""
  | MediumSpringGreen    -> "\"MediumSpringGreen\""
  | SpringGreen          -> "\"SpringGreen\""
  | MediumSeaGreen       -> "\"MediumSeaGreen\""
  | SeaGreen             -> "\"SeaGreen\""
  | ForestGreen          -> "\"ForestGreen\""
  | Green                -> "\"Green\""
  | DarkGreen            -> "\"DarkGreen\""
  | YellowGreen          -> "\"YellowGreen\""
  | OliveDrab            -> "\"OliveDrab\""
  | Olive                -> "\"Olive\""
  | DarkOliveGreen       -> "\"DarkOliveGreen\""
  | MediumAquamarine     -> "\"MediumAquamarine\""
  | DarkSeaGreen         -> "\"DarkSeaGreen\""
  | LightSeaGreen        -> "\"LightSeaGreen\""
  | DarkCyan             -> "\"DarkCyan\""
  | Teal                 -> "\"Teal\""
  | Coral                -> "\"Coral\""
  | Tomato               -> "\"Tomato\""
  | OrangeRed            -> "\"OrangeRed\""
  | DarkOrange           -> "\"DarkOrange\""
  | Orange               -> "\"Orange\""
  | Gold                 -> "\"Gold\""
  | Yellow               -> "\"Yellow\""
  | LightYellow          -> "\"LightYellow\""
  | LemonChiffon         -> "\"LemonChiffon\""
  | LightGoldenrodYellow -> "\"LightGoldenrodYellow\""
  | PapayaWhip           -> "\"PapayaWhip\""
  | Moccasin             -> "\"Moccasin\""
  | PeachPuff            -> "\"PeachPuff\""
  | PaleGoldenrod        -> "\"PaleGoldenrod\""
  | Khaki                -> "\"Khaki\""
  | DarkKhaki            -> "\"DarkKhaki\""
  | Cornsilk             -> "\"Cornsilk\""
  | BlanchedAlmond       -> "\"BlanchedAlmond\""
  | Bisque               -> "\"Bisque\""
  | NavajoWhite          -> "\"NavajoWhite\""
  | Wheat                -> "\"Wheat\""
  | BurlyWood            -> "\"BurlyWood\""
  | Tan                  -> "\"Tan\""
  | RosyBrown            -> "\"RosyBrown\""
  | SandyBrown           -> "\"SandyBrown\""
  | Goldenrod            -> "\"Goldenrod\""
  | DarkGoldenrod        -> "\"DarkGoldenrod\""
  | Peru                 -> "\"Peru\""
  | Chocolate            -> "\"Chocolate\""
  | SaddleBrown          -> "\"SaddleBrown\""
  | Sienna               -> "\"Sienna\""
  | Brown                -> "\"Brown\""
  | Maroon               -> "\"Maroon\""
  | White                -> "\"White\""
  | Snow                 -> "\"Snow\""
  | Honeydew             -> "\"Honeydew\""
  | MintCream            -> "\"MintCream\""
  | Azure                -> "\"Azure\""
  | AliceBlue            -> "\"AliceBlue\""
  | GhostWhite           -> "\"GhostWhite\""
  | WhiteSmoke           -> "\"WhiteSmoke\""
  | Seashell             -> "\"Seashell\""
  | Beige                -> "\"Beige\""
  | OldLace              -> "\"OldLace\""
  | FloralWhite          -> "\"FloralWhite\""
  | Ivory                -> "\"Ivory\""
  | AntiqueWhite         -> "\"AntiqueWhite\""
  | Linen                -> "\"Linen\""
  | LavenderBlush        -> "\"LavenderBlush\""
  | MistyRose            -> "\"MistyRose\""
  | Gainsboro            -> "\"Gainsboro\""
  | LightGrey            -> "\"LightGrey\""
  | Silver               -> "\"Silver\""
  | DarkGray             -> "\"DarkGray\""
  | Gray                 -> "\"Gray\""
  | DimGray              -> "\"DimGray\""
  | LightSlateGray       -> "\"LightSlateGray\""
  | SlateGray            -> "\"SlateGray\""
  | DarkSlateGray        -> "\"DarkSlateGray\""
  | Black                -> "\"Black\""
