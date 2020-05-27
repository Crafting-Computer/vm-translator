module VmTranslator exposing (main)


import Html exposing (div, pre, text)
import VmParser


source =
  """-- ewio
push pointer 1 -- ewio
add -- eiwo
"""


main =
  case VmParser.parse source of
    Err err ->
      div []
        [ pre [] [ text source]
        , pre [] [ text <| 
          "❌ Parse error.\n\n"
          ++ VmParser.showDeadEnds Nothing source err
        ]
        ]

    Ok program ->
      div []
        [ pre [] [ text source]
        , pre [] [ text "✔️ Passed parser." ]
        ]
