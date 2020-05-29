module VmTranslator exposing (main)


import Html exposing (div, pre, text)
import VmParser
import VmEmitter


source =
  """
push constant 17
push constant 17
eq
push constant 17
push constant 16
eq
push constant 16
push constant 17
eq
push constant 892
push constant 891
lt
push constant 891
push constant 892
lt
push constant 891
push constant 891
lt
push constant 32767
push constant 32766
gt
push constant 32766
push constant 32767
gt
push constant 32766
push constant 32766
gt
push constant 57
push constant 31
push constant 53
add
push constant 112
sub
neg
and
push constant 82
or
not
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
        , pre [] [ text "🏭 Emitted assembly:" ]
        , pre [] [ text <| VmEmitter.emit "basicTest" program ]
        ]
