module VmTranslator exposing (main)


import Html exposing (div, pre, text)
import VmParser
import VmEmitter


source =
  """
push argument 1
pop pointer 1           -- that = argument[1]

push constant 0
pop that 0              -- first element in the series = 0
push constant 1
pop that 1              -- second element in the series = 1

push argument 0
push constant 2
sub
pop argument 0          -- num_of_elements -= 2 (first 2 elements are set)

label MAIN_LOOP_START

push argument 0
if-goto COMPUTE_ELEMENT -- if num_of_elements > 0, goto COMPUTE_ELEMENT
goto END_PROGRAM        -- otherwise, goto END_PROGRAM

label COMPUTE_ELEMENT

push that 0
push that 1
add
pop that 2              -- that[2] = that[0] + that[1]

push pointer 1
push constant 1
add
pop pointer 1           -- that += 1

push argument 0
push constant 1
sub
pop argument 0          -- num_of_elements--

goto MAIN_LOOP_START

label END_PROGRAM

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
        , pre [] [ text <| VmEmitter.emit "BasicTest" program ]
        ]
