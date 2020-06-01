module VmTranslator exposing (main)


import Html exposing (div, pre, text)
import VmParser
import VmEmitter
import Dict


nestedCalls =
  Dict.fromList
  [ ( "Sys"
  , """
function Sys.init 0
push constant 4000
pop pointer 0
push constant 5000
pop pointer 1
call Sys.main 0
pop temp 1
label LOOP
goto LOOP

function Sys.main 5
push constant 4001
pop pointer 0
push constant 5001
pop pointer 1
push constant 200
pop local 1
push constant 40
pop local 2
push constant 6
pop local 3
push constant 123
call Sys.add12 1
pop temp 0
push local 0
push local 1
push local 2
push local 3
push local 4
add
add
add
add
return

function Sys.add12 0
push constant 4002
pop pointer 0
push constant 5002
pop pointer 1
push argument 0
push constant 12
add
return
""" )
  ]


simpleFunction =
  Dict.fromList [
    ( "SimpleFunction"
    , """function SimpleFunction.test 2
push local 0
push local 1
add
not
push argument 0
add
push argument 1
sub
return
      """
    )
  ]


fibonacciElements =
  Dict.fromList [ 
    ( "Sys"
    , """
function Sys.init 0
push constant 4
call Main.fibonacci 1   -- computes the 4'th fibonacci element
label WHILE
goto WHILE              -- loops infinitely
  """
    )
  , ( "Main"
  , """
-- Main.vm
function Main.fibonacci 0
push argument 0
push constant 2
lt                     -- checks if n<2
if-goto IF_TRUE
goto IF_FALSE
label IF_TRUE          -- if n<2, return n
push argument 0        
return
label IF_FALSE         -- if n>=2, returns fib(n-2)+fib(n-1)
push argument 0
push constant 2
sub
call Main.fibonacci 1  -- computes fib(n-2)
push argument 0
push constant 1
sub
call Main.fibonacci 1  -- computes fib(n-1)
add                    -- returns fib(n-1) + fib(n-2)
return
  """
  )
  ]


singleCall =
  Dict.fromList
    [ ("Call"
    , "call MyFunc 2"
    )]


staticTest =
  Dict.fromList
    [ ( "Sys"
    , """
function Sys.init 0
push constant 6
push constant 8
call Class1.set 2
pop temp 0 -- Dumps the return value
push constant 23
push constant 15
call Class2.set 2
pop temp 0 -- Dumps the return value
call Class1.get 0
call Class2.get 0
label WHILE
goto WHILE
"""
    )
    , ( "Class1"
    , """function Class1.set 0
push argument 0
pop static 0
push argument 1
pop static 1
push constant 0
return

-- Returns static[0] - static[1].
function Class1.get 0
push static 0
push static 1
sub
return
    """
    )
    , ( "Class2"
    , """function Class2.set 0
push argument 0
pop static 0
push argument 1
pop static 1
push constant 0
return

-- Returns static[0] - static[1].
function Class2.get 0
push static 0
push static 1
sub
return
    """
    )
    ]


source =
  staticTest


main =
  case VmParser.parseProject source of
    Err err ->
      div []
        [ pre [] [ text <| 
          "❌ Parse error.\n\n"
          ++ VmParser.showProjectDeadEnds source err
        ]
        ]

    Ok project ->
      let
        (emittedStr, sourceMap) =
          VmEmitter.emitProject project
      in
      div []
        [ pre [] [ text "✔️ Passed parser." ]
        , pre [] [ text "🏭 Emitted assembly:" ]
        , pre [] [ text emittedStr ]
        , pre [] [ text "Source map:" ]
        , pre [] [ text <| Debug.toString sourceMap ]
        ]