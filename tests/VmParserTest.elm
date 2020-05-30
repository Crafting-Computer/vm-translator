module VmParserTest exposing (..)


import VmParser exposing (Instruction(..), Segment(..), Operation(..), Context(..), Problem(..))
import Parser.Advanced exposing (DeadEnd)
import Expect
import Test exposing (Test, describe)


suite : Test
suite =
  describe "VM Parser"
    [ describe "Basic stack operations"
      [ describe "push instructions"
        [ test "local" "push local 948" <| Ok [ InsPush SegLocal 948 ]
        , test "argument" "push argument 948" <| Ok [ InsPush SegArgument 948 ]
        , test "this" "push this 948" <| Ok [ InsPush SegThis 948 ]
        , test "that" "push that 948" <| Ok [ InsPush SegThat 948 ]
        , test "constant" "push constant 948" <| Ok [ InsPush SegConstant 948 ]
        , test "static" "push static 948" <| Ok [ InsPush SegStatic 948 ]
        , test "temp" "push temp 5" <| Ok [ InsPush SegTemp 5 ]
        ]
      , describe "pop instructions"
        [ test "local" "pop local 948" <| Ok [ InsPop SegLocal 948 ]
        , test "argument" "pop argument 948" <| Ok [ InsPop SegArgument 948 ]
        , test "this" "pop this 948" <| Ok [ InsPop SegThis 948 ]
        , test "that" "pop that 948" <| Ok [ InsPop SegThat 948 ]
        , test "constant" "pop constant 948" <| Err [{ col = 17, contextStack = [{ col = 4, context = CtxPop, row = 1 }], problem = InvalidPopConstant, row = 1 }]
        , test "static" "pop static 948" <| Ok [ InsPop SegStatic 948 ]
        , test "temp" "pop temp 5" <| Ok [ InsPop SegTemp 5 ]
        ]
      , describe "arithmetic instructions"
        [ test "add" "add" <| Ok [ InsArith OpAdd ]
        , test "sub" "sub" <| Ok [ InsArith OpSub ]
        , test "neg" "neg" <| Ok [ InsArith OpNeg ]
        , test "eq" "eq" <| Ok [ InsArith OpEq ]
        , test "gt" "gt" <| Ok [ InsArith OpGt ]
        , test "lt" "lt" <| Ok [ InsArith OpLt ]
        , test "and" "and" <| Ok [ InsArith OpAnd ]
        , test "or" "or" <| Ok [ InsArith OpOr ]
        , test "not" "not" <| Ok [ InsArith OpNot ]
        ]
      , describe "comments"
        [ test "line comment"
        """-- a single-line comment
  push local 1"""
        <| Ok [ InsPush SegLocal 1 ]
        , test "multi-line comment"
        """{- a multi-line
  comment
  -}
  push local 1"""
        <| Ok [ InsPush SegLocal 1 ]
        ]
      , describe "invalid segment index"
        [ test "index is not in decimal"
          "push local 0b0" <|
          Err [{ col = 12, contextStack = [{ col = 5, context = CtxPush, row = 1 }], problem = InvalidNumber, row = 1 }]
        , test "index is not integer"
          "push local 389.2" <|
          Err [{ col = 12, contextStack = [{ col = 5, context = CtxPush, row = 1 }], problem = InvalidNumber, row = 1 }]
        , test "index is negative"
          "push local -9" <|
          Err [{ col = 12, contextStack = [{ col = 5, context = CtxPush, row = 1 }], problem = ExpectingInt, row = 1 }]
        ]
      , describe "invalid pointer index"
        [ test "push"
          "push pointer 2" <|
          Err [{ col = 15, contextStack = [{ col = 5, context = CtxPush, row = 1 }], problem = InvalidPointerIndex 2, row = 1 }]
        ,  test "pop"
          "pop pointer 2" <|
          Err [{ col = 14, contextStack = [{ col = 4, context = CtxPop, row = 1 }], problem = InvalidPointerIndex 2, row = 1 }]
        ]
      , describe "invalid temp index"
        [ test "push"
          "push temp 8" <|
          Err [{ col = 12, contextStack = [{ col = 5, context = CtxPush, row = 1 }], problem = InvalidTempIndex 8, row = 1 }]
        ,  test "pop"
          "pop temp 8" <|
          Err [{ col = 11, contextStack = [{ col = 4, context = CtxPop, row = 1 }], problem = InvalidTempIndex 8, row = 1 }]
        ]
      ]
    , describe "Branching"
      [ test "label"
        "label LOOP_START" <|
          Ok [InsLabel { from = (1,7), to = (1,17), value = "LOOP_START" }]
      , test "goto"
        "goto LOOP_END\nlabel LOOP_END" <|
          Ok [InsGoto { from = (1,6), to = (1,14), value = "LOOP_END" },InsLabel { from = (2,7), to = (2,15), value = "LOOP_END" }]
      , test "if-goto"
        "if-goto LOOP_END\nlabel LOOP_END" <|
          Ok [InsIfGoto { from = (1,9), to = (1,17), value = "LOOP_END" },InsLabel { from = (2,7), to = (2,15), value = "LOOP_END" }]
      , test "duplicated label"
        "label MY_LABEL\nlabel MY_LABEL" <|
          Err [{ col = 15, contextStack = [], problem = DuplicatedLabel { from = (1,7), to = (1,15), value = "MY_LABEL" } { from = (2,7), to = (2,15), value = "MY_LABEL" }, row = 2 }]
      , test "goto undefined label"
        "goto MY_LABEL" <|
        Err [{ col = 6, contextStack = [], problem = UndefinedLabel { from = (1,6), to = (1,14), value = "MY_LABEL" }, row = 1 }]
      , test "if-goto undefined label"
        "if-goto MY_LABEL" <|
        Err [{ col = 9, contextStack = [], problem = UndefinedLabel { from = (1,9), to = (1,17), value = "MY_LABEL" }, row = 1 }]
      ]
    ]


test : String -> String -> Result (List (DeadEnd Context Problem)) (List Instruction) -> Test
test description src expected =
  Test.test
  description
  (\_ -> Expect.equal expected (VmParser.parse src))