module VmEmitter exposing (..)


import VmParser exposing (Instruction(..), Segment(..), Operation(..))
import String.Case


emit :  String -> List Instruction -> String
emit programName instructions =
  String.join "\n" <|
  List.indexedMap
    (emitInstruction <| "$" ++ String.Case.toSnakeCaseLower programName)
    instructions


emitInstruction : String -> Int -> Instruction -> String
emitInstruction programName instructionIndex instruction =
  String.join "\n" <|
  case instruction of
    InsPush segment index ->
      let
        i =
          String.fromInt index
      in
      case segment of
        SegStatic ->
          [ "@" ++ programName ++ "_" ++ i
          ]
          ++ emitPushA

        SegPointer ->
          [ "@" ++ emitPointerIndex index
          ]
          ++ emitPushA
        
        SegTemp ->
          [ "@" ++ (String.fromInt <| 5 + index)
          ]
          ++ emitPushA
        
        SegConstant ->
          [ "@" ++ i
          , "D=A"
          , "@SP"
          , "AM=M+1"
          , "A=A-1"
          , "M=D"
          ]

        _ ->
          [ "@" ++ i
          , "D=A"
          , "@" ++ emitSegment segment
          , "A=D+M"
          ]
          ++ emitPushA
        

    InsPop segment index ->
      let
        i =
          String.fromInt index
      in
      case segment of
        SegStatic ->
          emitPopA <| "@" ++ programName ++ "_" ++ i
        
        SegPointer ->
          emitPopA <| "@" ++ emitPointerIndex index
        
        SegTemp ->
          emitPopA <| "@" ++ (String.fromInt <| 5 + index)
        
        _ ->
          [ "@" ++ i
          , "D=A"
          , "@" ++ emitSegment segment
          , "D=D+M"
          ]
          ++ emitPopD

    InsArith op ->
      case op of
        OpAdd ->
          emitBinaryArith "D+M"

        OpSub ->
          emitBinaryArith "M-D"
        
        OpNeg ->
          emitUnaryArith "-"
        
        OpEq ->
          emitComparison instructionIndex "EQ" "EQ"
        
        OpGt ->
          emitComparison instructionIndex "GT" "LT"
        
        OpLt ->
          emitComparison instructionIndex "LT" "GT"
        
        OpAnd ->
          emitBinaryArith "D&M"
        
        OpOr ->
          emitBinaryArith "D|M"
        
        OpNot ->
          emitUnaryArith "!"


emitUnaryArith : String -> List String
emitUnaryArith op =
  [ "@SP"
  , "A=M-1"
  , "M=" ++ op ++ "M"
  ]


emitComparison : Int -> String -> String -> List String
emitComparison instructionIndex opName op =
  let
    i =
      String.fromInt instructionIndex
  in
  [ "@SP"
  , "AM=M-1"
  , "D=M"
  , "A=A-1"
  , "D=D-M"
  , "@" ++ opName ++ "_TRUE_" ++ i
  , "D;J" ++ op
  , "D=0"
  , "@" ++ opName ++ "_END_" ++ i
  , "0;JMP"
  , "(" ++ opName ++ "_TRUE_" ++ i ++ ")"
  , "D=-1"
  , "(" ++ opName ++ "_END_" ++ i ++ ")"
  , "@SP"
  , "A=M-1"
  , "M=D"
  ]


emitBinaryArith : String -> List String
emitBinaryArith op =
  [ "@SP"
  , "AM=M-1"
  , "D=M"
  , "A=A-1"
  , "M=" ++ op
  ]


emitSegment : Segment -> String
emitSegment segment =
  case segment of
    SegLocal ->
      "LCL"

    SegArgument ->
      "ARG"

    SegThis ->
      "THIS"

    SegThat ->
      "THAT"
    
    _ ->
      "SEGMENT WITHOUT A NAME"


emitPointerIndex : Int -> String
emitPointerIndex index =
  if index == 0 then "THIS" else "THAT"


emitPushA : List String
emitPushA =
  [ "D=M"
  , "@SP"
  , "AM=M+1"
  , "A=A-1"
  , "M=D"
  ]


emitPopA : String -> List String
emitPopA addr =
  [ "@SP"
  , "AM=M-1"
  , "D=M"
  , addr
  , "M=D"
  ]


emitPopD : List String
emitPopD =
  [ "@R13"
  , "M=D"
  , "@SP"
  , "AM=M-1"
  , "D=M"
  , "@R13"
  , "A=M"
  , "M=D"
  ]