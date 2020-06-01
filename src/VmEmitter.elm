module VmEmitter exposing (emitProject, emitProgram)


import VmParser exposing (Instruction(..), Segment(..), Operation(..))
import Dict exposing (Dict)
import List.Extra


type alias SourceMap =
  Dict String (List SourceMapItem)


type alias SourceMapItem =
  { src : Instruction
  , start : Int
  , end : Int
  }


emitProject : Dict String (List Instruction) -> (String, SourceMap)
emitProject programs =
  let
    bootstrapLines =
      [ "@256"
      , "D=A"
      , "@SP"
      , "M=D"
      ]
      ++ emitCall 0 "Sys.init" 0

    startStrIndex =
      List.length bootstrapLines + 1
    
    (emittedStrs, sourceMaps, _) =
      Dict.foldl
        (\name instructions (emittedStrs1, sourceMaps1, startStrIndex1) ->
          let
            (emittedProgram, mappings) =
              emitProgram startStrIndex1 name instructions
          in
          ( emittedProgram :: emittedStrs1
          , Dict.insert name mappings sourceMaps1
          , case List.Extra.last mappings of
              Just mapping ->
                mapping.end + 1
              
              Nothing ->
                -1 -- impossible
          )
        )
        ([], Dict.empty, startStrIndex)
        programs
  in
  ( String.join "\n" <|
    bootstrapLines ++ emittedStrs
  , sourceMaps
  )


emitProgram :  Int -> String -> List Instruction -> (String, List SourceMapItem)
emitProgram startStrIndex programName instructions =
  Tuple.mapFirst (String.join "\n") <|
  List.unzip <|
  List.reverse <|
  Tuple.first <|
  List.foldl
    (\instruction (soureMapItems, (instructionIndex, startStrIndex1)) ->
      let
        (emittedStr, sourceMapItem) =
          emitInstruction (decapitalize programName) instructionIndex startStrIndex1 instruction
      in
      ( (emittedStr, sourceMapItem) :: soureMapItems
      , (instructionIndex + 1
        , sourceMapItem.end + 1
        )
      )
    )
    ([], (0, startStrIndex))
    instructions


emitInstruction : String -> Int -> Int -> Instruction -> (String, SourceMapItem)
emitInstruction programName instructionIndex startStrIndex instruction =
  (\strList ->
    ( String.join "\n" strList
    , { src =
        instruction
      , start =
        startStrIndex
      , end =
        startStrIndex + List.length strList - 1
      }
    )
  ) <|
  case instruction of
    InsPush segment index ->
      let
        i =
          String.fromInt index
      in
      case segment of
        SegStatic ->
          ("@" ++ programName ++ "." ++ i)
          :: emitPushM

        SegPointer ->
          ("@" ++ emitPointerIndex index)
          :: emitPushM
        
        SegTemp ->
          ("@" ++ (String.fromInt <| 5 + index))
          :: emitPushM
        
        SegConstant ->
          emitPushConstant index

        _ ->
          [ "@" ++ i
          , "D=A"
          , "@" ++ emitSegment segment
          , "A=D+M"
          ]
          ++ emitPushM
        

    InsPop segment index ->
      let
        i =
          String.fromInt index
      in
      case segment of
        SegStatic ->
          emitPopA [ "@" ++ programName ++ "." ++ i ]
        
        SegPointer ->
          emitPopA [ "@" ++ emitPointerIndex index ]
        
        SegTemp ->
          emitPopA [ "@" ++ (String.fromInt <| 5 + index) ]
        
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
    
    InsLabel label ->
      emitLabel label.value
    
    InsGoto label ->
      emitGoto label.value
    
    InsIfGoto label ->
      emitIfGoto label.value

    InsFunction name nVars ->
      emitFunction name.value nVars

    InsCall name nArgs ->
      emitCall instructionIndex name.value nArgs
    
    InsReturn ->
      emitReturn


{- endFrame = LCL
*ARG = pop()
SP = ARG + 1
THAT = *(--endFrame)
THIS = *(--endFrame)
ARG = *(--endFrame)
LCL = *(--endFrame)
retAddr = *(--endFrame)
goto retAddr
-}
emitReturn : List String
emitReturn =
  [ "@LCL"
  , "D=M"
  , "@R14"
  , "M=D"

  , "@5"
  , "A=D-A"
  , "D=M"
  , "@R15"
  , "M=D"
  ]
  ++
  emitPopA
    [ "@ARG"
    , "A=M"
    ]
  ++
  [ "@ARG"
  , "D=M+1"
  , "@SP"
  , "M=D"
  
  , "@R14"
  , "AM=M-1"
  , "D=M"
  , "@THAT"
  , "M=D"
  
  , "@R14"
  , "AM=M-1"
  , "D=M"
  , "@THIS"
  , "M=D"
  
  , "@R14"
  , "AM=M-1"
  , "D=M"
  , "@ARG"
  , "M=D"

  , "@R14"
  , "AM=M-1"
  , "D=M"
  , "@LCL"
  , "M=D"

  , "@R15"
  , "A=M"
  , "0;JMP"
  ]


{- push retAddr
push LCL
push ARG
push THIS
push THAT
ARG = SP-5-nArgs
LCL = SP
goto functionName
(retAddr)
-}
emitCall : Int -> String -> Int -> List String
emitCall instructionIndex name nArgs =
  let
    retAddr =
      "CALL_" ++ name ++ "_RET_ADDR_" ++ String.fromInt instructionIndex
    
    pushA addr =
      ("@" ++ addr) :: emitPushA
    
    pushM addr =
      ("@" ++ addr) :: emitPushM
  in
  pushA retAddr
  ++ pushM "LCL"
  ++ pushM "ARG"
  ++ pushM "THIS"
  ++ pushM "THAT"
  ++ [ "@SP"
  , "D=M"
  , "@" ++ String.fromInt (5 + nArgs)
  , "D=D-A"
  , "@ARG"
  , "M=D"
  , "@SP"
  , "D=M"
  , "@LCL"
  , "M=D"
  , "@" ++ name
  , "0;JMP"
  , "(" ++ retAddr ++ ")"
  ]


{- function functionName nVars

(functionName)
repeat nVar times:
push 0
-} 
emitFunction : String -> Int -> List String
emitFunction name nVars =
  let
    sharedName =
      "FUNC_DECL_" ++ name
    
    startLabel =
      sharedName ++ "_START"
      
    endLabel =
      sharedName ++ "_END"
  in
  [ "(" ++ name ++ ")"
  ,  "@" ++ String.fromInt nVars
  , "D=A"
  , "@R14"
  , "M=D"
  , "(" ++ startLabel ++ ")"
  , "@" ++ endLabel
  , "D;JLE"
  ]
  ++ emitPushConstant 0
  ++
  [ "@R14"
  , "MD=M-1"
  , "@" ++ startLabel
  , "0;JMP"
  , "(" ++ endLabel ++ ")"
  ]



emitPushConstant : Int -> List String
emitPushConstant i =
  [ "@" ++ String.fromInt i
  , "D=A"
  , "@SP"
  , "AM=M+1"
  , "A=A-1"
  , "M=D"
  ]


emitLabel : String -> List String
emitLabel label =
  [ "(" ++ emitName label ++ ")"
  ]


emitGoto : String -> List String
emitGoto label =
  [ "@" ++ emitName label
  , "0;JMP"
  ]


emitIfGoto : String -> List String
emitIfGoto label =
  [ "@SP"
  , "AM=M-1"
  , "D=M"
  , "@" ++ emitName label
  , "D;JNE"
  ]


emitName : String -> String
emitName name =
  capitalize name


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
  "D=A"
  :: emitPushD


emitPushM : List String
emitPushM =
  "D=M"
  :: emitPushD


emitPushD : List String
emitPushD =
  [ "@SP"
  , "AM=M+1"
  , "A=A-1"
  , "M=D"
  ]


emitPopA : List String -> List String
emitPopA addr =
  [ "@SP"
  , "AM=M-1"
  , "D=M"
  ] ++ addr
  ++ [ "M=D"
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


capitalize : String -> String
capitalize str =
  mapFirstChar Char.toUpper str


decapitalize : String -> String
decapitalize str =
  mapFirstChar Char.toLower str


mapFirstChar : (Char -> Char) -> String -> String
mapFirstChar f str =
  case String.uncons str of
    Just (firstChar, restChars) ->
      String.cons (f firstChar) restChars
    
    Nothing ->
      ""