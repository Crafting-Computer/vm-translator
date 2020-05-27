module VmParser exposing (parse, showDeadEnds, Instruction(..), Segment(..), Operation(..), Context(..), Problem(..))


import Parser.Advanced exposing (..)
import List.Extra


type alias VmParser a =
  Parser Context Problem a


type Context
  = CtxPush
  | CtxPop


type Problem
  = ExpectingPush
  | ExpectingPop
  | ExpectingArith
  | ExpectingSegment
  | ExpectingInt
  | InvalidNumber
  | InvalidPointerIndex Int
  | InvalidTempIndex Int
  | InvalidPopConstant
  | ExpectingEOF
  | ExpectingStartOfLineComment
  | ExpectingStartOfMultiLineComment
  | ExpectingEndOfMultiLineComment


type Instruction
  = InsPush Segment Int
  | InsPop Segment Int
  | InsArith Operation


type Segment
  = SegLocal
  | SegArgument
  | SegThis
  | SegThat
  | SegConstant
  | SegStatic
  | SegPointer
  | SegTemp


type Operation
  = OpAdd
  | OpSub
  | OpNeg
  | OpEq
  | OpGt
  | OpLt
  | OpAnd
  | OpOr
  | OpNot


parse : String -> Result (List (DeadEnd Context Problem)) (List Instruction)
parse src =
  run
    ( succeed identity
      |= parseInstructions
      |. end ExpectingEOF
    )
    src


parseInstructions : VmParser (List Instruction)
parseInstructions =
  loop []
  (\revInstructions ->
    succeed identity
    |. sps
    |= oneOf
      [ succeed (\instruction -> Loop <| instruction :: revInstructions)
        |= parseInstruction
        |. sps
      , succeed ()
        |> map (\_ -> Done <| List.reverse revInstructions)
      ]
  )


parseInstruction : VmParser Instruction
parseInstruction =
  oneOf
    [ parsePush
    , parsePop
    , parseArith
    ]


parsePush : VmParser Instruction
parsePush =
  succeed identity
    |. keyword (Token "push" ExpectingPush)
    |= ( inContext CtxPush <|
      (( succeed Tuple.pair
        |. sps
        |= parseSegment
        |. sps
        |= int ExpectingInt InvalidNumber
      ) |>
      andThen
      (\(segment, index) ->
        case segment of
          SegPointer ->
            if index /= 0 && index /= 1 then
              problem <| InvalidPointerIndex index
            else
              succeed <| InsPush segment index
            
          SegTemp ->
            if index > 7 then
              problem <| InvalidTempIndex index
            else
              succeed <| InsPush segment index
          
          _ ->
            succeed <| InsPush segment index
      ))
    )


parsePop : VmParser Instruction
parsePop =
  succeed identity
    |. keyword (Token "pop" ExpectingPop)
    |= ( inContext CtxPop <|
      (( succeed Tuple.pair
        |. sps
        |= parseSegment
        |. sps
        |= int ExpectingInt InvalidNumber
      ) |>
      andThen
      (\(segment, index) ->
        case segment of
          SegPointer ->
            if index /= 0 && index /= 1 then
              problem <| InvalidPointerIndex index
            else
              succeed <| InsPop segment index
                      
          SegTemp ->
            if index > 7 then
              problem <| InvalidTempIndex index
            else
              succeed <| InsPop segment index
          
          SegConstant ->
            problem <| InvalidPopConstant
          
          _ ->
            succeed <| InsPop segment index
      ))
    )


parseArith : VmParser Instruction
parseArith =
  ( getChompedString <|
    succeed ()
    |. chompWhile (Char.isAlpha)
  ) |>
  andThen
  (\name ->
    map InsArith <|
    case name of
      "add" -> succeed OpAdd
      "sub" -> succeed OpSub
      "neg" -> succeed OpNeg
      "eq" -> succeed OpEq
      "gt" -> succeed OpGt
      "lt" -> succeed OpLt
      "and" -> succeed OpAnd
      "or" -> succeed OpOr
      "not" -> succeed OpNot
      _ -> problem ExpectingArith
  )



parseSegment : VmParser Segment
parseSegment =
  ( getChompedString <|
    succeed ()
    |. chompWhile (Char.isAlpha)
  ) |>
  andThen
  (\name ->
    case name of
      "local" -> succeed SegLocal
      "argument" -> succeed SegArgument
      "this" -> succeed SegThis
      "that" -> succeed SegThat
      "constant" -> succeed SegConstant
      "static" -> succeed SegStatic
      "pointer" -> succeed SegPointer
      "temp" -> succeed SegTemp
      _ -> problem ExpectingSegment
  )


sps : VmParser ()
sps =
  loop 0 <| ifProgress <|
    oneOf
      [ succeed () |. symbol (Token "--" ExpectingStartOfLineComment) |. chompWhile (\c -> c /= '\n')
      , multiComment (Token "{-" ExpectingStartOfMultiLineComment) (Token "-}" ExpectingEndOfMultiLineComment) Nestable
      , spaces
      ]


ifProgress : VmParser a -> Int -> VmParser (Step Int ())
ifProgress parser offset =
  succeed identity
    |. parser
    |= getOffset
    |> map (\newOffset -> if offset == newOffset then Done () else Loop newOffset)


showDeadEnds : Maybe Int -> String -> List (DeadEnd Context Problem) -> String
showDeadEnds lineNumber src deadEnds =
  let
    deadEndGroups =
      List.Extra.groupWhile (\d1 d2 -> d1.row == d2.row && d1.col == d2.col) <| deadEnds
  in
  String.join "\n" <| List.map (showDeadEndsHelper lineNumber src) deadEndGroups


showDeadEndsHelper : Maybe Int -> String -> ((DeadEnd Context Problem), List (DeadEnd Context Problem)) -> String
showDeadEndsHelper lineNumber src (first, rests) =
  let
    location =
      showProblemLocation lineNumber first.row first.col src
    context =
      showProblemContextStack first.contextStack
  in
  location ++ "\n"
  ++ (case first.problem of
    InvalidNumber ->
      "I found an invalid number. I'm expecting a decimal integer"
    
    InvalidPointerIndex index ->
      "I found an invalid pointer index " ++ String.fromInt index ++ ". I'm expecting either 0 (THIS) or 1 (THAT)"
    
    InvalidTempIndex index ->
      "I found an invalid temp index " ++ String.fromInt index ++ ". I'm expecting an integer between 0 and 7"
    
    InvalidPopConstant ->
      "I found that you are trying to pop a constant. You can only push a constant to the stack"

    _ ->
      let
        problemStrs =
          List.map (.problem >> showProblem) <| List.reverse <| first :: rests
      in
      "I'm expecting " ++ String.join " or " problemStrs
  )
  ++ (if String.isEmpty context then "" else " in the " ++ context)
  ++ "."


showProblem : Problem -> String
showProblem p =
  case p of
    ExpectingEOF ->
      "the end of program"
    
    ExpectingInt ->
      "an integer"

    ExpectingStartOfLineComment ->
      "the start of a single-line comment '--'"
    
    ExpectingStartOfMultiLineComment ->
      "the start of a multi-line comment '{-'"
    
    ExpectingEndOfMultiLineComment ->
      "the end of a multi-line comment '-}'"

    ExpectingPush ->
      "a push instruction"
    
    ExpectingPop ->
      "a pop instruction"
    
    ExpectingArith ->
      "an arithmetic instruction"
    
    ExpectingSegment ->
      "a segment name"

    _ ->
      "PROBLEM"


showProblemContextStack : List { row : Int, col : Int, context : Context } -> String
showProblemContextStack contexts =
  String.join " of the " <| List.map (.context >> showProblemContext) contexts


showProblemContext : Context -> String
showProblemContext context =
  case context of
    CtxPush ->
      "push instruction"
    
    CtxPop ->
      "pop instruction"


showProblemLocation : Maybe Int -> Int -> Int -> String -> String
showProblemLocation customLineNumber row col src =
  let
    rawLine =
      getLine row src
    lineNumber =
      case customLineNumber of
        Nothing ->
          row
        
        Just number ->
          number
    line =
      String.fromInt lineNumber ++ "| " ++ (String.trimLeft <| rawLine)
    offset =
      String.length line - String.length rawLine - 1
    offsettedCol =
      offset + col
    underline =
      makeUnderline line offsettedCol (offsettedCol + 1)
  in
  line ++ "\n" ++ underline


makeUnderline : String -> Int -> Int -> String
makeUnderline row minCol maxCol =
  String.toList (row ++ " ")
    |> List.indexedMap (\i _ -> toUnderlineChar minCol maxCol i)
    |> String.fromList


toUnderlineChar : Int -> Int -> Int -> Char
toUnderlineChar minCol maxCol col =
  if minCol <= col && col < maxCol then
    '^'
  else
    ' '


getLine : Int -> String -> String
getLine row src =
  Maybe.withDefault ("CAN'T GET LINE AT ROW " ++ String.fromInt row) -- impossible
    <| List.Extra.getAt (row - 1) <| String.split "\n" src
