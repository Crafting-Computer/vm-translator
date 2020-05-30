module VmParser exposing (parse, showDeadEnds, Instruction(..), Segment(..), Operation(..), Context(..), Problem(..))


import Parser.Advanced exposing (..)
import List.Extra
import Set exposing (Set)
import Dict exposing (Dict)


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
  | ExpectingGoto
  | ExpectingLabel
  | ExpectingIfGoto
  | DuplicatedLabel (Located String) (Located String)
  | UndefinedLabel (Located String)


type Instruction
  = InsPush Segment Int
  | InsPop Segment Int
  | InsArith Operation
  | InsGoto (Located String)
  | InsIfGoto (Located String)
  | InsLabel (Located String)


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


reserved : Set String
reserved =
  Set.empty


parse : String -> Result (List (DeadEnd Context Problem)) (List Instruction)
parse src =
  run
    ( succeed identity
      |= parseInstructions
      |. end ExpectingEOF
    )
    src
  |> Result.andThen
  (\(instructions, declaredLabels, usedLabels) ->
    Dict.foldl
      (\_ usedLabel problems ->
        case Dict.get usedLabel.value declaredLabels of
        Just _ ->
          problems
        
        Nothing ->
          { row = Tuple.first usedLabel.from
          , col = Tuple.second usedLabel.from
          , problem = UndefinedLabel usedLabel
          , contextStack = []
          }
          :: problems
      )
      []
      usedLabels
    |>
    (\problems ->
      if List.isEmpty problems then
        Ok instructions
      else
        Err problems
    )
  )


parseInstructions : VmParser (List Instruction, Dict String (Located String), Dict String (Located String))
parseInstructions =
  loop ([], Dict.empty, Dict.empty)
  (\(revInstructions, declaredLabels, usedLabels) ->
    succeed identity
    |. sps
    |= oneOf
      [ succeed identity
        |= parseInstruction
        |. sps
      |>
      andThen
      (\instruction ->
        let
          success =
            ( instruction :: revInstructions
            
            , case instruction of
              InsLabel label ->
                Dict.insert label.value label declaredLabels
              
              _ ->
                declaredLabels

            , case instruction of
              InsGoto label ->
                Dict.insert label.value label usedLabels
              
              InsIfGoto label ->
                Dict.insert label.value label usedLabels
              
              _ ->
                usedLabels
            )
        in
        case instruction of
          InsLabel label ->
            case Dict.get label.value declaredLabels of
              Just prevLabel ->
                problem <| DuplicatedLabel prevLabel label

              Nothing ->
                succeed <| Loop success
          
          _ ->
            succeed <| Loop success
      )
      , succeed ()
        |> map (\_ -> Done <| (List.reverse revInstructions, declaredLabels, usedLabels))
      ]
  )


parseInstruction : VmParser Instruction
parseInstruction =
  oneOf
    [ parsePush
    , parsePop
    , parseGoto
    , parseIfGoto
    , parseLabel
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


parseGoto : VmParser Instruction
parseGoto =
  succeed
    InsGoto
    |. keyword (Token "goto" ExpectingGoto)
    |. sps
    |= parseName


parseIfGoto : VmParser Instruction
parseIfGoto =
  succeed
    InsIfGoto
    |. keyword (Token "if-goto" ExpectingIfGoto)
    |. sps
    |= parseName


parseLabel : VmParser Instruction
parseLabel =
  succeed
    InsLabel
    |. keyword (Token "label" ExpectingLabel)
    |. sps
    |= parseName


parseName : VmParser (Located String)
parseName =
  located <|
  variable
    { start = Char.isUpper
    , inner = \c -> (Char.isAlphaNum c && (not <| Char.isLower c)) || c == '_'
    , reserved = reserved
    , expecting = ExpectingLabel
    }


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
  ++ String.join "\n"
  (case first.problem of
    InvalidNumber ->
      [ "I found an invalid number."
      , "Hint: Change it to a nonnegative decimal integer."
      ]
    
    InvalidPointerIndex index ->
      [ "I found an invalid pointer index " ++ String.fromInt index ++ "."
      , "Hint: Change it to either 0 (THIS) or 1 (THAT)."
      ]
    
    InvalidTempIndex index ->
      [ "I found an invalid temp index " ++ String.fromInt index ++ "."
      , "Hint: Change it to an integer between 0 and 7."
      ]
    
    InvalidPopConstant ->
      [ "I found that you are trying to pop a constant."
      , "You can only push a constant to the stack."
      , "Hint: You might want to do one of the followings:"
      , "1. Change pop into push."
      , "2. Pop some other memory segments like local and argument."
      ]

    DuplicatedLabel prevLabel label ->
      [ "I found a duplicated label `" ++ label.value ++ "`. It has appeared before:"
      , showLocation src prevLabel
      , "Hint: Remove one of the duplicated labels."
      ]

    UndefinedLabel label ->
      [ "I found an undefined label `" ++ label.value ++ "`."
      , "Hint: You might want to do one of the followings:"
      , "1. Declare the label somewhere before using it."
      , "2. Change it to another label."
      ]

    _ ->
      let
        problemStrs =
          List.map (.problem >> showProblem) <| List.reverse <| first :: rests
      in
      [ "I'm expecting " ++ String.join " or " problemStrs
      ]
  )
  ++ (if String.isEmpty context then "" else " in the " ++ context ++ ".")


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

    ExpectingGoto ->
      "the keyword 'goto'"
    
    ExpectingIfGoto ->
      "the keyword 'if-goto'"

    ExpectingLabel ->
      "a label"

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


showLocation : String -> Located a -> String
showLocation src location =
  let
    (fromRow, fromCol) =
      location.from
    (toRow, toCol) =
      location.to
  in
  showLocationRange fromRow fromCol toRow toCol src


showLocationRange : Int -> Int -> Int -> Int -> String -> String
showLocationRange startRow startCol endRow endCol src =
  String.join "\n" <|
  List.map
  (\row ->
    let
      rawLine =
        getLine row src
      line =
        String.fromInt row ++ "| " ++ (String.trimLeft <| rawLine)
      offset =
        String.length line - String.length rawLine - 1
      underlineStartCol =
        if row == startRow then
          offset + startCol
        else
          1
      underlineEndCol =
        if row == endRow then
          offset + endCol
        else
          String.length line
      underline =
        makeUnderline line underlineStartCol underlineEndCol
    in
    line ++ "\n" ++ underline
  )
  (List.range startRow endRow)


type alias Located a =
  { from : (Int, Int)
  , value : a
  , to : (Int, Int)
  }


located : VmParser a -> VmParser (Located a)
located parser =
  succeed Located
    |= getPosition
    |= parser
    |= getPosition