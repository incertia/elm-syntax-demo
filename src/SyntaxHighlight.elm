module SyntaxHighlight exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class, classList)
import String exposing (fromInt)

type alias Source =
  { lines : List Line
  }

type alias Line =
  { number : Int
  , tokens : List Token
  }

type alias Token =
  { typ  : Type
  , str  : String
  }

type Type = Whitespace
          | Text
          | Comment
          | BoolLit
          | IntLit
          | CharLit
          | StringLit
          | Keyword
          | Type
          | Identifier
          | Bracket
          | Operator
          | Special
          | Unknown

type alias Options =
  { lineNumbers : Bool
  , twocolor    : Bool
  }

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

annotate : Source -> Options -> Html a
annotate s opts = div [ class "elmsh-sourceview" ]
                      ( List.map (flip annotateLine opts) s.lines )

annotateLine : Line -> Options -> Html a
annotateLine l opts =
  div
    [ classList
      [ ("elmsh-line", True)
      , ("elmsh-line-odd", modBy 2 l.number == 1 && opts.twocolor)
      , ("elmsh-line-even", modBy 2 l.number == 0 && opts.twocolor)
      , ("elmsh-line-num", opts.lineNumbers)
      ]
    , attribute "data-elmsh-line-num" <| fromInt l.number
    ] <|
    -- there's this weird case where an empty div takes up no space
    if l.tokens == [] then
       [ text "\n" ]
    else
       List.map (flip annotateToken opts) l.tokens

annotateToken : Token -> Options -> Html a
annotateToken t _ = span [ class ("elmsh-" ++ typestr t.typ) ] [ text t.str ]

typestr : Type -> String
typestr t =
  case t of
    Whitespace -> "ws"
    Text       -> "text"
    Comment    -> "com"
    BoolLit    -> "bool"
    IntLit     -> "int"
    CharLit    -> "char"
    StringLit  -> "str"
    Keyword    -> "kw"
    Type       -> "type"
    Identifier -> "id"
    Bracket    -> "brak"
    Operator   -> "op"
    Special    -> "spec"
    Unknown    -> "unk"
