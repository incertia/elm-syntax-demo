import Browser exposing (..)
import Browser.Navigation exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (decodeString, value, at, int, map2, map, errorToString)
import Json.Encode exposing (encode)
import Result exposing (Result(..))
import String exposing (fromInt, join)
import Url exposing (..)
import Parser exposing (run)
import SyntaxHighlight exposing (..)
import SyntaxHighlight.Haskell exposing (parse)

type alias Model =
  { str    : String
  , scroll : Scroll
  , opts   : Options
  }

type Msg = Nop
         | ChangeStr    String
         | ChangeScroll Scroll
         | ToggleNum
         | ToggleColor

type Scroll = Scroll Int Int

init : () -> Url -> Key -> (Model, Cmd Msg)
init () _ _ = ( { str = def
                , scroll = Scroll 0 0
                , opts = { lineNumbers = True, twocolor = True }
                }
              , Cmd.none )

def : String
def = join "\n"
  [ "-- this is a comment"
  , "module Main"
  , "  ( main"
  , "  , Prod(..)"
  , "  , x, y"
  , "  ) where"
  , ""
  , "import Control.Lens.TH (makeLenses)"
  , ""
  , "data Prod = Prod { _x :: Int"
  , "                 , _y :: String"
  , "                 }"
  , "  deriving (Show, Eq)"
  , "makeLenses ''Prod"
  , ""
  , "main :: IO ()"
  , "main = do putStrLn $ \"yolo\""
  ]

view : Model -> Document Msg
view m =
  { title = "title"
  , body = 
    [ div
      [ class "container"
      ]
      [ backdrop m
      , textarea
        [ classList
          [ ("highlight", True)
          , ("elmsh-line-num", m.opts.lineNumbers)
          ]
        , spellcheck False
        , onInput ChangeStr
        , on "scroll"
            ( map2 Scroll
              ( at [ "target", "scrollTop" ] int )
              ( at [ "target", "scrollLeft" ] int )
              |> map ChangeScroll
            )
        ]
        [ text def ]
      ]
    , div
      [ class "buttons" ]
      [ button [ onClick ToggleNum ] [ text "Toggle line numbers" ]
      , button [ onClick ToggleColor ] [ text "Toggle odd/even coloring" ]
      ]
    ]
  }

backdrop : Model -> Html Msg
backdrop m =
  div [ class "backdrop" ]
      [ div
        [ class "highlights"
        , transform m.scroll
        ]
        [ highlight m.opts m.str ]
      ]

highlight : Options -> String -> Html Msg
highlight o src =
  case run parse src of
       Ok s -> annotate s o
       _    -> div [] [ text "whoops" ]

transform : Scroll -> Attribute Msg
transform (Scroll top left) =
  style "transform" <|
        "translate(" ++ fromInt -left ++ "px," ++ fromInt -top ++ "px)"

display : String -> Html Msg
display s =
  div
  [ class "display"
  ]
  [ text s
  ]

decode : String -> String
decode s =
  case decodeString value s of
    Ok  v -> encode 2 v
    Err e -> "bad parse: " ++ (errorToString e)

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  let n =
        case msg of
          ChangeStr s    -> { m | str = s }
          ChangeScroll s -> { m | scroll = s }
          ToggleNum      -> { m | opts = { o | lineNumbers = not o.lineNumbers } }
          ToggleColor    -> { m | opts = { o | twocolor = not o.twocolor } }
          Nop -> m
      o = m.opts
  in
    ( n, Cmd.none )

subs : Model -> Sub Msg
subs mod = Sub.none

urlreq : UrlRequest -> Msg
urlreq _ = Nop

urlchange : Url -> Msg
urlchange _ = Nop

main : Program () Model Msg
main = application
  { init = init
  , view = view
  , update = update
  , subscriptions = subs
  , onUrlRequest = urlreq
  , onUrlChange = urlchange
  }
