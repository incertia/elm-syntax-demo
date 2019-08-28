module SyntaxHighlight.Haskell exposing (..)

import Char exposing (isHexDigit, isDigit, isUpper)
import Parser exposing (Parser, succeed, oneOf, getChompedString, end,
                        chompIf, chompWhile, andThen, keyword, token, (|.),
                        problem, backtrackable)
import String exposing (fromChar, concat, all)
import SyntaxHighlight exposing (Source, Line, Token, Type(..))

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

mk : Type -> Parser String -> Parser Token
mk = andThen << ((<<) succeed) << Token

-- endline parses the end of a line
endline : Parser String
endline = getChompedString <| chompEq '\n'

-- eol parses the end of line and returns True and False otherwise
eol : Parser Bool
eol = oneOf
  [ Parser.map (always True) endline
  , succeed False
  ]

-- ensures that the parsed string is non-empty
nonempty : Parser String -> Parser String
nonempty p = p |> andThen checkEmpty

checkEmpty : String -> Parser String
checkEmpty s =
  if s == "" then
     problem "parsed an empty string"
  else
     succeed s

-- just a helper function, any further abstraction causes a stack overflow for
-- some reason
supercons : a -> Parser (List a) -> Parser (List a)
supercons r p = p |> andThen (\xs -> succeed (r :: xs))

supercons2 : Parser a -> Parser (List a) -> Parser (List a)
supercons2 p q = p |> andThen (\x -> supercons x q)

-- some applies a parser many times until it fails to parse
some : Parser a -> Parser (List a)
some p = oneOf
  -- we then try to apply the parser and then monadically concat the results
  -- together
  [ p |> andThen (\x -> supercons x <| some p)
  --supercons2 p <| some p
  -- in the case of our parser failing, we just return an empty list
  , succeed []
  ]

-- many is like some but there is at least 1
many : Parser a -> Parser (List a)
many p = p |> andThen (\x -> supercons x <| some p)

-- parses an entire source string
parse : Parser Source
parse = parsel 1 |> andThen (succeed << Source)

-- parses a sequence of lines
parsel : Int -> Parser (List Line)
parsel i =
  line i
    |> andThen (\x -> eol
      |> andThen (\e ->
        if e then
           -- we found a newline, so we parse the next line
           supercons x <| parsel <| i + 1
        else
           -- this is the end of file
           succeed <| [x]
        ))

-- parses a line given a line number
line : Int -> Parser Line
line i = some tok |> andThen (\x -> succeed <| Line i x)

-- a token is one of many things
tok : Parser Token
tok = oneOf
  [ kw
  , spaces
  , com
  , brak
  , tl
  , string
  , infix
  , char
  , bool
  , int
  , op
  , typ
  , id
  , st
  , unk
  ]

-- parses a boolean literal
bool : Parser Token
bool = mk BoolLit <| getChompedString <| oneOf <| List.map keyword bools

bools : List String
bools =
  [ "True"
  , "False"
  ]

-- parses an integer literal
int : Parser Token
int = mk IntLit <| oneOf
  -- hex
  [ backtrackable
    <| (chompEqStr '0'
      |> andThen (\a -> chompEqStr 'x'
        |> andThen (\b -> nonempty (chompNotMemStr ws)
          |> andThen (\c ->
            if all isHexDigit c then
               succeed <| a ++ b ++ c
            else
               problem "encountered non-hex digits"))))
  -- decimal
  , backtrackable
    <| (nonempty (chompNotMemStr ws)
      |> andThen (\x ->
        if all isDigit x then
           succeed x
        else
           problem "encountered non-decimal digits"))
  ]

-- parses some whitespace as a Token
spaces : Parser Token
spaces = mk Whitespace <| nonempty <| chompMemStr <| ws

ws : List Char
ws = [' ', '\t', '\r']

-- parses a reserved keyword
kw : Parser Token
kw = mk Keyword <| getChompedString <| oneOf <| List.map keyword kws

kws : List String
kws =
  [ "module", "import", "qualified", "hiding", "as"
  , "type", "newtype", "data", "deriving"
  , "family"
  , "class", "instance"
  , "forall"
  , "default"
  , "infix", "infixl", "infixr"
  , "where"
  , "let", "in"
  , "case", "of"
  , "do"
  , "if", "then", "else"
  , "foreign"
  , "undefined"
  ]

-- parses a thing that appears in bracketed expressions
brak : Parser Token
brak = mk Bracket <| nonempty <| chompMemStr brakc

brakc : List Char
brakc =
  [ '(', ')'
  , '[', ']'
  , '{', '}'
  , ','
  ]

-- parses symbols for type level lists, namely the '[ and ': symbols
tl : Parser Token
tl = mk Bracket <| getChompedString <| backtrackable <| chompEq '\'' |. oneOf
  [ chompEq '['
  , chompEq ':'
  ]

-- parse an operator
op : Parser Token
op = mk Operator <| nonempty <| chompMemStr <| opc

opc : List Char
opc =
  [ '<', '>'
  , '+', '-', '*', '/', '\\', '='
  , '|'
  , '~', '!', '@', '#', '$', '%', '^', '&'
  , ':', ';'
  , ',', '.', '?'
  ]

-- parses an identifier
id : Parser Token
id = mk Identifier <| getChompedString
                   <| chompIf (not << badid) |. chompNotMem sep
                                             |. chompWhile ((==) '#')

badid c = c == '\'' || isDigit c || List.member c sep

-- a type is basically an identifier with a leading uppercase
typ : Parser Token
typ = mk Type <| nonempty
              <| getChompedString
              <| chompIf isUpper |. chompNotMem sep
                                 |. chompWhile ((==) '#')

-- parse a TH single tick thingy
st : Parser Token
st = mk Special
  <| backtrackable (chompEqStr '\''
    |> andThen (\a -> nonempty (chompNotMemStr sep)
      |> andThen (\b -> chompWhileStr ((==) '#')
        |> andThen (\c -> succeed <| a ++ b ++ c))))

sep : List Char
sep =
  [ '(', ')'
  , '[', ']'
  , '{', '}'
  , '<', '>'
  , '+', '-', '*', '/', '\\', '='
  , '|'
  , '~', '!', '@', '#', '$', '%', '^', '&'
  , ':', ';'
  , ',', '.', '?'
  , '\r', '\n', '\t', '`', '"', ' '
  ]

-- parses a single char literal
char : Parser Token
char = mk CharLit <| backtrackable <| chr

chr : Parser String
chr = chompEqStr '\''
  |> andThen (\a -> chrtok
    |> andThen (\b -> chompEqStr '\''
      |> andThen (\c -> succeed <| a ++ b ++ c)))

chrtok : Parser String
chrtok = oneOf
  [ getChompedString <| chompIf (not << chrspecial)
  , getChompedString <| (token "\\" |> andThen (always esc))
  ]

chrspecial : Char -> Bool
chrspecial x = x == '\'' || x == '\\'

-- parses a single line string literal
string : Parser Token
string = mk StringLit <| backtrackable <| str

-- we likely need context to correctly parse multiline strings, so we can resume
-- parsing a string if the last token was an unclosed string
str : Parser String
str = chompEqStr '"'
  |> andThen (\a -> some strtok
    |> andThen (\b -> chompEqStr '"'
      |> andThen (\c -> succeed <| a ++ concat b ++ c)))

strtok : Parser String
strtok = oneOf
  [ getChompedString <| chompIf (not << strspecial)
  , getChompedString <| (token "\\" |> andThen (always esc))
  ]

strspecial : Char -> Bool
strspecial x = x == '"' || x == '\\'

-- parses the escape sequences
esc : Parser String
esc = oneOf
  [ getChompedString <| (token "x" |> andThen (always <| chompWhile isHexDigit))
  , nonempty <| chompWhileStr isDigit
  , getChompedString <|chompIf (always True)
  ]

-- handle the special a `infix` b case
infix : Parser Token
infix = mk Operator <| backtrackable <| inf

inf : Parser String
inf = chompEqStr '`'
  |> andThen (\a -> chompWhileStr ((/=) '`')
    |> andThen (\b -> chompEqStr '`'
      |> andThen (\c -> succeed <| a ++ b ++ c)))

-- parses a single line comment
com : Parser Token
com = mk Comment <| getChompedString
                 <| backtrackable
                 <| chompEq '-' |. chompEq '-' |. chompWhile ((/=) '\n')

-- the default path of our parser, just parses until the end of line
unk : Parser Token
unk = mk Unknown <| nonempty <| chompWhileStr ((/=) '\n')

-- chomp consumes 1 character
chomp : Parser ()
chomp = chompIf (always True)

-- chompEq chomps if we get the character
chompEq : Char -> Parser ()
chompEq c = chompIf ((==) c)

chompEqStr : Char -> Parser String
chompEqStr c = getChompedString <| chompIf ((==) c)

-- parses the string out of the chomp
chompWhileStr : (Char -> Bool) -> Parser String
chompWhileStr = getChompedString << chompWhile

-- helper functions for dealing with lists and their string variants
chompMem : List Char -> Parser ()
chompMem = chompWhile << flip List.member

chompMemStr : List Char -> Parser String
chompMemStr = getChompedString << chompWhile << flip List.member

chompNotMem : List Char -> Parser ()
chompNotMem = chompWhile << ((<<) not) << flip List.member

chompNotMemStr : List Char -> Parser String
chompNotMemStr = getChompedString << chompWhile << ((<<) not) << flip List.member
