module LambdaParser exposing (..)

import Parser exposing (Parser, lazy, delayedCommit, repeat, run, oneOf, (|.), (|=), succeed, symbol, keyword, int, float, ignore, zeroOrMore, oneOrMore, keep)
import Parser.LanguageKit as Parser exposing (variable, Trailing(..))
import Char
import Set
import Dict exposing (..)
import Debug exposing (..)
import String exposing (toInt)
import Result exposing (withDefault)

spaces : Parser ()
spaces =
    ignore zeroOrMore (\char -> char == ' ')

parse = run
o = oneOf
rp = repeat
om = oneOrMore
s = succeed

type alias Name = String

type Expression = Variable Name
    | Constant Int
    | Combination Expression Expression
    | Abstraction Name Expression
    | Add Expression Expression
    | Mul Expression Expression
    | Error

str : Parser String
str =
    succeed identity
        |= variable Char.isLower Char.isLower Set.empty
        |. spaces

char : Parser String
char =
    succeed identity
     |= keep (Parser.Exactly 1) Char.isLower
     |. spaces

var : Parser Expression
var =
    succeed Variable
       |= str

const : Parser Expression
const =
    succeed Constant
        |= int
        |. spaces

parens : Parser a -> Parser a
parens p =
    delayedCommit (symbol "(") <|
        succeed identity
            |= p
            |. symbol ")"
            |. spaces

lambda : Parser Expression
lambda =
    (succeed <| flip <| List.foldr Abstraction)
        |. symbol "\\"
        |= (repeat oneOrMore char)
        |. symbol "."
        |. spaces
        |= expr
        |. spaces

add =
    (succeed (Abstraction "x" (Abstraction "y" (Add (Variable "x") (Variable "y")))))
        |. keyword "add"
        |. spaces

mul =
    (succeed (Abstraction "x" (Abstraction "y" (Mul (Variable "x") (Variable "y")))))
        |. keyword "mul"
        |. spaces

op : Parser Expression
op = oneOf
     [
      add
     ,mul
     ]

definition : Parser Expression
definition =
    succeed Abstraction
        |. keyword "define"
        |. spaces
        |= str
        |. spaces
        |. symbol "="
        |. spaces
        |= expr
        |. spaces

term = oneOf
       [
        lazy (\_ -> parens expr)
       ,op
       ,var
       ,const
       ,lazy (\_ -> lambda)
       ]

expr : Parser Expression
expr =
    (succeed <| foldl1 Combination)
        |= (repeat oneOrMore term)
        |. spaces

foldl1 f xs =
    case xs of
        [] -> Debug.crash "Empty List"
        [x] -> x
        (x::xs) -> List.foldl (flip f) x xs

type alias Scope = Dict String Value

type Value = VInt Int
    | VClosure String Expression Scope
    | VFunction String
    | VString String
    | VError

emptyScope : Scope
emptyScope = Dict.empty

type alias DefScope = Dict String Expression

initDefs : DefScope
initDefs = Dict.empty

parseDefs : DefScope -> Expression -> Expression
parseDefs defs expr = case expr of
                          Constant x -> Constant x
                          Variable x -> case (Dict.get x defs) of
                                            Just v -> (parseDefs defs v)
                                            Nothing -> Variable x
                          Abstraction v body -> Abstraction v (parseDefs defs body)
                          Combination a b -> Combination (parseDefs defs a) (parseDefs defs b)
                          Add x y -> Add x y
                          Mul x y -> Mul x y
                          Error -> Error


eval : Scope -> Expression -> Value
eval env expr = case expr of
                    Constant x -> VInt x
                    Variable x -> case (Dict.get x env) of
                                      Just v -> v
                                      Nothing -> VString x
                    Abstraction v body -> VClosure v body env
                    Combination a b ->
                        apply (eval env a) (eval env b)
                    Add (Variable x) (Variable y) -> VInt ((getValue env x) + (getValue env y))
                    Mul (Variable x) (Variable y) -> VInt ((getValue env x) * (getValue env y))
                    _ -> VError

getValue env x = case (Dict.get x env) of
                     Just (VInt v) -> v
                     Just _ -> Debug.crash "Tried to add or multiply a variable and a number"
                     Nothing -> 0

getExprFromResult expr = case expr of
                             Ok x -> x
                             Err _ -> Error

evalExprFromResult : Result.Result Parser.Error Expression -> Value
evalExprFromResult expr = case expr of
                    Ok x -> eval emptyScope x
                    Err _ -> VError

extend env v t = Dict.insert v t env

apply x y = case (x, y) of
                ((VClosure n e clo), ex) ->
                    eval (extend clo n ex) e
                (_, _) -> Debug.crash "Tried to apply non-closure"

-- add = Abstraction "x" (Abstraction "y" (Add (Variable "x") (Variable "y")))
-- mul = Abstraction "x" (Abstraction "y" (Variable "x * y"))

--"(((\\x y.(y x))(((((\\x y. (y x))(((\\x.x) 12)))) (\\x.x))))(\\x.x))"
-- (Combination (Combination (Abstraction "x" (Abstraction "y" (Combination (Variable "y") (Variable "x")))) (Combination (Combination (Abstraction "x" (Abstraction "y" (Combination (Variable "y") (Variable "x")))) (Combination (Abstraction "x" (Variable "x")) (Constant 12))) (Abstraction "x" (Variable "x")))) (Abstraction "x" (Variable "x")))
