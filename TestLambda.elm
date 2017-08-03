-- Check whether these functions are working
module TestLambda exposing (..)
import LambdaParser exposing (..)
import Dict exposing (..)

id = getExprFromResult <| parse expr "(\\x.x)"

zero = getExprFromResult <| parse expr "(\\f x.x)"
one = getExprFromResult <| parse expr "(\\f x.f x)"
two = getExprFromResult <| parse expr "(\\f x.f (f x))"

true = getExprFromResult <| parse expr "(\\x y.x)"
false = getExprFromResult <| parse expr "(\\x y.y)"

ifte = getExprFromResult <| parse expr "(\\b x y. b x y)"

iszero = Abstraction "x" (Combination (Combination (Variable "x") (Abstraction "y" false)) true)

defs = [zero, one, two, true, false, ifte, iszero]

names = [
 ("id", id),("zero", zero), ("one",one), ("two",two), ("true",true), ("false",false), ("ifte",ifte), ("iszero",iszero)]
