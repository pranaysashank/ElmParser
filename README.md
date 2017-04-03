# ElmParser

The only way (as of now) to run the _eval_ function is in elm-repl.

To parse a string into a valid Lambda Expression, use the _parse_ function:

```elm
parse : Parser.Parser a -> String -> Result Parser.Error a
```

# Example
```elm
> parse expr "\\x.x"
Ok (Abstraction "x" (Variable "x"))
```

To evaluate a Lambda Expression into a valid Value type, use either the _eval_ or _evalExprFromResult_ functions

```elm
eval : Scope -> Expression -> Value

evalExprFromResult : Result.Result Parser.Error Expression -> Value
```

# Example
```elm
> eval emptyScope (Combination (Abstraction "x" (Variable "x")) (Constant 2))
VInt 2

> evalExprFromResult <| parse expr "(\\x.x) 2"
VInt 2
```

I'm using this [Parser library](http://package.elm-lang.org/packages/elm-tools/parser/latest) to parse strings into Lambda Expressions.
