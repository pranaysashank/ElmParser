module LambdaRepl exposing (..)

import LambdaParser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    { content : String
    }


model : Model
model =
    { content = ""
    }


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to parse", onInput Change ] []
        , div [] [ text (toString <| evalExpr (r expr model.content)) ]
        ]
