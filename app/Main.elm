module Main exposing (..)

import Html exposing (Html, text, input, code, div)
import Html.Attributes exposing (style, type_)
import Formatting exposing (print)
import Html.Events exposing (onInput)
import Result exposing (withDefault)


main =
    Html.program
        { init = ( defaults, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = Update_pA String
    | Update_pBgivenA String
    | Update_pBgivenNotA String


type alias Model =
    { pA : Probability
    , pBgivenA : Probability
    , pBgivenNotA : Probability
    , pAgivenB : Probability
    }


type alias Probability =
    Float


defaults : Model
defaults =
    { pA = 0.5
    , pBgivenA = 0.5
    , pBgivenNotA = 0.5
    , pAgivenB = 0.5
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Update_pA pString ->
                    { model | pA = parseProbability pString |> withDefault model.pA }

                Update_pBgivenA pString ->
                    { model | pBgivenA = parseProbability pString |> withDefault model.pBgivenA }

                Update_pBgivenNotA pString ->
                    { model | pBgivenNotA = parseProbability pString |> withDefault model.pBgivenNotA }
    in
        ( calculateAgivenB newModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ showProbability model.pA "p(A)" Update_pA
        , showProbability model.pBgivenA "p(B|A)" Update_pBgivenA
        , showProbability model.pBgivenNotA "p(B|¬A)" Update_pBgivenNotA
        , showReadOnlyProbability model.pAgivenB "p(A|B)"
        ]


showProbability : Probability -> String -> (String -> Msg) -> Html Msg
showProbability p name msg =
    div []
        [ div [ style [ "min-width" => "20em" ] ]
            [ name ++ ": " |> text
            , code [] [ p |> prettyFloat 6 |> text ]
            ]
        , input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "1"
            , Html.Attributes.step "0.001"
            , Html.Attributes.value (toString p)
            , onInput msg
            ]
            []
        ]


showReadOnlyProbability : Probability -> String -> Html Msg
showReadOnlyProbability p name =
    div []
        [ div [ style [ "min-width" => "20em" ] ]
            [ name ++ ": " |> text
            , code [] [ p |> prettyFloat 6 |> text ]
            ]
        , input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "1"
            , Html.Attributes.step "0.001"
            , Html.Attributes.value (toString p)
            , Html.Attributes.disabled True
            ]
            []
        ]


prettyFloat : Int -> Float -> String
prettyFloat dp n =
    print (Formatting.roundTo dp |> Formatting.padLeft 6 ' ') n


parseProbability : String -> Result String Probability
parseProbability pString =
    pString |> String.toFloat


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


calculateAgivenB : Model -> Model
calculateAgivenB m =
    let
        pNotA =
            1 - m.pA

        pAgivenB =
            (m.pBgivenA * m.pA) / (m.pBgivenA * m.pA + m.pBgivenNotA * pNotA)
    in
        { m | pAgivenB = pAgivenB }
