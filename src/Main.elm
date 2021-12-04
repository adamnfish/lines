module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Svg exposing (Svg, circle, line, rect, svg)
import Svg.Attributes exposing (..)
import Time exposing (Posix)



---- MODEL ----


type alias Model =
    { pictures : List Picture
    , time : Float
    }


type alias Vector =
    { x : Float
    , y : Float
    }


type alias Box =
    { topLeft : Vector
    , bottomRight : Vector
    }


type Shape
    = Line { angle : Float }
    | Blank


type alias Picture =
    ( Box, Shape )


init : ( Model, Cmd Msg )
init =
    ( { pictures =
            lotsOfLines 0
      , time = 0
      }
    , Cmd.none
    )


lotsOfLines : Float -> List Picture
lotsOfLines angle =
    let
        axisCount =
            50

        boxSize =
            1000 // axisCount

        adjustedAngle : Int -> Int -> Float -> Float
        adjustedAngle xi yi oldAngle =
            oldAngle * toFloat (xi - axisCount) * toFloat (yi - axisCount)
    in
    List.concatMap
        (\xi ->
            List.map
                (\yi ->
                    ( boxAtCoord xi yi boxSize
                    , Line { angle = adjustedAngle xi yi angle }
                    )
                )
                (List.range 1 axisCount)
        )
        (List.range 1 axisCount)


boxAtCoord : Int -> Int -> Int -> Box
boxAtCoord xi yi boxSize =
    let
        topLeft : Vector
        topLeft =
            { x = toFloat <| boxSize * xi
            , y = toFloat <| boxSize * yi
            }

        bottomRight : Vector
        bottomRight =
            { x = toFloat <| boxSize + (boxSize * xi)
            , y = toFloat <| boxSize + (boxSize * yi)
            }
    in
    { topLeft = topLeft
    , bottomRight = bottomRight
    }



---- UPDATE ----


type Msg
    = NoOp
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick delta ->
            let
                newTime =
                    model.time + delta

                newAngle =
                    newTime / 10000
            in
            ( { model
                | time = newTime
                , pictures = lotsOfLines newAngle
              }
            , Cmd.none
            )



---- VIEW ----


render : Picture -> Svg Msg
render ( { topLeft, bottomRight }, shape ) =
    case shape of
        Line props ->
            let
                middleLeft : Vector
                middleLeft =
                    { x = topLeft.x
                    , y = topLeft.y - ((topLeft.y - bottomRight.y) / 2)
                    }

                middleRight : Vector
                middleRight =
                    { x = bottomRight.x
                    , y = bottomRight.y + ((topLeft.y - bottomRight.y) / 2)
                    }

                center : Vector
                center =
                    { x = topLeft.x + ((bottomRight.x - topLeft.x) / 2)
                    , y = topLeft.y + ((bottomRight.y - topLeft.y) / 2)
                    }
            in
            line
                [ stroke "black"
                , x1 <| String.fromFloat middleLeft.x
                , y1 <| String.fromFloat middleLeft.y
                , x2 <| String.fromFloat middleRight.x
                , y2 <| String.fromFloat middleRight.y
                , transform <|
                    "rotate("
                        ++ String.fromFloat props.angle
                        ++ " "
                        ++ String.fromFloat center.x
                        ++ " "
                        ++ String.fromFloat center.y
                        ++ ")"
                ]
                []

        Blank ->
            text "."


view : Model -> Html Msg
view model =
    svg
        [ width "800px"
        , height "800px"
        , viewBox "0 0 1000 1000"
        ]
    <|
        List.map render model.pictures



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always <| Browser.Events.onAnimationFrameDelta Tick
        }
