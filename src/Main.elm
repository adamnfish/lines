module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Json.Decode as Decode
import Svg exposing (Svg, circle, line, rect, svg)
import Svg.Attributes exposing (..)
import Svg.Events
import Task



---- MODEL ----


type alias Model =
    { pictures : List Picture
    , time : Float
    , perturbations : List Perturbation
    , canvasElement : Browser.Dom.Element
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


type alias Perturbation =
    { when : Float
    , at : Vector
    }


init : ( Model, Cmd Msg )
init =
    let
        initialTime =
            0

        perturbations =
            []

        emptyViewport : Browser.Dom.Element
        emptyViewport =
            { scene =
                { width = 0
                , height = 0
                }
            , viewport =
                { x = 0
                , y = 0
                , width = 0
                , height = 0
                }
            , element =
                { x = 0
                , y = 0
                , width = 0
                , height = 0
                }
            }
    in
    ( { pictures =
            lotsOfLines initialTime perturbations 0
      , time = initialTime
      , perturbations = perturbations
      , canvasElement =
            emptyViewport
      }
    , Task.attempt
        (\r ->
            case r of
                Ok element ->
                    UpdateViewportInfo element

                Err error ->
                    NoOp
        )
        (Browser.Dom.getElement "art-canvas")
    )


lotsOfLines : Float -> List Perturbation -> Float -> List Picture
lotsOfLines time perturbations angle =
    let
        axisCount =
            70

        boxSize =
            1000 / axisCount

        adjustedAngle : Int -> Int -> Float -> Float
        adjustedAngle xi yi oldAngle =
            oldAngle
                + (0.5 * toFloat (xi - axisCount))
                + (0.4 * toFloat (yi - axisCount))
    in
    List.concatMap
        (\xi ->
            List.map
                (\yi ->
                    ( boxAtCoord time perturbations axisCount xi yi boxSize
                    , Line { angle = adjustedAngle xi yi angle }
                    )
                )
                (List.range 1 axisCount)
        )
        (List.range 1 axisCount)


boxAtCoord : Float -> List Perturbation -> Int -> Int -> Int -> Float -> Box
boxAtCoord time perturbations axisCount xi yi boxSize =
    let
        tlx =
            boxSize * toFloat xi

        tly =
            boxSize * toFloat yi

        brx =
            boxSize + (boxSize * toFloat xi)

        bry =
            boxSize + (boxSize * toFloat yi)

        totalSize =
            toFloat axisCount * boxSize

        ( tlOffset, brOffset ) =
            List.foldl
                (\perturbation ( tlAcc, brAcc ) ->
                    let
                        timeFactor =
                            (perturbation.when + 3000 - time) / 3000

                        xRelativeDistance =
                            perturbation.at.x - (tlx + (boxSize / 2))

                        yRelativeDistance =
                            perturbation.at.y - (bry + (boxSize / 2))
                    in
                    ( (timeFactor * (xRelativeDistance / totalSize)) + tlAcc
                    , (timeFactor * (yRelativeDistance / totalSize)) + brAcc
                    )
                )
                ( toFloat 0, toFloat 0 )
                perturbations

        topLeft : Vector
        topLeft =
            { x = tlx + (tlOffset * boxSize)
            , y = tly + (tlOffset * boxSize)
            }

        bottomRight : Vector
        bottomRight =
            { x = brx + (brOffset * boxSize)
            , y = bry + (brOffset * boxSize)
            }
    in
    { topLeft = topLeft
    , bottomRight = bottomRight
    }


clickLocationDecoder : Float -> Float -> Decode.Decoder Msg
clickLocationDecoder xOffset yOffset =
    Decode.map2
        (\px py ->
            Perturb
                (px - xOffset)
                (py - yOffset)
        )
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)



---- UPDATE ----


type Msg
    = NoOp
    | Tick Float
    | Perturb Float Float
    | UpdateViewportInfo Browser.Dom.Element


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
                    newTime / 100
            in
            ( { model
                | time = newTime
                , pictures = lotsOfLines newTime model.perturbations newAngle

                -- remove old perturbations
                , perturbations =
                    List.filter
                        (\p -> p.when > newTime - 3000)
                        model.perturbations
              }
            , Cmd.none
            )

        Perturb x y ->
            ( { model
                | perturbations =
                    { when = model.time
                    , at = { x = x, y = y }
                    }
                        :: model.perturbations
              }
            , Cmd.none
            )

        UpdateViewportInfo element ->
            ( { model
                | canvasElement = element
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
        [ id "art-canvas"
        , width "800px"
        , height "800px"
        , viewBox "0 0 1000 1000"
        , Svg.Events.on "click"
            (clickLocationDecoder
                (1000 * model.canvasElement.element.x / 800)
                (1000 - (1000 * model.canvasElement.element.y / 800))
            )
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
