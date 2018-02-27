module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Task exposing (perform)
import Time exposing (..)
import Html exposing (..)
import Color exposing (..)
import Collage exposing (Shape,Form)
import Text exposing (fromString,Text)
import Element exposing (toHtml)
import Platform.Sub exposing (batch)
import Window exposing (Size,resizes)


---- MODEL ----


type alias Model =
    {window: Window.Size}


init : ( Model, Cmd Msg )
init =
    ( initialModel , initialSize )

initialSize : Cmd Msg
initialSize =
  Window.size
    |> perform SizeUpdated

initialModel : Model
initialModel =
  {window = Size 0 0}



---- UPDATE ----


type Msg
    = SizeUpdated Size
    -- | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SizeUpdated newSize ->
      { model | window = newSize } ! []



---- VIEW ----


view : Model -> Html Msg
view model =
  let h = model.window.height in
  let w = model.window.width in
    div []
        [ h1 []
            [
              text <| toString h
            , text <| toString w
            ]
        ]


---- SUBSCRIPTIONS ----
subscriptions : Model -> Sub Msg
subscriptions model =
  batch
    -- [ every millisecond (\_ -> Tick)
    [ resizes SizeUpdated ]

---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
