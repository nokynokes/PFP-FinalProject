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

getWindowSize : Model -> (Int, Int)
getWindowSize model =
  (model.window.height , model.window.width )

view : Model -> Html Msg
view model =
  let (h, w) = getWindowSize model in
    div []
        [ div []
          [text "Height: ", text <| toString h]
        , div []
          [text "Width: ", text <| toString w]
        , div []
          [
            let path = Collage.segment (0,0) (toFloat w, toFloat h) in
            let line = Collage.traced (Collage.solid black) path in
              toHtml <| Collage.collage h w [line] 
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
