
module Fractals exposing (main)


import Random exposing (Generator, Seed, map2, float)
import Time exposing (Time, second)
import Html exposing (Html, Attribute)
import Html.Attributes exposing (style)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Platform.Sub exposing (batch)
import Window exposing (Size, resizes)
import Task exposing (perform)


main : Program Never Model Msg
main = 
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Point = { x:Float, y:Float }
type alias Branch = (Point, Point) -- (start, end)
type Tree = Empty | BranchFamily Branch (List Tree)

type alias Model = 
    {
        trees : List Tree,
        window: Window.Size

    }

type Msg = SizeUpdated Size

init : (Model, Cmd Msg)
init = (initialModel, initialSize)

initialSize : Cmd Msg
initialSize = 
    Window.size |> perform SizeUpdated
    

initialModel : Model
initialModel = 
    {
        trees = 
            [
                BranchFamily ({x = 100, y = 100}, {x = 100, y = 140})  []
            ],
        window = Size 0 0

    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SizeUpdated newSize -> {model | window = newSize} ! []  -- ! combines what's after it (multiple commands) into one command message
        {-
        Tick time ->
            let seed = Random.initialSeed (round time) in
        -}



subscriptions : Model -> Sub Msg
subscriptions model = 
    batch  -- way to listen for multiple subscriptions
        [ resizes SizeUpdated ]--, Time.every second Tick] 

getWindowSize : Model -> (Int, Int)
getWindowSize m = 
    (m.window.height, m.window.width)

view : Model -> Html Msg
view model = 
    let (h, w) = getWindowSize model in
    Html.div []
        [
            toHtml <| Collage.collage h w []
        ]

                
                