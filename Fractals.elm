
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

initialChildren : List Tree
initialChildren =
  [
    (BranchFamily ({x = 100, y = 140}, {x = 120, y = 160}) [(BranchFamily ({x = 120, y = 160}, {x = 110, y = 200})  [])])
    , (BranchFamily ({x = 100, y = 120}, {x = 180, y = 140})  [(BranchFamily ({x = 180, y = 140}, {x = 180, y = 200})  [])])
  ]

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

makeBranch : Branch -> Form
makeBranch (start, end) =

   Collage.traced (Collage.solid blue) (Collage.segment (start.x, start.y) (end.x, end.y))

getBranches : List Tree -> List Form -> List Form
getBranches t_lst f_lst =
    case t_lst of
        [] -> f_lst
        (BranchFamily (s1, e1) c1) :: t1 ->
            case c1 of
                [] -> getBranches t1 ( (makeBranch (s1, e1)) :: f_lst)
                BranchFamily _ _ :: _ -> (getBranches c1  ( (makeBranch (s1, e1)) :: f_lst )) |> getBranches t1
                _ -> Debug.crash "Shouldn't get here"
        _ -> Debug.crash "Shouldn't get here"




view : Model -> Html Msg
view model =
    let
        (h, w) = getWindowSize model
        trees = getBranches model.trees []
    in
      Html.div []
          [
              toHtml <| Collage.collage h w trees
          ]
