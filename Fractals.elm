
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

type Msg =
  SizeUpdated Size
  | Tick Time

init : (Model, Cmd Msg)
init = (initialModel, initialSize)

initialSize : Cmd Msg
initialSize =
    Window.size > perform SizeUpdated

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
        treeHeight = 0,
        window = Size 0 0

    }

-- takes in the previous x and y and does point generator based on that

createEndPoint : Point -> Seed -> Point  --takes in previous branch's endpoint and uses as startpoint to help generate its endpoint
createEndPoint start seed = 
    Random.step pointGenerator seed

directionGenerator : Generator Int
directionGenerator = 
    Int 1 2

pointGenerator :  Generator Point  -- Float -> ((Float, Float), Int)
pointGenerator 
  map2 Point (float 0.2 0.8) (float 0.2 0.8)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SizeUpdated newSize -> {model | window = newSize} ! []  -- ! combines what's after it (multiple commands) into one command message
        Tick time ->
            case model.trees of 
                [] -> Debug.crash "wont happen"
                BranchFamily (start, end) children :: t-> 
                    --takes in previous branch's endpoint and uses as startpoint to help generate its endpoint
                    let 
                        seed = Random.initialSeed (round time) 
                        (direction, newSeed1) = Random.step directionGenerator seed
                        (newPt_Scaling, newSeed2) = Random.step pointGenerator (newSeed1) 
                        newPt = {x = direction * newPt_Scaling.x * end.x, y = newPt_Scaling.y * end.y}
                        newTree = BranchFamily (end, newPt) []

                    in
                    ( {model | trees = BranchFamily (start, end) (newTree :: children) :: t}, Cmd.none )

                    
                -- branching off of height = 0
                seed = Random.initialSeed (round time) 
                model.trees
                (newPoint, newSeed) = Random.step pointGenerator seed in
            in



subscriptions : Model -> Sub Msg
subscriptions model =
    batch  -- way to listen for multiple subscriptions
        [ resizes SizeUpdated
        , Time.every (5 second) Tick]

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
