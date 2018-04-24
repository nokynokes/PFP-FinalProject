
module Fractals exposing (main)

import Random exposing (Generator, Seed, map2, float, int)
import Time exposing (Time, second, millisecond)
import Html exposing (Html, Attribute)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Platform.Sub exposing (batch)
import Window exposing (Size, resizes)
import Task exposing (perform)
import Mouse exposing (clicks)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Point = { x:Float, y:Float }
--** New Types, implment later ****
--type alias Angle = Float -- in radians (cos and sin need radian arguments)
--type alias Magnitude = Float
--type alias Vector = (Angle, Magnitude)
--type alias Branch = (Point, Vector)
type alias Branch = (Point, Point) -- (start, end)
type alias Height = Int

type Mode = Spawn | Destroy
type Tree = Empty | BranchFamily Branch (List Tree)
type Root = TreeRoot Height Branch (List Tree)

type alias Model =
    {
        trees : List Tree ,
        treeHeight : Int , --Eventually a List Int
        window: Window.Size ,
        seed : Seed ,
        time : Time ,
        mode : Mode
    }

type Msg =
  SizeUpdated Size
  | Tick Time
  | Click Mouse.Position
  | SwitchMode Mode

init : (Model, Cmd Msg)
init = (initialModel, initialSize)

initialSize : Cmd Msg
initialSize =
    Window.size |> perform SizeUpdated

-- this is for testing, not being used currently
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
                BranchFamily ({x = 100, y = 100}, {x = 100, y = 200})  []
                -- This will be it when converted to radians BranchFamily ({x=100, y=100}, ((degree 90), 100))
            ],
        treeHeight = 0,
        window = Size 0 0 ,
        seed = Random.initialSeed 4308,
        time = 0.0,
        mode = Spawn

    }

-- random int between 0 and 11 that makes direction
directionGenerator : Generator Int
directionGenerator =
    int 0 11

-- for help determining height of next insertion
floatGenerator : Generator Float
floatGenerator =
    float 0 1

pointGenerator :  Generator Point  -- Float -> ((Float, Float), Int)
pointGenerator =
  map2 Point (float 0.2 0.8) (float 0.2 0.8)

-- returns the new tree model and tree height of the newly created branch
makeTree : Int -> Seed -> List Tree -> (Int, List Tree)
makeTree insH seed tLst =
    if insH < 0 then Debug.crash "can't have a negative insH"

    else case tLst of
        [] ->
            let log = Debug.log "No Trees exist to insert into" tLst in
            (0, tLst)
        (BranchFamily (start, end) children) :: _ ->
            let
                (randFloat, newSeed) = (Random.step floatGenerator seed)
                treeNum = round ((toFloat ((List.length tLst) - 1)) * randFloat)

                log = Debug.log "root path chosen is" treeNum

            in
            chooseRoot treeNum insH newSeed tLst []

        _ -> Debug.crash "shouldn't reach here"

--chooses the root/path from the list of tree/children
chooseRoot : Int -> Int -> Seed -> List Tree -> List Tree -> (Int, List Tree)
chooseRoot n insH seed tLst hLst =
    case tLst of
        [] -> Debug.crash "exceeded tree list length"
        h :: t ->
            if n == 0 then
                let (newBranchHeight, changedTree) = insertBranch insH seed h in
                (newBranchHeight, (hLst ++ (changedTree :: t)) )

            else chooseRoot (n-1) insH seed t (h :: hLst)

--creates branch to be inserted once correct position is identified
addBranch : Seed -> Tree -> Tree
addBranch seed t =
    case t of
        Empty -> Debug.crash "trying to add branch to empty tree"
        (BranchFamily (start, end) children) ->

            let
                distOfParent = getDistance (start,end)
                (power, newSeed1) = Random.step directionGenerator seed
                direction = -1^power
                (newPt_Scaling, newSeed2) = Random.step pointGenerator (seed)
                newPt = {x = ((toFloat direction) * newPt_Scaling.x * distOfParent) + end.x , y = (newPt_Scaling.y * distOfParent) + end.y}
                newTree = BranchFamily (end, newPt) []
                log = Debug.log "added Branch to tree (tree not yet updated)" t

            in
            newTree
            --BranchFamily (start, end) (newTree :: children)

--recursively inserts branches
insertBranch : Int -> Seed -> Tree -> (Int, Tree)
insertBranch insH seed t =
    case t of
        Empty -> Debug.crash "empty tree"
        (BranchFamily (start, end) children) ->
            if insH == 0  -- if inserting at max height of tree (increasing it's height)
                then
                    let
                        log = Debug.log "increasing tree height" insH
                        newTree = addBranch seed t
                    in
                    (0, BranchFamily (start, end) (newTree :: children) )
            else -- not at original insH level yet
                case children of
                    -- if no children at this branch, but insH > 0, insert a branch but the maxHeight of the tree is not increased
                    [] ->
                        let
                            newTree = addBranch seed t
                            log = Debug.log "still more insert height, but inserting tree at height" (insH + 1)
                        in
                        (insH, BranchFamily (start, end) (newTree :: children))

                    -- recursively insert deeper into the tree
                    _ ->
                       let
                            (randFloat, newSeed) = (Random.step floatGenerator seed)
                            treeNum = round ((toFloat ((List.length children) - 1)) * randFloat)
                            --chooseRoot picks path and calls insertBranch
                            (aboveH, newChildren) = chooseRoot treeNum (insH-1) newSeed children []
                            --debugging logs
                            logT = Debug.log "tree is" t
                            logC = Debug.log "children are" children
                            logLen = Debug.log "length children are" (List.length children)
                            log = Debug.log "choosing path" treeNum
                        in

                        ( (insH - aboveH), BranchFamily (start, end) newChildren)

-- took this out of update to clean it up, prep for move to new file/module
treeUpdate : Int -> Seed -> List Tree -> (Int, Seed, List Tree)
treeUpdate treeH seed tLst =
    case tLst of
        [] -> Debug.crash "wont happen"
        BranchFamily (start, end) children :: t->
            let
                (randFloat, newSeed1) = (Random.step floatGenerator (seed))
                insertHeight = round (toFloat (treeH) * randFloat)

                logH = Debug.log "(tree Height, insert height)" (treeH, insertHeight)

                (branchHeight, newTrees) = makeTree insertHeight newSeed1 tLst
                newTreeHeight =
                    if branchHeight == 0 then treeH + 1
                    else treeH
            in
            (newTreeHeight, newSeed1, newTrees)

        _ -> Debug.crash "TODO"

getDistance : Branch -> Float
getDistance (p1, p2) =
  let
    xdiff = (p2.x - p1.x)^2
    ydiff = (p2.y - p1.y)^2
  in
    sqrt (xdiff + ydiff)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SwitchMode newMode -> {model | mode = newMode } ! []
        SizeUpdated newSize -> {model | window = newSize} ! []  -- ! combines what's after it (multiple commands) into one command message
        Tick time ->
            let
                seed = Random.initialSeed (round time)
                (newHeight, newSeed, newTrees) = treeUpdate model.treeHeight seed model.trees
            in

            ( {model | trees = newTrees, seed = newSeed, treeHeight = newHeight, time = time}, Cmd.none )

        Click pos ->
          case model.mode of
            Spawn -> spawnTree pos model
            Destroy -> model ! [] --TODO




spawnTree : Mouse.Position -> Model -> (Model, Cmd Msg)
spawnTree pos model =
    let
      (h, w) = getWindowSize model
      offsets = ((toFloat w)/2, (toFloat h)/2)
      start = {x = (toFloat pos.x), y = (toFloat pos.y)}
      end = {x = (toFloat pos.x), y = (toFloat (pos.y))}
      (startp, endp) = translateCoords (start,end) offsets
    in
      if startp.y <= -(toFloat h)/4
      then { model | trees = (BranchFamily (startp,endp) []) :: model.trees } ! []
      else model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    batch  -- way to listen for multiple subscriptions
        [ resizes SizeUpdated
        , Time.every (0.5* second) Tick
        , clicks (\pos -> Click pos)
        ]

getWindowSize : Model -> (Int, Int)
getWindowSize m =
    (m.window.height, m.window.width)

makeBranch : Branch -> Form
makeBranch (start, end)=
   Collage.traced (Collage.solid blue) (Collage.segment (start.x, start.y) (end.x, end.y))

getBranches : List Tree -> List Form -> List Form
getBranches t_lst f_lst =
    case t_lst of
        [] -> f_lst
        (BranchFamily (s1, e1) c1) :: t1 ->
            case c1 of
                [] -> getBranches t1 ( (makeBranch (s1, e1)) :: f_lst)
                BranchFamily _ _ :: _ -> (getBranches c1 ( (makeBranch (s1, e1)) :: f_lst )) |> getBranches t1
                _ -> Debug.crash "Shouldn't get here"
        _ -> Debug.crash "Shouldn't get here"


translateCoords : (Point,Point) -> (Float,Float) -> (Point, Point)
translateCoords (start, end) (offsetX,offsetY) =
  ({x = start.x - offsetX, y = offsetY - start.y}, {x = end.x - offsetX, y = (offsetY - end.y) + 40 })

lineBound : Float -> Float -> Form
lineBound x y =
  Collage.traced (Collage.solid brown) (Collage.segment (-x, -y) (x, -y))

sunImage : (Float, Float) -> Form
sunImage pos = image 250 200 "sun.gif" |> toForm |> move pos

view : Model -> Html Msg
view model =
    let
        (h, w) = getWindowSize model
        trees = getBranches model.trees []
    in
        Html.div []
          [
            Html.button [onClick <| SwitchMode Spawn] [Html.text "Spawn"]
            , Html.button [onClick <| SwitchMode Destroy] [Html.text "Destroy"]
            , case model.mode of
                Spawn -> toHtml <| color black <| Collage.collage w h <| (sunImage ((toFloat w)/3, (toFloat h)/3)) :: (lineBound ((toFloat w)/2) ((toFloat h)/4)) :: trees
                Destroy -> toHtml <| color black <| Collage.collage w h <| (lineBound ((toFloat w)/2) ((toFloat h)/4)) :: trees

          ]
