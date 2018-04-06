
module Fractals exposing (main)

import Random exposing (Generator, Seed, map2, float, int)
import Time exposing (Time, second, millisecond)
import Html exposing (Html, Attribute)
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

type Tree = Empty | BranchFamily Branch (List Tree)

type alias Model =
    {
        trees : List Tree,
        treeHeight : Int, --Eventually a List Int
        window: Window.Size,
        seed : Seed,
        time : Time,
        mousePos: Mouse.Position

    }

type Msg =
  SizeUpdated Size
  | Tick Time
  | Click Mouse.Position

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
        mousePos = {x = 0, y = 0}

    }

-- takes in the previous x and y and does point generator based on that

-- createEndPoint : Point -> Seed -> Point  --takes in previous branch's endpoint and uses as startpoint to help generate its endpoint
-- createEndPoint start seed =
--     Random.step pointGenerator seed

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

{-
getTree : Int -> Int -> Int -> Seed -> List Tree -> List Tree
getTree insH currH n seed tLst = 
    case tLst of 
        [] -> 
            let log = Debug.log "empty list, insert here" tLst in
            getTreeAndAdd 0 
        h :: t ->
            if n == 0 then
                case h of
                    Empty -> Debug.crash "branch doesn't exist!"
                    (BranchFamily (start, end) children) -> makeTree insH currH seed children
            else h :: getTree insH currH (n-1) seed t
-}


-- take in a random number for tree picking and the tree list, and return the modified tree list with added tree to chosen child
{-
getTreeAndAdd : Int -> Seed -> List Tree -> List Tree
getTreeAndAdd n seed tLst = 
    case tLst of 
        [] -> 
        h :: t ->
            if n == 0 then 
                case h of
                    Empty -> Debug.crash "Tree chosen doesn't exist"
                    (BranchFamily (start, end) children) -> 
                        let
                            distOfParent = getDistance (start,end)
                            (power, newSeed1) = Random.step directionGenerator seed
                            direction = -1^power
                            (newPt_Scaling, newSeed2) = Random.step pointGenerator (seed)
                            newPt = {x = ((toFloat direction) * newPt_Scaling.x * distOfParent) + end.x , y = (newPt_Scaling.y * distOfParent) + end.y}
                            newTree = BranchFamily (end, newPt) []
                        in
                        BranchFamily (start, end) (newTree :: children) :: t
            else h :: getTreeAndAdd (n-1) seed t 
-}

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

chooseRoot : Int -> Int -> Seed -> List Tree -> List Tree -> (Int, List Tree)
chooseRoot n insH seed tLst hLst = 
    case tLst of
        [] -> Debug.crash "exceeded tree list length"
        h :: t -> 
            if n == 0 then 
                let (newBranchHeight, changedTree) = insertBranch insH seed h in
                (newBranchHeight, (hLst ++ (changedTree :: t)) )

            else chooseRoot (n-1) insH seed t (h :: hLst)

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

insertBranch : Int -> Seed -> Tree -> (Int, Tree)
insertBranch insH seed t = 
    case t of 
        Empty -> Debug.crash "empty tree"
        (BranchFamily (start, end) children) -> 
            if insH == 0 
                then 
                    let
                        log = Debug.log "inserting to end" insH
                        newTree = addBranch seed t
                    in
                    (0, BranchFamily (start, end) (newTree :: children) )
            else -- not at branch level
                case children of
                    [] -> 
                        let 
                            newTree = addBranch seed t 
                            log = Debug.log "still more insert height, but inserting tree at height" (insH + 1)
                        in 
                        (insH, BranchFamily (start, end) (newTree :: children))

                    _ -> 
                       let 
                            (randFloat, newSeed) = (Random.step floatGenerator seed)
                            treeNum = round ((toFloat ((List.length children) - 1)) * randFloat)
                            (aboveH, newChildren) = chooseRoot treeNum (insH-1) newSeed children []
                            logT = Debug.log "tree is" t
                            logC = Debug.log "children are" children
                            logLen = Debug.log "length children are" (List.length children)
                            log = Debug.log "choosing path" treeNum
                        in 

                        ( (insH - aboveH), BranchFamily (start, end) newChildren)

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

{-}
makeTree : Int -> Int -> Seed -> List Tree -> (Int, List Tree)
makeTree insH currH seed tLst = 
    if insH < 0 then Debug.crash "can't have a negative insH"
    else 
        case tLst of
            [] -> Debug.crash "All Trees are dead or no tress on this level, should be handled by getTree"

            (BranchFamily (start, end) children) :: _ ->
                let 
                    (randFloat, newSeed) = (Random.step floatGenerator seed)
                    treeNum = round ((toFloat ((List.length tLst) - 1)) * randFloat)
                    -- treeInsert = getTree treeNum tLst
                in
                if insH == currH
                    -- add new tree to to child treeNum, return insert height
                    then ((insH + 1), getTreeAndAdd treeNum seed tLst)

                    -- recurse on child treeNum
                else 
                    let children = getTree insH currH treeNum seed tLst) in
                    case children of
                        [] -> 
            _ -> Debug.crash "shouldn't reach here"
-}
            -- randomly get tree, and insert to child

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
        SizeUpdated newSize -> {model | window = newSize} ! []  -- ! combines what's after it (multiple commands) into one command message
        Tick time ->
            let 
                seed = Random.initialSeed (round time)
                (newHeight, newSeed, newTrees) = treeUpdate model.treeHeight seed model.trees
                --log = Debug.log "(new tree height, new seed, new tree model) = " (newHeight, newSeed, newTrees)
                --log = Debug.log "(new tree height, new seed) = " (newHeight, newSeed)
            in

            ( {model | trees = newTrees{-BranchFamily (start, end) (newTree :: children) :: t-}, seed = newSeed, treeHeight = newHeight, time = time}, Cmd.none )
                -- branching off of height = 0

        Click pos ->
          {model | mousePos = pos} ! []



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
        mouse = model.mousePos
    in
      Html.div []
          [
              toHtml <| Collage.collage h w trees
          ]
