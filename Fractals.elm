
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
--type Root = TreeRoot Height Branch (List Tree)
type Tree = BranchFamily Height Branch (List Tree)

type alias Model =
    {
        trees : List Tree ,
        growFactor : Float,
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
  | Clear

init : (Model, Cmd Msg)
init = (initialModel, initialSize)

initialSize : Cmd Msg
initialSize =
    Window.size |> perform SizeUpdated

-- this is for testing, not being used currently
{-
initialChildren : List Tree
initialChildren =
  [
    (BranchFamily ({x = 100, y = 140}, {x = 120, y = 160}) [(BranchFamily ({x = 120, y = 160}, {x = 110, y = 200})  [])])
    , (BranchFamily ({x = 100, y = 120}, {x = 180, y = 140})  [(BranchFamily ({x = 180, y = 140}, {x = 180, y = 200})  [])])
  ]
-}

initialModel : Model
initialModel =
    {
        trees = [],
        growFactor = 0.01,
            {-
            [
                BranchFamily 0 ({x = 100, y = 100}, {x = 100, y = 200}) []
                --BranchFamily ({x = 100, y = 100}, {x = 100, y = 200})  []
                -- This will be it when converted to radians BranchFamily ({x=100, y=100}, ((degree 90), 100))
            ],
            -}
        --treeHeight = 0,
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

startFractionGenerator : Generator Float
startFractionGenerator =
    float 0.5 0.9

pointGenerator :  Generator Point  -- Float -> ((Float, Float), Int)
pointGenerator =
  map2 Point (float 0.3 0.6) (float 0.3 0.6)


getDistance : Branch -> Float
getDistance (p1, p2) =
  let
    xdiff = (p2.x - p1.x)^2
    ydiff = (p2.y - p1.y)^2
  in
    sqrt (xdiff + ydiff)

-- took this out of update to clean it up, prep for move to new file/module


getTreeNumber : Seed -> List Tree -> (Seed, Int)
getTreeNumber seed tLst =
    let
        (randFloat, newSeed) = (Random.step floatGenerator (seed))
        --log = Debug.log "list has length" (List.length tLst)
        --logF = Debug.log "rand float is" randFloat
        listLen = toFloat ( List.length tLst) - 1
        treeNum = round ( randFloat * listLen )
    in
    (newSeed, treeNum)

getTallestTree : Int -> List Tree -> Int
getTallestTree ht tLst =
    case tLst of
        [] -> ht
        (BranchFamily ht1 _ _)  :: t -> getTallestTree (max ht ht1) t


--gets a random root from the list and chooses which tree to branch from, as well as randomly choosing the insert height based on the height of that tree
chooseRoot : Int -> Float -> Seed -> List Tree -> List Tree
chooseRoot n growFactor seed tLst =
    case tLst of
        [] -> [] -- nothing to add a branch to

        (BranchFamily ht br lst1) :: t ->
            if n == 0 then
                let
                    (randFloat, newSeed) = (Random.step floatGenerator (seed))
                    insH = round (toFloat (ht) * randFloat)
                    updatedRoot = insertBranch insH newSeed (BranchFamily ht br lst1)
                    grownRoot = growTree growFactor updatedRoot
                    --log = Debug.log "updated root is" updatedRoot
                in
                grownRoot :: t
            else BranchFamily ht br lst1 :: chooseRoot (n-1) growFactor seed t

growTree : Float -> Tree -> Tree
growTree growFactor (BranchFamily ht br children) = 
    case children of
        [] -> growBranch growFactor (BranchFamily ht br [])--BranchFamily ht (growBranch growFactor (s, e)) []
        h :: t -> 
            let newChildren = List.map (\child -> growTree growFactor child) children in
            growBranch growFactor (BranchFamily ht br newChildren)
            --BranchFamily ht (growBranch growFactor (s, e)) newChildren
    

limitGrowth : Float -> (Float, Float) -> (Float, Float)
limitGrowth mag (dx, dy) = 
    if mag > 120 then
        let scaleFactor = 120/mag  in
        (dx * scaleFactor, dy * scaleFactor)
    else (dx, dy)
        
-- grows the branch by some epsilon and based on the height level of the tree
--growBranch : Float -> Branch -> Branch
--growBranch epsilon (start, end) =
growBranch : Float -> Tree -> Tree
growBranch epsilon (BranchFamily ht (start, end) children) =
    let
        growFactor = 1 + epsilon
        mag = getDistance (start, end)
        log = Debug.log "magnitude" mag
        (dx, dy) = {-limitGrowth mag-} (end.x - start.x, end.y - start.y)
        end_ = { x = start.x + (growFactor * dx), y = start.y + (growFactor * dy) }
    in
    BranchFamily ht (start, end_) children
    
    {- 
    if ht < 3 then 
        let
            growFactor = 1 + epsilon
            mag = getDistance (start, end)
            (dx, dy) = (end.x - start.x, end.y - start.y)
            end_ = { x = start.x + (growFactor * dx), y = start.y + (growFactor * dy) }
        in
        BranchFamily ht (start, end_) children
    else if ht < 7 then
        let
            growFactor = 1 + (epsilon/5)
            mag = getDistance (start, end)
            (dx, dy) = (end.x - start.x, end.y - start.y)
            end_ = { x = start.x + (growFactor * dx), y = start.y + (growFactor * dy) }
        in
        BranchFamily ht (start, end_) children
    else
        let
            growFactor = 1 + (epsilon/5)
            mag = getDistance (start, end)
            (dx, dy) = (end.x - start.x, end.y - start.y)
            end_ = { x = start.x + (growFactor * dx), y = start.y + (growFactor * dy) }
        in
        BranchFamily ht (start, end_) children
    -}
    




--- ***** Issue somewhere with tree growing exclusively to the left, fix this!!! ******
chooseTreeBranch : Int -> Int -> Seed -> List Tree -> List Tree ---> List Tree
chooseTreeBranch n insH seed tLst =
    case tLst of
        [] -> Debug.crash "This shouldn't happen" -- makeBranch and put into child list"
        h :: t ->
            let
                log1 = Debug.log "current treeNum = " n
            in
            if n == 0 then
                let
                    log = Debug.log "chose child tree =" h
                    newLst = (insertBranch insH seed h) :: t
                in
                --hLst ++ newLst
                newLst
            else
                h :: chooseTreeBranch (n-1) insH seed t --(h :: hLst)

insertBranch : Int -> Seed -> Tree -> Tree
insertBranch insH seed tree =
    
    if insH == 0 then addBranchPair seed tree -- new version of tree
    else case tree of
        (BranchFamily ht br tLst) -> case tLst of
            [] -> addBranchPair seed (BranchFamily 1 br tLst) -- empty parent list -> makeBranch here and increment height from 0 to 1
            _ :: _ ->
                let
                    (newSeed, treeNum) = getTreeNumber seed tLst
                    logLst = Debug.log "tLst is w/Length" (List.length tLst, tLst)
                    logNum = Debug.log "treeNum for tLst is" treeNum
                    newLst = chooseTreeBranch treeNum (insH-1) newSeed tLst --[]
                    maxHt = (getTallestTree 0 newLst) + 1
                    --log = Debug.log "maxht for tree is" (tree, maxHt)
                in

                BranchFamily maxHt br newLst

--- gives back angle of parent direction with horizontal
{-}
parentDir : (Point, Point) -> Int
parentDir (start, end) =
    let
        --(dx, dy) = (end.x - start.x, end.y - start.y)
        --scalar = dx + dy
        --(dx_, dy_) = (dx / scalar, dy / scalar)
    in
-}

-- takes in the float, parentAngle (in radians), magnitude, and start point and produces both endpoints
getEndpts : Float -> Float -> Float -> (Point) -> (Point, Point)
getEndpts flt parentAngle mag start =
    let
        splitAngle = ((-200 * flt) + 245) * pi/180 -- decided want angle between 145 and 65 degrees for flts 0.5 to 0.9, respectively (in radians)
        (lAngle, rAngle) = (parentAngle + splitAngle/2, parentAngle - splitAngle/2)
        (ldx_scalar, ldy_scalar, rdx_scalar, rdy_scalar) = (cos lAngle, (sin lAngle), cos rAngle, (sin rAngle))
        (lEndpt, rEndpt) = ({x = start.x + ldx_scalar * mag, y = start.y + ldy_scalar * mag}, {x = start.x + rdx_scalar * mag, y = start.y + rdy_scalar * mag})
    in
    (lEndpt, rEndpt)


--creates branch to be inserted once correct position is identified
addBranchPair : Seed -> Tree -> Tree
addBranchPair seed t =
    case t of
        (BranchFamily treeH (start, end) children) ->

            let
                distOfParent = getDistance (start,end)

                -- need angle of branch pair, and distance up the branch
                --get float
                (flt, newSeed) = Random.step startFractionGenerator seed
                --get dx and dy of parent, and then use it to make start point (scale by float) of next branch pair
                (dx, dy) = (end.x - start.x, end.y - start.y)

                newStart = {x = start.x + (flt * dx), y = start.y + (flt * dy) }
                -- get magnitude (length) of next branch pair (scale by flt, so that larger branches farther down and smaller branches near top or end of parent)
                magnitude = (1.0 - flt) * distOfParent
                parentAngle = atan2 dy dx -- get parent angle in radians

                (lEndpt, rEndpt) = getEndpts flt parentAngle magnitude newStart

                --(power, newSeed1) = Random.step directionGenerator seed
                --direction = -1^power
                (newPt_Scaling, newSeed2) = Random.step pointGenerator (seed)
                --newPt = {x = ((toFloat direction) * newPt_Scaling.x * distOfParent) + end.x , y = (newPt_Scaling.y * distOfParent) + end.y}
                --leftPt = {x = (-1 * newPt_Scaling.x * distOfParent) + newStart.x , y = (newPt_Scaling.y * distOfParent) + newStart.y}
                leftTree = BranchFamily 0 (newStart, lEndpt) []
                --leftTree = BranchFamily 0 (newStart, leftPt) []
                --rightPt = {x = (newPt_Scaling.x * distOfParent) + newStart.x , y = (newPt_Scaling.y * distOfParent) + newStart.y}
                rightTree = BranchFamily 0 (newStart, rEndpt) []
                --rightTree = BranchFamily 0 (newStart, rightPt) []
                --log = Debug.log "added Branch to tree (tree not yet updated)" t

            in
            if treeH > 0 then BranchFamily treeH (start, end) (leftTree :: rightTree :: children) -- if tLst is not empty, just append newTree
            else BranchFamily 1 (start, end) (leftTree :: rightTree :: []) -- if treeH == 0, then child tree list must be empty


--- update for time-based tree growth/adding of branches
treeUpdate : Float -> Seed -> List Tree -> (Seed, List Tree)
treeUpdate growFactor seed tLst =
    case tLst of
        [] -> (seed, [])
        BranchFamily treeH (start, end) children :: t->
            let
                (newSeed, rootNum) = getTreeNumber seed tLst
                newTrees = chooseRoot rootNum growFactor newSeed tLst
            in
                {-
                newTreeHeight =
                    if branchHeightDiff == 0 then treeH + 1
                    else treeH
                -}
            (newSeed, newTrees)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SizeUpdated newSize -> {model | window = newSize} ! []  -- ! combines what's after it (multiple commands) into one command message
        SwitchMode newMode -> {model | mode = newMode } ! []
        Clear -> {model | trees = []} ! []
        Tick time ->
            if model.mode == Spawn then
                let
                    seed = Random.initialSeed (round time)
                    -- old(newHeight, newSeed, newTrees) = treeUpdate model.treeHeight seed model.trees
                    (newSeed, newTrees) = treeUpdate model.growFactor seed model.trees
                in

                ( {model | trees = newTrees, seed = newSeed, {-treeHeight = newHeight,-} time = time}, Cmd.none )
            else --in Destroy Mode
                model ! [] 

        Click pos ->
            case model.mode of
                Spawn -> --spawnTree pos model
                    let
                        (h, w) = getWindowSize model
                        offsets = ((toFloat w)/2, (toFloat h)/2)
                        start = {x = (toFloat pos.x), y = (toFloat pos.y)}
                        end = {x = (toFloat pos.x), y = (toFloat (pos.y))}
                        (startp, endp) = translateCoords (start,end) offsets
                    in
                    if startp.y <= -(toFloat h)/4
                        then { model | trees = (BranchFamily 0 (startp,endp) []) :: model.trees } ! []
                    else model ! []
                        --{ model | trees = (BranchFamily 0 (startp,endp) []) :: model.trees } ! []

                Destroy -> model ! [] --TODO

subscriptions : Model -> Sub Msg
subscriptions model =
    batch  -- way to listen for multiple subscriptions
        [ resizes SizeUpdated
        , Time.every (0.1* second) Tick
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
        (BranchFamily _ (s1, e1) c1) :: t1 ->
            case c1 of
                [] -> getBranches t1 ( (makeBranch (s1, e1)) :: f_lst)
                BranchFamily _ _ _ :: _ -> (getBranches c1 ( (makeBranch (s1, e1)) :: f_lst )) |> getBranches t1


-- allowedToGrow : Int -> Int -> Bool
-- allowedToGrow yBound yMouse =
--   round (toFloat (yBound) * 0.25) >= yMouse

translateCoords : (Point,Point) -> (Float,Float) -> (Point, Point)
translateCoords (start, end) (offsetX,offsetY) =
  ({x = start.x - offsetX, y = offsetY - start.y}, {x = end.x - offsetX, y = (offsetY - end.y) + 40 })

lineBound : Float -> Float -> Form
lineBound x y =
  Collage.traced (Collage.solid brown) (Collage.segment (-x, -y) (x, -y))

sunImage : (Float, Float) -> Form
sunImage pos = image 250 200 "sun.gif" |> toForm |> move pos

cloudImage : (Float, Float) -> Form
cloudImage pos = image 250 200 "clouds.gif" |> toForm |> move pos

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
            , Html.button [onClick <| Clear] [Html.text "Clear All Trees"]
            , case model.mode of
                Spawn -> toHtml <| color black <| Collage.collage w h <| (sunImage ((toFloat w)/3, (toFloat h)/3)) :: (lineBound ((toFloat w)/2) ((toFloat h)/4)) :: trees
                Destroy -> toHtml <| color black <| Collage.collage w h <| (cloudImage ((toFloat w)/3, (toFloat h)/3)) :: (lineBound ((toFloat w)/2) ((toFloat h)/4)) :: trees

          ]

{-
view : Model -> Html Msg
view model =
    let
        (h, w) = getWindowSize model
        trees = getBranches model.trees []
    in
        toHtml <| color black <| Collage.collage w h trees
-}


------------------------------------------------------------------

{-
makeTreeOld : Int -> Seed -> List Tree -> (Int, List Tree)
makeTreeOld insH seed tLst =
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



treeUpdateOld : Int -> Seed -> List Tree -> (Int, Seed, List Tree)
treeUpdateOld treeH seed tLst =
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


insertBranchOld : Int -> Seed -> Tree -> (Int, Tree)
insertBranchOld insH seed t =
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


chooseRootOld : Int -> Int -> Seed -> List Tree -> List Tree -> (Int, List Tree)
chooseRootOld n insH seed tLst hLst =
    case tLst of
        [] -> Debug.crash "exceeded tree list length"
        h :: t ->
            if n == 0 then
                let (newBranchHeight, changedTree) = insertBranch insH seed h in
                (newBranchHeight, (hLst ++ (changedTree :: t)) )

            else chooseRoot (n-1) insH seed t (h :: hLst)

-}


------------------------------------------------------------------

{-
makeTree : Seed -> List Tree -> List Tree--(Seed, List Root)
makeTree seed tLst =
    --if insH < 0 then Debug.crash "can't have a negative insH"

    case tLst of
        [] ->
            let log = Debug.log "No Trees exist to insert into" tLst in
            (0, tLst)
        _ :: _ ->
>>>>>>> 889d20c257a8d6c94439e63b3b93b81726130d15
            let
                (randFloat, newSeed) = (Random.step floatGenerator seed)
                treeNum = round ((toFloat ((List.length tLst) - 1)) * randFloat)
                -- picks a random element in the root list to insert into on a tick

                log = Debug.log "root path chosen is" treeNum

            in
            chooseRoot treeNum newSeed tLst []

        _ -> Debug.crash "shouldn't reach here"




--chooseRoot : Int -> Seed -> List Tree -> List Tree -> List Tree
chooseRoot : Int -> Int -> Seed -> List Tree -> List Tree -> List Tree
chooseRoot n seed tLst hLst = --location in list of root, seed, tail of list (not explored), head of list ()
    {-
    let
        BranchFamily _ _ children = t
        pickRoot : Int -> Seed -> List Tree -> List Tree
        pickRoot = n seed tLst hLst
    in
    -}
    case tLst of
        [] -> Debug.crash "exceeded tree list length"
        treeHead :: t ->
            if n == 0 then
                let
                    (BranchFamily treeH _ _ ) = treeHead
                    (randFloat, newSeed1) = (Random.step floatGenerator (seed))
                    insertHeight = round (toFloat (treeH) * randFloat)
                    (newRootHeight, newRoot) = insertBranch insertHeight seed treeHead
                in

                hLst ++ ( newRoot :: t)

----- spawntree commented out
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


--creates branch to be inserted once correct position is identified
addBranch : Seed -> Tree -> Tree
addBranch seed t =
    case t of
        Empty -> Debug.crash "trying to add branch to empty tree"
        (BranchFamily treeH (start, end) children) ->

            let
                distOfParent = getDistance (start,end)
                (power, newSeed1) = Random.step directionGenerator seed
                direction = -1^power
                (newPt_Scaling, newSeed2) = Random.step pointGenerator (seed)
                newPt = {x = ((toFloat direction) * newPt_Scaling.x * distOfParent) + end.x , y = (newPt_Scaling.y * distOfParent) + end.y}
                newTree = BranchFamily 0 (end, newPt) []
                log = Debug.log "added Branch to tree (tree not yet updated)" t

            in
            newTree
            --BranchFamily (start, end) (newTree :: children)

--recursively inserts branches
insertBranch : Int -> Seed -> Tree -> (Int, Tree)
insertBranch insH seed t =
    case t of
        Empty -> Debug.crash "empty tree"
        (BranchFamily treeH (start, end) children) ->
            if insH == 0  -- if inserting at max height of tree (increasing it's height)
                then
                    let
                        log = Debug.log "increasing tree height" insH
                        newTree = addBranch seed t
                        newChildren = newTree :: children
                    in

                    if treeH == 0 then
                        (1, (BranchFamily 1 (start, end) newChildren ))
                    else
                        (treeH, BranchFamily treeH (start, end) newChildren )

            else -- not at original insH level yet
                case children of
                    -- if no children at this branch, but insH > 0, insert a branch and increase the maxHeight of that tree
                    [] ->
                        let
                            newTree = addBranch seed t
                            log = Debug.log "still more insert height, but inserting tree at height" (insH + 1)
                        in
                        (1, BranchFamily 1 (start, end) (newTree :: children)) --[newTree]

translateCoords : (Point,Point) -> (Float,Float) -> (Point, Point)
translateCoords (start, end) (offsetX,offsetY) =
  ({x = start.x - offsetX, y = offsetY - start.y}, {x = end.x - offsetX, y = (offsetY - end.y) + 40 })



-}
