module Example exposing (main)

{-| # Overview
A simulation of planetary motion.
The drawing is supplied by this module (the BoxesAndBubbles library provides only the model).
The scene is updated after each animation frame.

# Running

@docs main

-}

import BoxesAndBubbles.Bodies exposing (..)
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2, Vec2, plus, lenSq, minus)
import List exposing (map, foldl)
import Char
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Text exposing (fromString)
import AnimationFrame
import String
import Html
import Keyboard
import Time exposing (Time, now)
import Task exposing (perform)
import Mouse exposing (downs, ups, position, moves)

inf = 1/0 -- infinity, hell yeah
e0 = 0.8 -- default restitution coefficient

-- Size of the canvas
sizeX = 1024
sizeY = 800
canvasCenter : Vec2
canvasCenter = (sizeX / 2.0, sizeY / 2.0)

-- box: (w,h) pos velocity density restitution 
-- bubble: radius density restitution pos velocity 

type alias Pos3d = {x: Int, y: Int, t: Time}

type alias Model meta = {
 bodies: List (Body meta),
 clickDown: Maybe Pos3d,
 dragOrbit: Maybe (Int, Int),
 running: Bool,
 editMode: EditMode
 }

defaultLabel = ""

-- Star needs to remain at (0,0) with velocity (0,0)
star = bubble 50 2000 0 (0, 0) (0,0) defaultLabel

someBodies = [ star ]
-- we'll just compute the label from the data in the body
bodyLabel restitution inverseMass = 
  ["e = ", toString restitution, "\nm = ", toString (round (1/inverseMass))] |> String.concat

makeNewBody : Float -> Float -> Float -> Float -> LabeledBody
makeNewBody x y vx vy = bubble 2 1 e0 (x - sizeX / 2.0, sizeY / 2.0 - y) (vx, vy) defaultLabel

distanceToStar: Vec2 -> Float
distanceToStar r = sqrt(lenSq (minus r star.pos))

makeNewOrbit : Float -> Float -> LabeledBody
makeNewOrbit xRaw yRaw =
  let
    x = xRaw - sizeX / 2.0
    y = yRaw - sizeY / 2.0
    r = distanceToStar (x, y)
    vTotal = sqrt(k/star.inverseMass/r)
    vx = -vTotal*y/r
    vy = vTotal*x/r
  in
    bubble 2 1 e0 (x, y) (vx, vy) defaultLabel

type alias Labeled = String
type alias LabeledBody = Body Labeled

initialModelValue : Model String
initialModelValue = {
  bodies = map (\b -> { b | meta = bodyLabel b.restitution b.inverseMass }) someBodies,
  clickDown = Nothing,
  dragOrbit = Nothing,
  running = True,
  editMode = Fling
  }

-- why yes, it draws a body with label. Or creates the Element, rather
drawBody ({pos,velocity,inverseMass,restitution,shape,meta}) = 
  let veloLine = segment (0,0) (mul2 velocity 0.5) |> traced (solid lightGreen)
      info = meta |> fromString |> centered |> toForm 

      ready = case shape of
        Bubble radius ->
          group [ 
            circle radius |> outlined (solid black),
--            info |> move (0,radius+16),
            veloLine
            ]
        Box extents -> 
          let (w,h) = extents
          in group [
            rect (w*2) (h*2) |> outlined (solid black),
            info |> move (0,h+16),
            veloLine            
          ] 
  in move pos ready  

drawModeIndicator: Model String -> Form
drawModeIndicator model = let
  color = case model.editMode of
    Fling -> Color.lightBrown
    CircleOrbit -> Color.darkBlue
    Delete -> Color.lightRed
 in
  (filled color <| rect 36 12)

drawOrbit: Vec2 -> Form
drawOrbit rv = outlined (dashed Color.darkBlue) (circle (distanceToStar (minus rv canvasCenter)))

scene : Model String -> Element
scene model =
  let
   modeIndicator = drawModeIndicator model
   orbitLine =
     if model.editMode == CircleOrbit
     then
      case model.dragOrbit of
        Nothing ->  []
        Just (x,y) -> [drawOrbit (toFloat x, toFloat y)]
     else []
  in
    collage sizeX sizeY <| List.append ( map drawBody model.bodies) <| List.append [modeIndicator] orbitLine

type EditMode = Fling | CircleOrbit | Delete

type Msg =
  Tick Time |
  AddBody Float Float Float Float |
  ClickDown Int Int |
  ClickDownTime Int Int Time |
  ClickUp Int Int |
  SetMode EditMode |
  ToggleRunning |
  MouseMoved Int Int

subs : Model String -> Sub Msg
subs model = let
  ticks = if model.running then [AnimationFrame.diffs Tick] else []
 in
  Sub.batch <| List.append [
    downs (\p -> ClickDown p.x p.y)
    , ups (\p -> ClickUp p.x p.y)
    , moves (\p -> MouseMoved p.x p.y)
    , Keyboard.presses (\keycode ->
        case Char.fromCode keycode of
          'd' -> SetMode Delete
          'o' -> SetMode CircleOrbit
          's' -> ToggleRunning
          _ -> SetMode Fling
        )
    ] ticks

k : Float
k = 0.1

pairForce : Body meta -> Body meta -> Vec2
pairForce b1 b2 =
  let m1 = mass b1
      m2 = mass b2
      r12 = minus b2.pos b1.pos
      dSq = lenSq r12
  in if dSq == 0 then (0,0) else mul2 r12 (k*m1*m2 / dSq / (sqrt dSq))

-- gravitational attraction between bodies
forces: List(Body meta) -> Body meta -> Vec2
forces bodies body = foldl (\b vec -> plus vec (pairForce body b)) (0,0) bodies


computeVelocity: Pos3d -> Int -> Int -> Time -> Msg
computeVelocity pos3d x1 y1 t1 =
    let
        xF = (toFloat x1)
        yF = (toFloat y1)
        dt = (t1 - pos3d.t) * timeFactor
        vx = (xF - toFloat pos3d.x) / dt
        vy = -1 * (yF - toFloat pos3d.y) / dt
    in
        AddBody xF yF vx vy

timeFactor = 0.01

updateAll: Msg -> Model String -> (Model String, Cmd Msg)
updateAll msg model =
    let
        bodies = model.bodies
    in
    case msg of
        Tick dt ->
         let
          newBodiesRaw = step forces (dt * timeFactor) bodies
          newBodies = star :: List.filter (\b -> b.inverseMass /= star.inverseMass) newBodiesRaw
         in
          ({ model | bodies = newBodies }, Cmd.none)

        AddBody x y vx vy -> case model.editMode of
          CircleOrbit ->
            ({ model | bodies = List.append bodies [makeNewOrbit x y], clickDown = Nothing }, Cmd.none)
          _ ->
            ({ model | bodies = List.append bodies [makeNewBody x y vx vy], clickDown = Nothing }, Cmd.none)

        ClickDown x y -> ({model | dragOrbit = Just (x, y)}, perform (\t -> ClickDownTime x y t) now )
        ClickDownTime x y t -> ({model | clickDown = Just {x = x, y = y, t = t}}, Cmd.none)
        ClickUp x y -> ({model | dragOrbit = Nothing}, case model.clickDown of
            Just pos -> perform (\t -> computeVelocity pos x y t) now
            Nothing -> Cmd.none
          )
        MouseMoved x y ->
          if model.dragOrbit == Nothing
          then
            (model, Cmd.none)
          else
            ({model | dragOrbit = Just (x, y)}, Cmd.none)

        ToggleRunning -> ({ model | running = not model.running }, Cmd.none)

        SetMode mode ->
          let
           newMode = if model.clickDown == Nothing then mode else model.editMode
          in
          ({ model | editMode = newMode }, Cmd.none)

{-| Run the animation started from the initial scene defined as `labeledBodies`.
-}

main : Program Never (Model String) Msg
main = Html.program {
  init = (initialModelValue, Cmd.none)
  , update = updateAll
  , subscriptions = subs
  , view = scene >> Element.toHtml
  }
