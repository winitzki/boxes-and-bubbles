module Example exposing (main)

{-| # Overview
A basic example of using BoxesAndBubbles.
The drawing is supplied by this module (the BoxesAndBubbles library provides only the model).
The scene is updated after each animation frame.

# Running

@docs main

-}

import BoxesAndBubbles.Bodies exposing (..)
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2, Vec2, plus, lenSq, minus)
import List exposing (map, foldl)
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
 useOrbits: Bool
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

labeledBodies : Model String
labeledBodies = {
  bodies = map (\b -> { b | meta = bodyLabel b.restitution b.inverseMass }) someBodies,
  clickDown = Nothing,
  dragOrbit = Nothing,
  useOrbits = False
  }

-- why yes, it draws a body with label. Or creates the Element, rather
drawBody ({pos,velocity,inverseMass,restitution,shape,meta}) = 
  let veloLine = segment (0,0) (mul2 velocity 0.5) |> traced (solid red)
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
drawModeIndicator model = if model.useOrbits then (filled Color.darkBlue <| rect 36 12) else (filled Color.lightBrown <| rect 36 12)

drawOrbit: Vec2 -> Form
drawOrbit rv = outlined (dashed Color.darkBlue) (circle (distanceToStar (minus rv canvasCenter)))

scene : Model String -> Element
scene model =
  let
   modeIndicator = drawModeIndicator model
   indicators =
     if model.useOrbits
     then
       case model.dragOrbit of
         Nothing ->  [modeIndicator]
         Just (x,y) -> [modeIndicator, drawOrbit (toFloat x, toFloat y)]
     else
       [modeIndicator]
  in
    collage sizeX sizeY <| List.append ( map drawBody model.bodies) indicators

type Msg =
 Tick Time |
 AddBody Float Float Float Float |
 ClickDown Int Int |
 ClickDownTime Int Int Time |
 ClickUp Int Int |
 ToggleMode |
 MouseMoved Int Int

subs : Sub Msg
subs = Sub.batch [
    AnimationFrame.diffs Tick
    , downs (\p -> ClickDown p.x p.y)
    , ups (\p -> ClickUp p.x p.y)
    , moves (\p -> MouseMoved p.x p.y)
    , Keyboard.presses (\p -> ToggleMode)
    ]

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

        AddBody x y vx vy ->
          if model.useOrbits
          then
            ({ model | bodies = List.append bodies [makeNewOrbit x y], clickDown = Nothing }, Cmd.none)
          else
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

        ToggleMode ->
          let
           newMode = if model.clickDown == Nothing then not model.useOrbits else model.useOrbits
          in
          ({ model | useOrbits = newMode }, Cmd.none)

{-| Run the animation started from the initial scene defined as `labeledBodies`.
-}

main : Program Never (Model String) Msg
main = Html.program {
  init = (labeledBodies, Cmd.none)
  , update = updateAll
  , subscriptions = always subs
  , view = scene >> Element.toHtml
  }
