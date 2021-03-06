module Example exposing (main)

{-| # Overview
A basic example of using BoxesAndBubbles.
The drawing is supplied by this module (the BoxesAndBubbles library provides only the model).
The scene is updated after each animation frame.

# Running

@docs main

-}

import Html exposing (program)
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
import Time exposing (Time, now)
import Task exposing (perform)
import Mouse exposing (downs, ups, position)

inf = 1/0 -- infinity, hell yeah
e0 = 0.8 -- default restitution coefficient

-- box: (w,h) pos velocity density restitution 
-- bubble: radius density restitution pos velocity 

type alias Pos3d = {x: Int, y: Int, t: Time}

type alias Model meta = { bodies: List (Body meta), clickDown: Maybe Pos3d }

defaultLabel = ""

someBodies = [
--  bubble 2 1 e0 (70,0) (0.0,5.0) defaultLabel,
--  bubble 2 1 0.4 (40,0) (0,-6) defaultLabel,
  bubble 50 1000 0 (0, 0) (0,0) defaultLabel
  ]
-- we'll just compute the label from the data in the body
bodyLabel restitution inverseMass = 
  ["e = ", toString restitution, "\nm = ", toString (round (1/inverseMass))] |> String.concat

makeNewBody : Float -> Float -> Float -> Float -> LabeledBody
makeNewBody x y vx vy = bubble 2 1 e0 (x - sizeX / 2.0, sizeY / 2.0 - y) (vx, vy) defaultLabel

type alias Labeled = String
type alias LabeledBody = Body Labeled

labeledBodies : Model String
labeledBodies = {bodies = map (\b -> { b | meta = bodyLabel b.restitution b.inverseMass }) someBodies, clickDown = Nothing }

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

sizeX = 800
sizeY = 600

scene : Model String -> Element
scene model = collage sizeX sizeY <| map drawBody model.bodies

type Msg = Tick Time | AddBody Float Float Float Float | ClickDown Int Int | ClickDownTime Int Int Time | ClickUp Int Int

subs : Sub Msg
subs = Sub.batch [
    AnimationFrame.diffs Tick
    , downs (\p -> ClickDown p.x p.y)
    , ups (\p -> ClickUp p.x p.y)
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
        Tick dt -> ({ model | bodies = step forces (dt * timeFactor) bodies }, Cmd.none)
        AddBody x y vx vy -> ({ model | bodies = (makeNewBody x y vx vy) :: bodies }, Cmd.none)
        ClickDown x y -> (model, perform (\t -> ClickDownTime x y t) now )
        ClickDownTime x y t -> ({model | clickDown = Just {x = x, y = y, t = t}}, Cmd.none)
        ClickUp x y -> (model, case model.clickDown of
            Just pos -> perform (\t -> computeVelocity pos x y t) now
            Nothing -> Cmd.none
          )

{-| Run the animation started from the initial scene defined as `labeledBodies`.
-}

main : Program Never
main = program { 
  init = (labeledBodies, Cmd.none)
  , update = updateAll
  , subscriptions = always subs
  , view = scene >> Element.toHtml
  }
