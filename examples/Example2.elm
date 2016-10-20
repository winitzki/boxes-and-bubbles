module Example exposing (main)

{-| # Overview
A basic example of using BoxesAndBubbles.
The drawing is supplied by this module (the BoxesAndBubbles library provides only the model).
The scene is updated after each animation frame.

# Running

@docs main

-}

import Html.App exposing (program)
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
import Time exposing (Time)

inf = 1/0 -- infinity, hell yeah
e0 = 0.8 -- default restitution coefficient

-- box: (w,h) pos velocity density restitution 
-- bubble: radius density restitution pos velocity 

type alias Model meta = List (Body meta)

defaultLabel = ""

someBodies = [
--  bubble 2 50 e0 (20,40) (0,1) defaultLabel,
--  bubble 2 20 0 (0,40) (0,0) defaultLabel,
  bubble 2 1 e0 (70,0) (0.0,5.0) defaultLabel,
  bubble 2 1 0.4 (40,0) (0,-6) defaultLabel,
  bubble 5 200 0 (0, 0) (0,0) defaultLabel
  ]
-- we'll just compute the label from the data in the body
bodyLabel restitution inverseMass = 
  ["e = ", toString restitution, "\nm = ", toString (round (1/inverseMass))] |> String.concat

type alias Labeled = { label: String }
type alias LabeledBody = Body Labeled

labeledBodies : Model String
labeledBodies = map (\b -> { b | meta = bodyLabel b.restitution b.inverseMass }) someBodies

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

scene : Model String -> Element
scene bodies = collage 800 600 <| map drawBody bodies 

type Msg = Tick Time

subs : Sub Msg
subs = AnimationFrame.diffs Tick

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

update: Msg -> Model meta -> Model meta
update (Tick dt) bodies = step forces (dt * 0.01) bodies

{-| Run the animation started from the initial scene defined as `labeledBodies`.
-}

main : Program Never
main = program { 
  init = (labeledBodies, Cmd.none)
  , update = (\msg bodies -> ( update msg bodies, Cmd.none ))
  , subscriptions = always subs
  , view = scene >> Element.toHtml
  }
