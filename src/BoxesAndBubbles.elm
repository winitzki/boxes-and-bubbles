module BoxesAndBubbles exposing (bubble,box,bounds,step)
{-| The interface for the Boxes and Bubbles physics engine.

# Concepts

## Simulation

Boxes and Bubbles implements a very simple physics simulation. It updates a list of bodies
at each step. There is no time-normalized integration - if you run it with higher fps, 
it will run faster.

See the [example code](https://github.com/jastice/boxes-and-bubbles/blob/master/Example.elm) 
and the [example animation](http://jastice.github.io/boxes-and-bubbles/) that it produces
for a working usage example.

## Bodies

Everything in Boxes and Bubbles is a Body. A Body is a Box, or a Bubble.

Bodies have some properties:

* `position` -- reference point and center of body
* `velocity` -- direction and speed of movement
* `mass` -- the mass (stored as inverseMass)
* `restitution` -- bounciness factor: how much force is preserved on collisions
* `shape` -- radius for Bubble, extents for Box, wrapped in an ADT.

Bodies can have infinite mass. Infinite mass bodies are not affected by any forces.

## Forces

Two types of global forces: gravity and ambient. Both are vectors,
so that they could point in any direction. Both can vary over time.
Ambient force takes the mass of objects into account, while gravity does not.

# Functions

## Constructors and helpers

@docs bubble, box, bounds

## Running the simulation

@docs step

-}

import BoxesAndBubbles.Engine exposing (..)
import BoxesAndBubbles.Bodies exposing (..)
import BoxesAndBubbles.Math2D exposing (Vec2)
import List

-- constructors

{-| Create a bubble. Mass is derived from density and size.

    bubble radius density restitution position velocity meta

Create a bubble with radius 100 with density 1 and restitution 1
at origin and a string "tag", moving toward the upper right:

    bubble 100 1 1 (0,0) (3,3) "tag"
-}
bubble: Float -> Float -> Float -> Vec2 -> Vec2 -> meta -> Body meta
bubble radius density restitution pos velocity meta = { 
  pos = pos,
  velocity = velocity, 
  inverseMass = 1/(pi*radius*radius*density), 
  restitution = restitution,
  shape = Bubble radius,
  meta = meta
  }

{-| Create a box. Mass is derived from density and size.
    
    box (width,height) position velocity density restitution

Create a box with width 100, height 20, density 1 and restitution 1
at origin, moving toward the upper right:

    box (100,20) 1 1 (0,0) (3,3)
-}
box: Vec2 -> Float -> Float -> Vec2 -> Vec2 -> meta -> Body meta
box (w,h) density restitution pos velocity meta = {
  pos = pos,
  velocity = velocity,
  inverseMass = 1/(w*h*density),
  restitution = restitution,
  shape = Box (w/2,h/2),
  meta = meta
  }

{-| Create a bounding box made up of boxes with infinite mass.

    bounds (width,height) thickness restitution center meta

Create bounds with width and height 800, 50 thick walls and 0.6 restitution and a String tag
at the origin:

    bounds (800,800) 50 0.6 (0,0) "tag"

-}
bounds: Vec2 -> Float -> Float -> Vec2 -> meta -> List (Body meta)
bounds (w,h) thickness restitution (cx,cy) meta = 
  let (wExt,hExt) = (w/2,h/2)
      halfThick = thickness/2
      inf = 1/0
  in [
    box (w,thickness) inf restitution (cx, hExt+halfThick) (0,0) meta,
    box (w,thickness) inf restitution (cx, -(hExt+halfThick)) (0,0) meta,
    box (thickness,h) inf restitution (wExt+halfThick, cy) (0,0) meta,
    box (thickness,h) inf restitution (-(hExt+halfThick), cy) (0,0) meta
  ]

{-| Perform a step in the physics simulation. Applies forces to objects and updates them based
on their velocity and collisions. Order of bodies in input list is not preserved in the output.

Apply computed forces to bodies:

    step forces dt bodies
-}

step: (List(Body meta) -> Body meta -> Vec2) -> Float -> List (Body meta) -> List (Body meta)
step forces dt bodies = 
  List.map (\body -> update (forces bodies body) dt body) (collide [] bodies)
