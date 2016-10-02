module Shapes
( Point, Shape, area, nudge, baseCircle, baseRect) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

shift :: Point -> (Float, Float) -> Point
shift (Point x y) (dx, dy) = Point (x+dx) (y+dy)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle p r) a b = Circle (shift p (a, b)) r
nudge (Rectangle p1 p2) a b = Rectangle (shift p1 (a, b)) (shift p2 (a, b))

origin :: Point
origin = Point 0 0

baseCircle :: Float -> Shape
baseCircle r = Circle (origin) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (origin) (Point width height)
