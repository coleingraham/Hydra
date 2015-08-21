{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module LibHydra (
)    where

import           Data.List
import           Numeric

arg :: [Float] -> String
arg n = (intercalate "" $ map comma i) ++ (show l)
    where
        i       = init n
        l       = last n
        comma m = show m ++ ","

method :: String -> [Float] -> String
method name args = name ++ "(" ++ (arg args) ++ ") "

func :: String -> [String] -> String
func name things = "function " ++ name ++ "() " ++ (intercalate "" things) ++ "end "

draw :: [String] -> String
draw = func "draw"

line ::
    (Float, Float, Float) -> 
    (Float, Float, Float) -> String
line (x1,y1,z1) (x2,y2,z2) = method "line" [x1,y1,z1,x2,y2,z2]

triangle ::
    (Float, Float, Float) -> 
    (Float, Float, Float) -> 
    (Float, Float, Float) -> String
triangle (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) = method "triangle" [x1,y1,z1,x2,y2,z2,x3,y3,z3]

rec (w,h) = "rect(" ++ (show w) ++ "," ++ (show h) ++ ") "

rect :: Float -> Float -> String
rect width height = method "rect" [width,height]

disk :: Float -> Float -> String
disk resolution radius = method "disk" [resolution, radius]

ring :: Float -> Float -> Float -> String
ring resolution radius thickness = method "ring" [resolution,radius,thickness]

clamp :: Float -> Float
clamp n = min 1 $ max 0 $ n

colorMethod :: String -> [Float] -> String
colorMethod name args = method name $ map clamp args

background :: [Float] -> String
background []           = "background(0)"
background [gray]       = method "background" $ map clamp [gray]
background [gray,alpha] = method "background" $ map clamp [gray,alpha]
background [r,g,b]      = method "background" $ map clamp [r,g,b]
background [r,g,b,a]    = method "background" $ map clamp [r,g,b,a]
background [r,g,b,a,xs] = background $ map clamp [r,g,b,a]

color :: [Float] -> String
color []           = "color(1)"
color [gray]       = method "color" $ map clamp [gray]
color [gray,alpha] = method "color" $ map clamp [gray,alpha]
color [r,g,b]      = method "color" $ map clamp [r,g,b]
color [r,g,b,a]    = method "color" $ map clamp [r,g,b,a]
color [r,g,b,a,xs] = color $ map clamp [r,g,b,a]
