
module Main where

import Graphics.Gloss.Interface.IO.Animate
import System.IO.Unsafe(unsafePerformIO)
import Data.IORef

main :: IO ()
main    =  animateIO (InWindow "machina" (800, 600) (10, 10))
                  black frame' (const $ return())

v :: IORef (Int,Float)
v = unsafePerformIO (newIORef (0,0))

frame' :: Float -> IO Picture
frame' t = do
        (n,t') <- readIORef v
        if t>(t'+1) then print (n+1,t-t') >> writeIORef v (0,t'+1) else writeIORef v (n+1,t')
        return (frame t)

frame :: Float -> Picture
frame time
        -- = Color (makeColor 1.0 1.0 1.0 0.5) $Translate time (-100)$ Polygon circ'
        = Color (makeColor 1.0 1.0 1.0 0.5) $ mach time 12

csz = 20000
--csz = 100
circ :: [(Float,Float)]
circ = [(100*(sin (i*pi/(csz))), 100*(cos (i*pi/(csz))))  | i <- [0..2*csz]]
csz' = 100
circ' :: [(Float,Float)]
circ' = [(100*(sin (i*pi+i*pi/(csz'))),100*(cos (i*pi+i*pi/(csz'))))  | i <- [0..2*csz']]


mach :: Float -> Int -> Picture
mach _ 0 = leaf
mach t d
 = Pictures
        [ leaf
        , {-Translate 0 (-100)
                $ -}Scale 1.0 1.0
                -- $ Rotate (90 + t * 10)
                $ mach (t * 1.1) (d - 1)

        , Translate 0   100
                -- $ Scale 0.8 0.8
                {-$ Rotate (90 - t * 30)-}
                $ mach (t * 1.1) (d - 1) ]

leaf :: Picture
leaf    = Pictures
                [ Polygon loop
                --, Color (makeColor 0.0 0.0 1.0 0.8) $ Line loop ]
                ]

loop :: [(Float,Float)]
loop    = [(-10, -100), (-10, 100), (10, 100), (10, -100), (-10, -100)]

