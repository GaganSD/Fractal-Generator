-- Made by: Gagan Devagiri
-- For: Introduction To Computation: Tutorial 6
-- Date: 2nd November 2018


module LSystems where
import LSystem
import Data.Char


tree :: Int -> Command
tree x  =  f x
  where
  f 0      = GrabPen purple :#: Go 10
  f x  = g (x-1) :#: Branch (n :#: f (x-1))
                 :#: Branch (p :#: f (x-1))
                 :#: Branch (g (x-1) :#: f (x-1))
  g 0      = GrabPen white :#: Go 10
  g x  = g (x-1) :#: g (x-1)
  n        = Turn 45
  p        = Turn (-45)


-- -- snowflake:
--       angle: 60
--       start: f--f--f-
--       rewrite: f → f+f--f+f

snowflake :: Int -> Command
snowflake x = f(x) :#: angle :#: angle :#: f(x) :#: angle :#: angle :#: f(x) :#: angle :#: angle
  where
    f 0 = GrabPen white :#: Go 10
    f x = f(x-1) :#: angle' :#: f(x-1) :#: angle :#: angle :#: f(x-1) :#: angle' :#: f(x-1)
    angle = Turn (-60)
    angle' = Turn (60)

-- -- Peano-Gosper:
--        angle: 60
--        start: f
--        rewrite: f → f+g++g-f--ff-g+ g → -f+gg++g+f--f-g

peanoGosper x = f x
    where
      f 0 = GrabPen purple :#: Go 10
      f x = f (x-1) :#: n :#: g (x-1) :#: n
          :#: n :#: g (x-1) :#: p :#: f (x-1)
          :#: p :#: p :#: f (x-1) :#: f (x-1)
          :#: p :#: g (x-1) :#: n

      g 0 = GrabPen white :#: Go 10

      g x = p :#: f (x-1)
            :#: n :#: g (x-1)
            :#: g (x-1) :#: n
            :#: n :#: g (x-1) :#: n
            :#: f (x-1) :#: p :#: p
            :#: f (x-1) :#: p :#: g (x-1)

      n = Turn 60
      p = Turn(-60)

--------------
-- Cross:
    -- angle: 90
    -- start: f-f-f-f
    -- rewrite: f → f-f+f+ff-f-f+f

cross x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x
    where
      f 0 =  GrabPen white :#: Go 10

      f x = f (x-1) :#: n :#: f (x-1) :#: p
            :#: f (x-1) :#: p :#: f (x-1)
            :#: f (x-1) :#: n :#: f (x-1)
            :#: n :#: f (x-1) :#: p :#: f (x-1)

      n = Turn 90
      p = Turn(-90)

--------
-- Branch:
    -- angle: 22.5
    -- start: g
    -- rewrite: g → f-[[g]+g]+f[+fg]-g f → ff

branch x = g x
   where
     g 0 = GrabPen purple :#: Go 10
     g x = f (x-1) :#: p :#: Branch (Branch (g (x-1))
            :#: n :#: g (x-1)) :#: f (x-1)
            :#: Branch (n :#: f (x-1) :#: g (x-1)) :#: p :#: g (x-1)

     f 0 = GrabPen white :#: Go 10
     f x = f (x-1) :#: f (x-1)

     n = Turn 22.5
     p = Turn(-22.5)

---------
-- 32-segment:
    -- angle: 90
    -- start: F+F+F+F
    -- rewrite: F → -F+F-F-F+F+FF-F+F+FF+F-F-FF+ FF-FF+F+F-FF-F-F+FF-F-F+F+F-F+

thirtytwo x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x
    where
      f 0 = GrabPen white :#: Go 10.0

      f x =  p :#: f (x-1) :#: n
            :#: f (x-1) :#: p :#: f (x-1)
            :#: p :#: f (x-1) :#: n :#: f (x-1)
            :#: n :#: f (x-1) :#: f (x-1) :#: p
            :#: f (x-1) :#: n :#: f (x-1) :#: n
            :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1)
            :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1)
            :#: n :#: f (x-1) :#: f (x-1) :#: p
            :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1)
            :#: p :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1)
            :#: p :#: f (x-1) :#: n :#: f (x-1)
            :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1)
            :#: n :#: f (x-1) :#: n
            :#: f (x-1) :#: p :#: f (x-1) :#: n

      n = Turn 90
      p = Turn (-90)


triangle :: Int -> Command
triangle x  =  p :#: f x
  where
  f 0      = Go 10
  f x  = f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
  n        = Turn 90
  p        = Turn (-90)
