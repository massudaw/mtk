{-# LANGUAGE TypeSynonymInstances,TypeOperators,TypeFamilies,FlexibleContexts,FlexibleInstances,NoMonomorphismRestriction #-}
module Solver.RungeKutta where

import Data.Ratio
import Control.Applicative
import Space.Class
import Linear.Vector
import Data.Maybe
import Data.List
import Data.Functor.Compose
import Data.Functor.Product
import Linear.V1
import Local
import Control.Lens
import Data.Traversable(Traversable)
import CartesianProduct

rk_version = "1.0.2"

-- Store a list of rational numbers as a common denominator, then a list
-- of numerators, all stored as doubles: this lets us write the values in
-- the source as exact rational numbers, yet we can take advantage of not
-- having to do too many conversions and also of not having to multiply
-- by 0 or 1 in some cases.




type RCL = (Double, [Double])

ratToRCL :: [Rational] -> RCL
ratToRCL [] = (1.0, [])
ratToRCL rs =
  let ds = map denominator rs
      dp = foldl1' (*) ds
      ns = map (numerator . (*(fromInteger dp))) rs
      g = foldl' gcd dp ns
  in (fromInteger (quot dp g), map (fromInteger . (flip quot g)) ns)

ratToRCLs :: [[Rational]] -> [RCL]
ratToRCLs = map ratToRCL

ratToDbls :: [Rational] -> [Double]
ratToDbls = map fromRational

-- Helper function to sum a list of K_i, skipping
-- un-necessary multiplications and additions

sumS [] = pure 0
sumS x = foldl1 (^+^) x

k_sum h (d,ns) ks =
  (h/d) *^ (sumS (zipWith (*^) ns ks))


-- Helper function to generate a list of K_i

gen_ksM der_fn h old ks [] [] = ks
gen_ksM der_fn h old@((V1 tn) :|: yn) ks (c:cs) (a:as) =
  gen_ksM der_fn h old (ks ++ [der_fn ((V1 $ tn + c*h) :|:
          (yn |+|  (k_sum h a ks)))]) cs as

gen_ks der_fn h (tn,yn) ks [] [] = ks
gen_ks der_fn h old@(tn,yn) ks (c:cs) (a:as) =
  gen_ks der_fn h old (ks ++ [der_fn (tn + c*h)
          (yn |+|  (k_sum h a ks))]) cs as

core1 cl al bl der_fn h old@(tn,yn) = (tn + h, yn |+| (k_sum h bl (gen_ks der_fn h old [] cl al)))

core1M
  :: (RealFloat a, Space b) =>
     [a]
     -> [(a, [a])]
     -> (a, [a])
     -> ((V1 :|: b)a -> Local b a)
     ->  a
     -> (V1 :|: b) a
     -> (V1 :|: b) a
core1M cl al bl der_fn h old = old |+| (V1 h :|: (k_sum h bl (gen_ksM der_fn h old [] cl al)))

-- This is the second core routine, analogous to the previous one.
-- The difference is that this gets an additional internal table arg,
-- and it returns a 3-tuple instead of a 2-tuple: (tnew,ynew,enew),
-- where enew is the error state vector
-- e_{n+1} = h sum_{i=1}^s (b_i - b'_i) k_i

core2 cl al bl dl der_fn h old@(tn,yn) =
  let ks = gen_ks der_fn h old [] cl al
  in (tn + h, yn  |+| (k_sum h bl ks), k_sum h dl ks)

-- A pair of helper routines to show the internal tables

rk_show1 :: String -> [Double] -> [RCL] -> RCL -> String
rk_show1 title cs as bs =
  title ++ ":\ncs:\t" ++ (show cs) ++ "\nas:\t" ++
    foldl1' gl (map show as) ++ "\nbs:\t" ++ (show bs)
  where gl a b = a ++ "\n\t" ++ b

rk_show2 :: String -> [Double] -> [RCL] -> RCL -> RCL -> String
rk_show2 title cs as bs ds =
  title ++ ":\ncs:\t" ++ (show cs) ++ "\nas:\t" ++
    foldl1' gl (map show as) ++ "\nbs:\t" ++ (show bs) ++
    "\nds:\t" ++ (show ds)
  where gl a b = a ++ "\n\t" ++ b

-- Some specific explicit methods, taken from
-- "List of Runge-Kutta methods" at Wikipedia

-- forward Euler: unconditionally unstable: don't use this!
-- If you do, your dangly bits will fall off!

cs_fe = ratToDbls [0]
as_fe = ratToRCLs [[]]
bs_fe = ratToRCL  [1]
rkfe = core1 cs_fe as_fe bs_fe
show_rkfe = rk_show1 "Forward Euler" cs_fe as_fe bs_fe

-- Kutta's third-order method:

cs_rk3 = ratToDbls [0, 1%2, 1]
as_rk3 = ratToRCLs [[], [1%2], [-1, 2]]
bs_rk3 = ratToRCL  [1%6, 2%3, 1%6]
rk3 = core1 cs_rk3 as_rk3 bs_rk3
show_rk3 = rk_show1 "Kutta\'s third-order method" cs_rk3 as_rk3 bs_rk3

-- Classic fourth-order method

cs_rk4a = ratToDbls [0, 1%2, 1%2, 1]
as_rk4a = ratToRCLs [[], [1%2], [0, 1%2], [0, 0, 1]]
bs_rk4a = ratToRCL  [1%6, 1%3, 1%3, 1%6]
rk4a = core1 cs_rk4a as_rk4a bs_rk4a
show_rk4a = rk_show1 "Classic fourth-order method" cs_rk4a as_rk4a bs_rk4a

-- Kutta's other fourth-order method... "The first [above] is more popular,
-- the second is more precise." (Hairer, Norsett, Wanner)

cs_rk4b = ratToDbls [0, 1%3, 2%3, 1]
as_rk4b = ratToRCLs [[], [1%3], [-1%3, 1], [1, -1, 1]]
bs_rk4b = ratToRCL  [1%8, 3%8, 3%8, 1%8]
rk4b = core1 cs_rk4b as_rk4b bs_rk4b
show_rk4b = rk_show1 "Kutta's other classic fourth-order method" cs_rk4b as_rk4b bs_rk4b

-- Some adaptive-stepsize methods, also from Wikipedia; more from HNW.
-- These don't auto-adapt, but they do allow the user to make a somewhat
-- intelligent decision about what the step size ought to be at each step.

-- Helper function to take the difference of two lists of rationals:
-- we don't want to use zipWith (-) because that gives only the head where
-- both lists have entries; we want implicit zeros at the end, as far as
-- is necessary.

diffs [] [] = []
diffs xs [] = xs
diffs [] xs = map negate xs
diffs (x:xs) (y:ys) = (x - y) : diffs xs ys

-- Heun-Euler, order 2/1

cs_he = ratToDbls [0, 1]
as_he = ratToRCLs [[], [1]]
r1_he = [1%2, 1%2]		-- second-order coeffs
r2_he = [1]			-- first-order coeffs
bs_he = ratToRCL r1_he
ds_he = ratToRCL (diffs r1_he r2_he)
rkhe = core2 cs_he as_he bs_he ds_he
show_rkhe = rk_show2 "Heun-Euler 2(1)" cs_he as_he bs_he ds_he

bs_he_aux = ratToRCL r2_he
rkhe_aux = core1 cs_he as_he bs_he_aux
show_rkhe_aux = rk_show1 "Heun-Euler (1)" cs_he as_he bs_he_aux

-- Bogacki-Shampine, order 3/2

cs_bs = ratToDbls [0, 1%2, 3%4, 1]
as_bs = ratToRCLs [[], [1%2], [0, 3%4], [2%9, 1%3, 4%9]]
r1_bs = [2%9, 1%3, 4%9]		-- third-order coeffs
r2_bs = [7%24, 1%4, 1%3, 1%8]	-- second-order coeffs
bs_bs = ratToRCL r1_bs
ds_bs = ratToRCL (diffs r1_bs r2_bs)
rkbs = core2 cs_bs as_bs bs_bs ds_bs
show_rkbs = rk_show2 "Bogacki-Shampine 3(2)" cs_bs as_bs bs_bs ds_bs

bs_bs_aux = ratToRCL r2_bs
rkbs_aux = core1 cs_bs as_bs bs_bs_aux
show_rkbs_aux = rk_show1 "Bogacki-Shampine (2)" cs_bs as_bs bs_bs_aux

-- Runge-Kutta-Fehlberg, order 4/5

cs_rkf = ratToDbls [0, 1%4, 3%8, 12%13, 1, 1%2]
as_rkf = ratToRCLs [[],
                    [1%4],
                    [3%32, 9%32],
                    [1932%2197, -7200%2197, 7296%2197],
                    [439%216, -8, 3680%513, -845%4104],
                    [-8%27, 2, -3544%2565, 1859%4104, -11%40]]
-- fourth-order coeffs
r1_rkf = [25%216, 0, 1408%2565, 2197%4104, -1%5]
-- fifth-order coeffs
r2_rkf = [16%135, 0, 6656%12825, 28561%56430, -9%50, 2%55]
bs_rkf = ratToRCL r1_rkf
ds_rkf = ratToRCL (diffs r1_rkf r2_rkf)
rkf45 = core2 cs_rkf as_rkf bs_rkf ds_rkf
show_rkf45 = rk_show2 "Runge-Kutta-Fehlberg 4(5)" cs_rkf as_rkf bs_rkf ds_rkf

bs_rkf_aux = ratToRCL r2_rkf
rkf45_aux = core1 cs_rkf as_rkf bs_rkf_aux
rkf45_aux_tan = core1M cs_rkf as_rkf bs_rkf_aux
--rkf45_auxM = core1M cs_rkf as_rkf bs_rkf_aux
show_rkf45_aux = rk_show1 "Runge-Kutta-Fehlberg (5)" cs_rkf as_rkf bs_rkf_aux

-- Cash-Karp, order 4/5 (use 4th-order sol'n,
-- coeffs chosen to minimize error of 4th-order sol'n)

cs_ck = ratToDbls [0, 1%5, 3%10, 3%5, 1, 7%8]
as_ck = ratToRCLs [[],
                   [1%5],
                   [3%40, 9%40],
                   [3%10, -9%10, 6%5],
                   [-11%54, 5%2, -70%27, 35%27],
                   [1631%55296, 175%512, 575%13824, 44275%110592, 253%4096]]
-- fourth-order coeffs
r1_ck = [2825%27648, 0, 18575%48384, 13525%55296, 277%14336, 1%4]
-- fifth-order coeffs
r2_ck = [37%378, 0, 250%621, 125%594, 0, 512%1771]
bs_ck = ratToRCL r1_ck
ds_ck = ratToRCL (diffs r1_ck r2_ck)
rkck = core2 cs_ck as_ck bs_ck ds_ck
show_rkck = rk_show2 "Cash-Karp 4(5)" cs_ck as_ck bs_ck ds_ck

bs_ck_aux = ratToRCL r2_ck
rkck_aux = core1 cs_ck as_ck bs_ck_aux
show_rkck_aux = rk_show1 "Cash-Karp (5)" cs_ck as_ck bs_ck_aux

-- Dormand-Prince, order 5/4 (use 5th-order sol'n,
-- coeffs chosen to minimize error of 5th-order sol'n)
-- This is DOPRI5 from Hairer, Norsett, Wanner

cs_dp = ratToDbls [0, 1%5, 3%10, 4%5, 8%9, 1, 1]
as_dp = ratToRCLs [[],
                   [1%5],
                   [3%40, 9%40],
                   [44%45, -56%15, 32%9],
                   [19372%6561, -25360%2187, 64448%6561, -212%729],
                   [9017%3168, -355%33, 46732%5247, 49%176, -5103%18656],
                   [35%384, 0, 500%1113, 125%192, -2187%6784, 11%84]]
-- fifth-order coeffs
r1_dp = [35%384, 0, 500%1113, 125%192, -2187%6784, 11%84]
-- fourth-order coeffs
r2_dp = [5179%57600, 0, 7571%16695, 393%640, -92097%339200, 187%2100, 1%40]
bs_dp = ratToRCL r1_dp
ds_dp = ratToRCL (diffs r1_dp r2_dp)
rkdp = core2 cs_dp as_dp bs_dp ds_dp
show_rkdp = rk_show2 "Dormand-Prince 5(4) \"DOPRI5\"" cs_dp as_dp bs_dp ds_dp

bs_dp_aux = ratToRCL r2_dp
rkdp_aux = core1 cs_dp as_dp bs_dp_aux
show_rkdp_aux = rk_show1 "Dormand-Prince (4)" cs_dp as_dp bs_dp_aux

-- Fehlberg, order 7/8: "... of frequent use in all high precision
-- computations, e.g., in astronomy." -- Hairer, Norsett, Wanner.
-- But caveat: suffers from the drawback that error estimate is
-- identically 0 for quadrature problems y' = f(x)

-- NOTE BUG in Hairer Norsett Wanner: the third-last A coefficient in the
-- last row of the tableau is listed as "19/41" in the book. This is WRONG:
-- that row does not then sum to 1, and the convergence of the auxiliary
-- solver is then order 1 or 2, not 8

cs_f78 = ratToDbls [0, 2%27, 1%9, 1%6, 5%12, 1%2, 5%6, 1%6, 2%3, 1%3, 1, 0, 1]
as_f78 =
  ratToRCLs
    [[],
    [2%27],
    [1%36, 1%12],
    [1%24, 0, 1%8],
    [5%12, 0, -25%16, 25%16],
    [1%20, 0, 0, 1%4, 1%5],
    [-25%108, 0, 0, 125%108, -65%27, 125%54],
    [31%300, 0, 0, 0, 61%225, -2%9, 13%900],
    [2, 0, 0, -53%6, 704%45, -107%9, 67%90, 3],
    [-91%108, 0, 0, 23%108, -976%135, 311%54, -19%60, 17%6, -1%12],
    [2383%4100, 0, 0, -341%164, 4496%1025, -301%82, 2133%4100, 45%82, 45%164, 18%41],
    [3%205, 0, 0, 0, 0, -6%41, -3%205, -3%41, 3%41, 6%41, 0],
    [-1777%4100, 0, 0, -341%164, 4496%1025, -289%82, 2193%4100, 51%82, 33%164, 12%41, 0, 1]]
-- seventh-order coeffs
r1_f78 = [41%840, 0, 0, 0, 0, 34%105, 9%35, 9%35, 9%280, 9%280, 41%840]
-- eighth-order coeffs
r2_f78 = [0, 0, 0, 0, 0, 34%105, 9%35, 9%35, 9%280, 9%280, 0, 41%840, 41%840]
bs_f78 = ratToRCL r1_f78
ds_f78 = ratToRCL (diffs r1_f78 r2_f78)
rkf78 = core2 cs_f78 as_f78 bs_f78 ds_f78
show_rkf78 = rk_show2 "Fehlberg 7(8)" cs_f78 as_f78 bs_f78 ds_f78

bs_f78_aux = ratToRCL r2_f78
rkf78_aux = core1 cs_f78 as_f78 bs_f78_aux
show_rkf78_aux = rk_show1 "Fehlberg (8)" cs_f78 as_f78 bs_f78_aux

-- Verner, order 6/5 "DVERK"

cs_v65 = ratToDbls [0, 1%6, 4%15, 2%3, 5%6, 1, 1%15, 1]
as_v65 =
  ratToRCLs
    [[],
    [1%6],
    [4%75, 16%75],
    [5%6, -8%3, 5%2],
    [-165%64, 55%6, -425%64, 85%96],
    [12%5, -8, 4015%612, -11%36, 88%255],
    [-8263%15000, 124%75, -643%680, -81%250, 2484%10625],
    [3501%1720, -300%43, 297275%52632, -319%2322, 24068%84065, 0, 3850%26703]]
-- sixth-order coeffs
r1_v65 = [3%40, 0, 875%2244, 23%72, 264%1955, 0, 125%11592, 43%616]
-- fifth-order coeffs
r2_v65 = [13%160, 0, 2375%5984, 5%16, 12%85, 3%44]
bs_v65 = ratToRCL r1_v65
ds_v65 = ratToRCL (diffs r1_v65 r2_v65)
rkv65 = core2 cs_v65 as_v65 bs_v65 ds_v65
show_rkv65 = rk_show2 "Verner 6(5) \"DVERK\"" cs_v65 as_v65 bs_v65 ds_v65

bs_v65_aux = ratToRCL r2_v65
rkv65_aux = core1 cs_v65 as_v65 bs_v65_aux
show_rkv65_aux = rk_show1 "Verner (5)" cs_v65 as_v65 bs_v65_aux
