{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import GHC.Real (Integral)
import GHCi.FFI (C_ffi_cif)
type ThreeTuple = (Char, Double, Double)

-- TASK 1:
makeRec :: Double -> Double -> ThreeTuple
makeRec a b = ('R',a,b) -- z = a + b*i

makePol :: Double -> Double -> ThreeTuple
makePol r v
    | v < 0 || v>2*pi = error "Please, insert an angle v in the range of [0,2*pi]"
    | r < 0 = error "Please, insert a distance r ub the range of r>= 0"
    | otherwise = ('P',r,v) -- z = re^(iv)

-- TASK 2:
getRe :: ThreeTuple -> Double
getRe ('R',a,_) = a
getRe ('P',r,v) = r * cos v
getRe _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getIm :: ThreeTuple -> Double
getIm ('R',_,b) = b
getIm ('P',r,v) = r * sin v
getIm _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getDist :: ThreeTuple -> Double
getDist ('R',a,b) = sqrt(a ^2 + b ^2)
getDist ('P',r,_) = r
getDist _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getAngle :: ThreeTuple -> Double
getAngle ('R',a,b)
    | a == 0 && b == 0 = 0 -- edge case
    | a > 0 && b == 0 = 0 -- edge case
    | a < 0 && b == 0 = pi -- edge case
    | b > 0 = atan2 b a -- (0,pi)
    | b < 0 = 2*pi - abs(atan2 b a) -- (pi,2pi) == (pi,2pi)
getAngle ('P',r,v)
    | v > 0 = v
getAngle _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

-- TASK 3:
toRec :: ThreeTuple -> ThreeTuple
toRec tupR@('R',a,b) = tupR
toRec tupP@('P',r,v) = makeRec (getRe tupP) (getIm tupP)
toRec _ =  error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

toPol :: ThreeTuple -> ThreeTuple
toPol tupR@('R',a,b) = makePol (getDist tupR) (getAngle tupR)
toPol tupP@('P',r,v) = tupP
toPol _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

-- TASK 4
compAdd :: ThreeTuple -> ThreeTuple -> ThreeTuple
compAdd ('R',a,b) ('R',c,d) = makeRec (a+c) (b+d)
compAdd im@('P',r,v) ('R',c,d) = makeRec (getRe im + c) (getIm im + d)
compAdd ('R',a,b) im@('P',r,v) = makeRec (a + getRe im) (b + getIm im)
compAdd im1@('R',r1,v1) im2@('R',r2,v2) = makeRec (getRe im1 + getRe im2) (getIm im1 + getIm im2)
compAdd _ _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

compSub :: ThreeTuple -> ThreeTuple -> ThreeTuple
compSub ('R',a,b) ('R',c,d) = ('R',a-c,b-d)
compSub im@('P',r,v) ('R',c,d) = makeRec (getRe im - c) (getIm im - d)
compSub ('R',a,b) im@('P',r,v) = makeRec (a - getRe im) (b - getIm im)
compSub im1@('R',r1,v1) im2@('R',r2,v2) = makeRec (getRe im1 - getRe im2) (getIm im1 - getIm im2)
compSub _ _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

compMult :: ThreeTuple -> ThreeTuple -> ThreeTuple
compMult re1@('R',a,b) re2@('R',c,d) = makePol (getDist re1 * getDist re2) (getAngle re1 + getAngle re2)
compMult ('P',r,v) re@('R',c,d) = makePol (r * getDist re) (v + getAngle re)
compMult re@('R',a,b) ('P',r,v) = makePol (getDist re * r) (getAngle re + v)
compMult ('P',r1,v1) ('P',r2,v2) = makePol (r1 * r2) (v1 + v2)
compMult _ _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

compDiv :: ThreeTuple -> ThreeTuple -> ThreeTuple
compDiv re1@('R',a,b) re2@('R',c,d) = makePol (getDist re1 / getDist re2) (getAngle re1 - getAngle re2)
compDiv ('P',r,v) re@('R',c,d) = makePol (r / getDist re) (v - getAngle re)
compDiv re@('R',a,b) ('P',r,v) = makePol (getDist re / r) (getAngle re - v)
compDiv ('P',r1,v1) ('P',r2,v2) = makePol (r1 / r2) (v1 - v2)
compDiv _ _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

-- TASK 5
type IntegerPair = (Integer,Integer)
genCompList :: IntegerPair -> IntegerPair -> [ThreeTuple]
genCompList p1 p2 = [('R',fromIntegral re, fromIntegral im)| re <- [fst p1 .. snd p1], im <- [fst p2 .. snd p2]]
genCompList _ _ = error "Please, enter some valid input"

listToPol :: [ThreeTuple] -> [ThreeTuple]
listToPol inList = [pol | threeTuple <- inList, let pol = makePol (getDist threeTuple) (getAngle threeTuple)]
listToPol _ = error "Please, enter some valid input"

filterLengths :: Double -> [ThreeTuple] -> [ThreeTuple]
filterLengths k xs = [cplex | cplex <- xs, getDist cplex <= k]
filterLengths _ _ = error "Please, enter some valid input"

filterQuadrant :: Int -> [ThreeTuple] -> [ThreeTuple]
filterQuadrant m inList = 
    if m<1 || m>4 then error "Invalid Quadrant"
    else
        [threeTuple | threeTuple <- inList, isInQuadM threeTuple == m]

isInQuadM :: ThreeTuple -> Int
isInQuadM ('R',a,b)
    | a > 0 && b > 0 = 1
    | a < 0 && b > 0 = 2
    | a < 0 && b < 0 = 3
    | a > 0 && b < 0 = 4
    | otherwise = 42 -- not inside any quad
isInQuadM ('P',_,v)
    | v > 0 && v < pi/2 = 1
    | v > pi/2 && v < pi = 2
    | v > pi && v < 1.5*pi = 3
    | v > 1.5*pi && v < 2*pi = 4 
    | otherwise = 42 -- not inside any quad
isInQuadM _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"