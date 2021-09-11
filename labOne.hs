type ThreeTuple = (Char, Double, Double)

-- QUESTION 1:
makeRec :: Double -> Double -> ThreeTuple
makeRec a b = ('R',a,b) -- z = a + b*i

makePol :: Double -> Double -> ThreeTuple
makePol r v 
    | v < 0 || v>2*pi = error "Please, insert an angle v in the range of [0,2pi]"
    | r < 0 = error "Please, insert a distance r ub the range of r>= 0"
    | otherwise = ('P',r,v) -- z = re^(iv)

-- QUESTION 2:
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
    | a == 0 && b == 0 = 0
    | a > 0 && b == 0 = 0
    | a < 0 && b == 0 = pi
    | b > 0 = atan2 b a
    | b < 0 = 2*pi - abs(atan2 b a)
getAngle ('P',r,v)
    | v > 0 = 0
getAngle _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

-- QUESTION 3:
toRec :: ThreeTuple -> ThreeTuple
toRec tupR@('R',a,b) = tupR
toRec tupP@('P',r,v) = makeRec (getRe tupP) (getIm tupP)
toRec _ =  error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

toPol :: ThreeTuple -> ThreeTuple
toPol tupR@('R',a,b) = makePol (getDist tupR) (getAngle tupR)
toPol tupP@('P',r,v) = tupP
toPol _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

-- QUESTION 4