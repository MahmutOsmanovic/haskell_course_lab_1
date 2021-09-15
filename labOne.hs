type Complex = (Char, Float, Float)

makeRec :: Float -> Float -> Complex
makeRec a b = ('R',a,b) -- z = a + b*i

makePol :: Float -> Float -> Complex
makePol r v -- z = r*e^(i(v))
    | v > 2 * pi && v <= 4 * pi = ('P',abs r,v - 2 * pi)
    | v < 0 = ('P',abs r,v + 2 * pi)
    | otherwise = ('P',abs r,v)

getRe :: Complex -> Float
getRe ('R',a,_) = a
getRe ('P',r,v) = r * cos v
getRe _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getIm :: Complex -> Float
getIm ('R',_,b) = b
getIm ('P',r,v) = r * sin v
getIm _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getDist :: Complex -> Float
getDist ('R',a,b) = sqrt(a ^2 + b ^2)
getDist ('P',r,_) = r
getDist _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getAngle :: Complex -> Float
getAngle ('R',a,b) 
    | a == 0 && b == 0 || a > 0 && b == 0 = 0 -- edge case
    | a < 0 && b == 0 = pi -- edge case
    | b > 0 = atan2 b a -- (0,pi)
    | b < 0 = 2*pi - abs(atan2 b a) -- (pi,2pi) == (pi,2pi)
getAngle ('P',r,v) = if r>0 && v>=0 then v else error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

toRec :: Complex -> Complex
toRec z = makeRec (getRe z) (getIm z)

toPol :: Complex -> Complex
toPol z = makePol (getDist z) (getAngle z)

compAdd :: Complex -> Complex -> Complex
compAdd z1 z2 = makeRec (getRe z1 + getRe z2) (getIm z1 + getIm z2) 

compSub :: Complex -> Complex -> Complex
compSub z1 z2 = makeRec (getRe z1 - getRe z2) (getIm z1 - getIm z2) 

compMult :: Complex -> Complex -> Complex
compMult z1 z2 = makePol (getDist z1 * getDist z2) (getAngle z1 + getAngle z2)

compDiv :: Complex -> Complex -> Complex
compDiv z1 z2 = makePol (getDist z1 / getDist z2) (getAngle z1 - getAngle z2)

genCompList :: (Integer,Integer) -> (Integer,Integer) -> [Complex] -- get pol form by comb re and im input
genCompList (a,b) (c,d) = if a > b || c > d then error "Invalid range/s" else [('R',fromIntegral x,fromIntegral y)| x <- [a..b], y <- [c..d]]

listToPol :: [Complex] -> [Complex] -- make all complex inputs on polar form
listToPol list = [toPol t | t <- list]

filterLengths :: Float -> [Complex] -> [Complex] -- get all coor in circle within radius k
filterLengths k xs = [cplex | cplex <- xs, getDist cplex <= k]

filterQuadrant :: Int -> [Complex] -> [Complex] -- get all coor in quad m
filterQuadrant m xs = [cplex | m>=1 && m<=4, cplex <- xs, let v = getAngle cplex in v > (fromIntegral m - 1)*pi/2 && v < fromIntegral m * pi / 2]