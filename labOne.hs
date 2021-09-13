type Complex = (Char, Double, Double)

makeRec :: Double -> Double -> Complex
makeRec a b = ('R',a,b) -- z = a + b*i

makePol :: Double -> Double -> Complex
makePol r v
    | v < 0 || v>2*pi = error "Please, insert an angle v in the range of [0,2*pi]"
    | r < 0 = error "Please, insert a distance r ub the range of r>= 0"
    | otherwise = ('P',r,v) -- z = re^(iv)

getRe :: Complex -> Double
getRe ('R',a,_) = a
getRe ('P',r,v) = r * cos v
getRe _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getIm :: Complex -> Double
getIm ('R',_,b) = b
getIm ('P',r,v) = r * sin v
getIm _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getDist :: Complex -> Double
getDist ('R',a,b) = sqrt(a ^2 + b ^2)
getDist ('P',r,_) = r
getDist _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getAngle :: Complex -> Double
getAngle ('R',a,b) --check this
    | a == 0 && b == 0 = 0 -- edge case
    | a > 0 && b == 0 = 0 -- edge case
    | a < 0 && b == 0 = pi -- edge case
    | b > 0 = atan2 b a -- (0,pi)
    | b < 0 = 2*pi - abs(atan2 b a) -- (pi,2pi) == (pi,2pi)
getAngle ('P',r,v)
    | v > 0 = v
getAngle _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

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

type IntegerPair = (Integer,Integer)
genCompList :: IntegerPair -> IntegerPair -> [Complex]
genCompList p1 p2 = [('R',fromIntegral re, fromIntegral im)| re <- [fst p1 .. snd p1], im <- [fst p2 .. snd p2]]

listToPol :: [Complex] -> [Complex]
listToPol inList = [pol | threeTuple <- inList, let pol = makePol (getDist threeTuple) (getAngle threeTuple)]

filterLengths :: Double -> [Complex] -> [Complex]
filterLengths k xs = [cplex | cplex <- xs, getDist cplex <= k]

filterQuadrant :: Int -> [Complex] -> [Complex]
filterQuadrant m inList =
    if m<1 || m>4 then error "Invalid Quadrant"
    else
        [cplex | cplex <- inList, isInQuadM cplex == m]

isInQuadM :: Complex -> Int
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