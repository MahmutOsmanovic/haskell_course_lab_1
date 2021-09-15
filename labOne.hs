type Complex = (Char, Float, Float) 
type IntegerPair = (Integer,Integer)

makeRec :: Float -> Float -> Complex -- tested
makeRec a b = ('R',a,b) -- z = a + b*i

makePol :: Float -> Float -> Complex -- tested
makePol r v
    | v < 0 || v>2*pi = error "Please, insert an angle v in the range of [0,2*pi]"
    | r < 0 = error "Please, insert a distance r ub the range of r>= 0"
    | otherwise = ('P',r,v) -- z = re^(iv)

getRe :: Complex -> Float -- tested
getRe ('R',a,_) = a
getRe ('P',r,v) = r * cos v
getRe _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getIm :: Complex -> Float -- tested
getIm ('R',_,b) = b
getIm ('P',r,v) = r * sin v
getIm _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getDist :: Complex -> Float -- tested
getDist ('R',a,b) = sqrt(a ^2 + b ^2)
getDist ('P',r,_) = r
getDist _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getAngle :: Complex -> Float  -- tested
getAngle ('R',a,b) --check this
    | a == 0 && b == 0 || a > 0 && b == 0 = 0 -- edge case
    | a < 0 && b == 0 = pi -- edge case
    | b > 0 = atan2 b a -- (0,pi)
    | b < 0 = 2*pi - abs(atan2 b a) -- (pi,2pi) == (pi,2pi)
getAngle ('P',r,v) = if v > 0 then v else error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

angleRem :: Float -> Float        --makes sure the sum/difference of 2 angles is within [0,2*pi]
angleRem v 
    | v > 2 * pi = v - 2 * pi
    | v < 0 = v + 2 * pi
    | otherwise = v

toRec :: Complex -> Complex -- tested
toRec z = makeRec (getRe z) (getIm z)

toPol :: Complex -> Complex -- tested
toPol z = makePol (getDist z) (getAngle z)

compAdd :: Complex -> Complex -> Complex -- tested
compAdd z1 z2 = makeRec (getRe z1 + getRe z2) (getIm z1 + getIm z2) 

compSub :: Complex -> Complex -> Complex -- tested
compSub z1 z2 = makeRec (getRe z1 - getRe z2) (getIm z1 - getIm z2) 

compMult :: Complex -> Complex -> Complex -- tested
compMult z1 z2 = makePol (getDist z1 * getDist z2) (angleRem(getAngle z1 + getAngle z2))

compDiv :: Complex -> Complex -> Complex -- tested
compDiv z1 z2 = makePol (getDist z1 / getDist z2) (angleRem(getAngle z1 - getAngle z2))

genCompList :: IntegerPair -> IntegerPair -> [Complex] -- tested
genCompList p1 p2 = [('R',fromIntegral re, fromIntegral im)| re <- [fst p1 .. snd p1], im <- [fst p2 .. snd p2]]

listToPol :: [Complex] -> [Complex] -- tested
listToPol inList = [pol | threeTuple <- inList, let pol = makePol (getDist threeTuple) (getAngle threeTuple)]

filterLengths :: Float -> [Complex] -> [Complex] -- tested
filterLengths k xs = [cplex | cplex <- xs, getDist cplex <= k]

filterQuadrant :: Int -> [Complex] -> [Complex] -- tested
filterQuadrant m inList = [cplex | m>=1 && m<=4, cplex <- inList, isInQuadM cplex m]

isInQuadM :: Complex -> Int -> Bool -- tested
isInQuadM z m
    | re > 0 && im > 0 && m == 1 = True
    | re < 0 && im > 0 && m == 2 = True
    | re < 0 && im < 0 && m == 3 = True
    | re > 0 && im < 0 && m == 4 = True
    | otherwise = False -- not inside any quad
        where re = getRe z
              im = getIm z