type Cplex = (Char,Double,Double)

--Task 1
makeRec :: Double -> Double -> Cplex
makeRec a b = ('R',a,b)

makePol :: Double -> Double -> Cplex
makePol r v
    | v > 2 * pi = error "Angle greater than 2 * pi"
    | v < 0 = error "Angle less than 0"
    | r < 0 = error "Negative radius"
    | otherwise = ('P',r,v)

--Task 2
getRe :: Cplex -> Double
getRe ('R',a,_) = a
getRe ('P',r,v) = r * cos v
getRe _ = error "Invalid argument"

getIm :: Cplex -> Double
getIm ('R',_,b) = b
getIm ('P',r,v) = r * sin v
getIm _ = error "Invalid argument"

getDist :: Cplex -> Double
getDist ('R',a,b) = sqrt (a^2 + b^2)
getDist ('P',r,_) = r
getDist _ = error "Invalid argument"

getAngle :: Cplex -> Double
getAngle ('R',a,b)
    | a == 0 && b == 0 = 0                      --edge case
    | a > 0 && b == 0 = 0                       --edge case
    | a < 0 && b == 0 = pi                      --edge case
    | a == 0 && b > 0 = pi / 2                  --edge case
    | a == 0 && b < 0 = 1.5 * pi                --edge case
    | a > 0 && b > 0 = atan(b / a)              --quadrant 1
    | a < 0 && b > 0 = pi - atan(b / a)         --quadrant 2
    | a < 0 && b < 0 = 1.5 * pi - atan(a / b)   --quadrant 3
    | a > 0 && b < 0 = 2 * pi - atan(b / a)     --quadrant 4
getAngle ('P',_,v) = v
getAngle _ = error "Invalid argument"

--Task 3
toRec :: Cplex -> Cplex
toRec ('R',a,b) = ('R',a,b)
toRec pol@('P',r,v) =
    let a = getRe pol
        b = getIm pol
    in ('R',a,b)
toRec _ = error "Invalid argument"

toPol :: Cplex -> Cplex
toPol rect@('R',a,b) = 
    let r = getDist rect
        v = getAngle rect
    in ('P',r,v)
toPol ('P',r,v) = ('P',r,v)
toPol _ = error "Invalid argument"

--Task 4
compAdd :: Cplex -> Cplex -> Cplex
compAdd ('R',a,b) ('R',c,d) = ('R',a + c,b + d)
compAdd ('R',a,b) pol@('P',r,v) = ('R',a + getRe pol,b + getIm pol)
compAdd pol@('P',r,v) ('R',a,b) = ('R',a + getRe pol, b + getIm pol)
compAdd pol1@('P',r1,v1) pol2@('P',r2,v2) = ('R',(getRe pol1) + getRe pol2,(getIm pol1) + getIm pol2)
compAdd _ _ = error "Invalid argument/s"

compSub :: Cplex -> Cplex -> Cplex
compSub ('R',a,b) ('R',c,d) = ('R',a - c,b - d)
compSub ('R',a,b) pol@('P',r,v) = ('R',a - getRe pol,b - getIm pol)
compSub pol@('P',r,v) ('R',a,b) = ('R',a - getRe pol, b - getIm pol)
compSub pol1@('P',r1,v1) pol2@('P',r2,v2) = ('R',(getRe pol1) - getRe pol2,(getIm pol1) - getIm pol2)
compSub _ _ = error "Invalid argument/s"

compMult :: Cplex -> Cplex -> Cplex
compMult rect1@('R',a,b) rect2@('R',c,d) = 
    let pol1 = toPol rect1
        pol2 = toPol rect2
    in ('P',(getDist pol1) * getDist pol2, (getAngle pol1) + getAngle pol2)
compMult rect@('R',a,b) ('P',r,v) = 
    let pol = toPol rect
    in ('P',r * getDist pol,v + getAngle pol)
compMult ('P',r,v) rect@('R',a,b) = 
    let pol = toPol rect
    in ('P',r * getDist rect,v + getAngle pol)
compMult _ _ = error "Invalid argument/s"

compDiv :: Cplex -> Cplex -> Cplex
compDiv rect1@('R',a,b) rect2@('R',c,d) = 
    let pol1 = toPol rect1
        pol2 = toPol rect2
    in ('P',(getDist pol1) / getDist pol2, (getAngle pol1) - getAngle pol2)
compDiv rect@('R',a,b) ('P',r,v) = 
    let pol = toPol rect
    in ('P',r / getDist pol,v - getAngle pol)
compDiv ('P',r,v) rect@('R',a,b) = 
    let pol = toPol rect
    in ('P',r / getDist rect,v - getAngle pol)
compDiv _ _ = error "Invalid argument/s"