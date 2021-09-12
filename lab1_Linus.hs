type Cplex = (Char,Double,Double)

--Task 1
makeRec :: Double -> Double -> Cplex
makeRec a b = ('R',a,b)                                 --tested

makePol :: Double -> Double -> Cplex
makePol r v
    | v > 2 * pi = error "Angle greater than 2 * pi"    --tested
    | v < 0 = error "Angle less than 0"                 --tested
    | r < 0 = error "Negative radius"                   --tested
    | otherwise = ('P',r,v)                             --tested

--Task 2
getRe :: Cplex -> Double
getRe ('R',a,_) = a                         --tested
getRe ('P',r,v) = r * cos v                 --tested
getRe _ = error "Invalid argument"          --tested

getIm :: Cplex -> Double
getIm ('R',_,b) = b                         --tested
getIm ('P',r,v) = r * sin v                 --tested
getIm _ = error "Invalid argument"          --tested

getDist :: Cplex -> Double
getDist ('R',a,b) = sqrt (a^2 + b^2)        --tested
getDist ('P',r,_) = r                       --tested
getDist _ = error "Invalid argument"        --tested

getAngle :: Cplex -> Double
getAngle ('R',a,b)
    | a == 0 && b == 0 = 0                      --edge case     --tested
    | a > 0 && b == 0 = 0                       --edge case     --tested
    | a < 0 && b == 0 = pi                      --edge case     --tested
    | a == 0 && b > 0 = pi / 2                  --edge case     --tested
    | a == 0 && b < 0 = 1.5 * pi                --edge case     --tested
    | a > 0 && b > 0 = atan(b / a)              --quadrant 1    --tested
    | a < 0 && b > 0 = pi - atan(y / x)         --quadrant 2    --tested
    | a < 0 && b < 0 = 1.5 * pi - atan(x / y)   --quadrant 3    --tested
    | a > 0 && b < 0 = 2 * pi - atan(y / x)     --quadrant 4    --tested
        where x = abs a
              y = abs b
getAngle ('P',_,v) = v                                          --tested
getAngle _ = error "Invalid argument"                           --tested

--Task 3
toRec :: Cplex -> Cplex
toRec ('R',a,b) = ('R',a,b)             --tested
toRec pol@('P',r,v) =                   --tested
    let a = getRe pol
        b = getIm pol
    in ('R',a,b)
toRec _ = error "Invalid argument"      --tested

toPol :: Cplex -> Cplex
toPol rect@('R',a,b) =                  --tested
    let r = getDist rect
        v = getAngle rect
    in ('P',r,v)
toPol ('P',r,v) = ('P',r,v)             --tested
toPol _ = error "Invalid argument"      --tested

--Task 4
compAdd :: Cplex -> Cplex -> Cplex
compAdd ('R',a,b) ('R',c,d) = ('R',a + c,b + d)                                                             --tested
compAdd ('R',a,b) pol@('P',r,v) = ('R',a + getRe pol,b + getIm pol)                                         --tested
compAdd pol@('P',r,v) ('R',a,b) = ('R',getRe pol + a,getIm pol + b)                                         --tested
compAdd pol1@('P',r1,v1) pol2@('P',r2,v2) = ('R',getRe pol1 + getRe pol2,getIm pol1 + getIm pol2)           --tested
compAdd _ _ = error "Invalid argument/s"                                                                    --tested

compSub :: Cplex -> Cplex -> Cplex
compSub ('R',a,b) ('R',c,d) = ('R',a - c,b - d)                                                             --tested
compSub ('R',a,b) pol@('P',r,v) = ('R',a - getRe pol,b - getIm pol)                                         --tested
compSub pol@('P',r,v) ('R',a,b) = ('R',getRe pol - a,getIm pol - b)                                         --tested
compSub pol1@('P',r1,v1) pol2@('P',r2,v2) = ('R',getRe pol1 - getRe pol2,getIm pol1 - getIm pol2)           --tested
compSub _ _ = error "Invalid argument/s"                                                                    --tested                                              

angleRem :: Double -> Double        --makes sure the sum/difference of 2 angles is within [0,2*pi]          --tested
angleRem v 
    | v > 2 * pi = v - 2 * pi
    | v < 0 = v + 2 * pi
    | otherwise = v

compMult :: Cplex -> Cplex -> Cplex
compMult rect1@('R',a,b) rect2@('R',c,d) =                  --tested
    let pol1 = toPol rect1
        pol2 = toPol rect2
        distance = getDist pol1 * getDist pol2
        angle = angleRem (getAngle pol1 + getAngle pol2)
    in ('P',distance,angle)
compMult rect@('R',a,b) ('P',r,v) =                         --tested
    let pol = toPol rect
        distance = r * getDist pol
        angle = angleRem (v + getAngle pol)
    in ('P',distance,angle)
compMult ('P',r,v) rect@('R',a,b) =                         --tested
    let pol = toPol rect
        distance = r * getDist rect
        angle = angleRem (v + getAngle pol) 
    in ('P',distance,angle)
compMult ('P',r1,v1) ('P',r2,v2) =                          --tested
    let distance = r1 * r2
        angle = angleRem (v1 + v2) 
    in ('P',distance,angle)
compMult _ _ = error "Invalid argument/s"                   --tested

compDiv :: Cplex -> Cplex -> Cplex
compDiv rect1@('R',a,b) rect2@('R',c,d) =                   --tested
    let pol1 = toPol rect1
        pol2 = toPol rect2
        distance = getDist pol1 / getDist pol2
        angle = angleRem (getAngle pol1 - getAngle pol2)
    in ('P',distance, angle)
compDiv rect@('R',a,b) ('P',r,v) =                          --tested
    let pol = toPol rect
        distance = getDist pol / r
        angle = angleRem (getAngle pol - v)
    in ('P',distance,angle)
compDiv ('P',r,v) rect@('R',a,b) =                          --tested
    let pol = toPol rect
        distance = r / getDist rect
        angle = angleRem (v - getAngle pol)
    in ('P',distance,angle)
compDiv ('P',r1,v1) ('P',r2,v2) =                           --tested
    let distance = r1 / r2
        angle = angleRem (v1 - v2)
    in ('P',distance,angle)
compDiv _ _ = error "Invalid argument/s"                    --tested