type Cplex = (Char,Float,Float)

--Task 1
makeRec :: Float -> Float -> Cplex
makeRec a b = ('R',a,b)

makePol :: Float -> Float -> Cplex
makePol r v
    | v > 2 * pi = error "Angle greater than 2 * pi"
    | v < 0 = error "Angle less than 0"
    | r < 0 = error "Negative radius"
    | otherwise = ('P',r,v)

--Task 2
getRe :: Cplex -> Float
getRe ('R',a,_) = a
getRe ('P',r,v) = r * cos v
getRe _ = error "Invalid argument"

getIm :: Cplex -> Float
getIm ('R',_,b) = b
getIm ('P',r,v) = r * sin v
getIm _ = error "Invalid argument"

getDist :: Cplex -> Float
getDist ('R',a,b) = sqrt (a^2 + b^2)
getDist ('P',r,_) = r
getDist _ = error "Invalid argument"

getAngle :: Cplex -> Float
getAngle ('R',a,b)
    | a == 0 && b == 0 || a > 0 && b == 0 = 0           --edge case
    | a < 0 && b == 0 = pi                              --edge case
    | a == 0 && b > 0 = pi / 2                          --edge case
    | a == 0 && b < 0 = 1.5 * pi                        --edge case
    | a > 0 && b > 0 = atan(b / a)                      --quadrant 1
    | a < 0 && b > 0 = pi - atan(b / abs a)             --quadrant 2
    | a < 0 && b < 0 = 1.5 * pi - atan(abs a / abs b)   --quadrant 3
    | a > 0 && b < 0 = 2 * pi - atan(abs b / a)         --quadrant 4
getAngle ('P',_,v) = v
getAngle _ = error "Invalid argument"

--Task 3
toRec :: Cplex -> Cplex
toRec c = ('R',getRe c,getIm c)

toPol :: Cplex -> Cplex
toPol c = ('P',getDist c,getAngle c)

--Task 4
compAdd :: Cplex -> Cplex -> Cplex
compAdd c1 c2 = ('R',getRe c1 + getRe c2,getIm c1 + getIm c2)

compSub :: Cplex -> Cplex -> Cplex
compSub c1 c2 = ('R',getRe c1 - getRe c2,getIm c1 - getIm c2)

angleRem :: Float -> Float        --makes sure the sum/difference of 2 angles is within [0,2*pi]
angleRem v 
    | v > 2 * pi = v - 2 * pi
    | v < 0 = v + 2 * pi
    | otherwise = v

compMult :: Cplex -> Cplex -> Cplex
compMult c1 c2 = ('P',getDist c1 * getDist c2,angleRem (getAngle c1 + getAngle c2))

compDiv :: Cplex -> Cplex -> Cplex
compDiv c1 c2 = ('P',getDist c1 / getDist c2,angleRem (getAngle c1 - getAngle c2))

--Task 5
genCompList :: Integral n => (n,n) -> (n,n) -> [Cplex]
genCompList (a,b) (c,d) = if a > b || c > d then error "Invalid range/s" else [('R',fromIntegral x,fromIntegral y)| x <- [a..b], y <- [c..d]]

listToPol :: [Cplex] -> [Cplex]
listToPol list = [ toPol t | t <- list]

filterLengths :: Float -> [Cplex] -> [Cplex]
filterLengths k xs = [ t | t <- xs, getDist t <= k]

findQuadrant :: Cplex -> Int
findQuadrant c = if getAngle c > 0 && getAngle c < pi / 2 then 1 else if getAngle c > pi / 2 && getAngle c < pi then 2 
                 else if getAngle c > pi && getAngle c < 1.5 * pi then 3 else if getAngle c > 1.5 * pi && getAngle c < 2*pi then 4 else 42 --not inside a quadrant

filterQuadrant :: Int -> [Cplex] -> [Cplex]
filterQuadrant m xs = [t | t <- xs, m >= 1, m <= 4, findQuadrant t == m]