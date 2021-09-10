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
getAngle ('R',a,b) = abs ( atan (b / a))
getAngle ('P',_,v) = v
getAngle _ = error "Invalid argument"

--Task 3
toRec :: Cplex -> Cplex
toRec ('R',a,b) = ('R',a,b)
toRec ('P',r,v) =
    let a = r * cos v
        b = r * sin v
    in ('R',a,b)
toRec _ = error "Invalid argument"

toPol :: Cplex -> Cplex
toPol ('R',a,b) = 
    let r = sqrt (a^2 + b^2)
        v = abs (atan (b / a))
    in ('P',r,v)
toPol ('P',r,v) = ('P',r,v)
toPol _ = error "Invalid argument"

--Task 4