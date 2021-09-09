-- QUESTION 1:
makeRec :: (Floating a, Ord a) => a -> a -> (Char,a,a)
makeRec a b = ('R',a,b) -- z = a + b*i

makePol :: (Floating a, Ord a) => a -> a -> (Char,a,a)
makePol r v 
    | v < 0 || v>2*pi = error "Please, insert an angle v in the range of [0,2pi]"
    | r < 0 = error "Please, insert a distance r ub the range of r>= 0"
    | otherwise = ('P',r,v) -- z = re^(iv)

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

-- QUESTION 2:
getRe :: (Char, Float, Float) -> Float
getRe ('R',a,_) = a
getRe ('P',r,v) = r * cos v
getRe _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getIm :: (Char, Float, Float) -> Float
getIm ('R',_,b) = b
getIm ('P',r,v) = r * sin v
getIm _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getDist :: (Char, Float, Float) -> Float
getDist ('R',a,b) = sqrt(second threeTuple ^2 + third threeTuple ^2)
getDist ('P',r,_) = r
getDist _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getAngle :: (Char, Float, Float) -> Float
getAngle ('R',a,b) 
    | a > 0 && b == 0 = 0
    | a < 0 && b == 0 = pi
    | b > 0 = atan2 b a
    | b < 0 = 2*pi - abs(atan2 b a)
getAngle ('P',r,v)
    | v > 0 = 0
getAngle _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"


getAngle :: (Char, Float, Float) -> Float
getAngle threeTuple
    | first threeTuple == 'R' && third threeTuple > 0 = atan2 (third threeTuple) (second threeTuple)
    | first threeTuple == 'R' && third threeTuple < 0 = 2*pi - abs(atan2 (third threeTuple) (second threeTuple))
    | first threeTuple == 'R' && second threeTuple > 0 && third threeTuple == 0 = 0 
    | first threeTuple == 'R' && second threeTuple < 0 && third threeTuple == 0 = pi
    | first threeTuple == 'P' && third threeTuple > 0 = third threeTuple
    | otherwise = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

-- QUESTION 3:
toRec :: (Char, Float, Float) -> (Char, Float, Float)
toRec ('R',a,b) = ('R',a,b)
toRec ('P',r,v) = makeRec r v
toRec _ =  error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

toPol :: (Char, Float, Float) -> (Char, Float, Float)
toPol ('R',a,b) = makePol a b
toPol ('P',r,v) = ('P',r,v)
toPol _ = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

-- QUESTION 4
compAdd :: (Char, Float, Float) -> String
-- compAdd threeTuple 


compSub :: (Char, Float, Float) -> String
compSub 

compMult :: (Char, Float, Float) -> String
compMult

compDiv :: (Char, Float, Float) -> String
compDiv