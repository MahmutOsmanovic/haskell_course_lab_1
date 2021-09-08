-- QUESTION 1:

makeRec :: Floating a => a -> a -> (Char,a,a)
makeRec a b = ('R',a,b) -- z = a + b*i

makePol :: (Floating a, Ord a) => a -> a -> (Char,a,a)
makePol r v 
    | v < 0 || v>2*pi = error "Please, insert an angle v in the range of [0,2pi]"
    | r < 0 = error "Please, insert a distance r ub the range of r>= 0"
    | otherwise = ('P', r, v) -- z = re^(iv)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- QUESTION 2:
getRe :: (Char, Float, Float) -> Float
getRe threeTuple
    | first threeTuple == 'R' = second threeTuple
    | first threeTuple == 'P' = second threeTuple * cos (third threeTuple)
    | otherwise = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getIm :: (Char, Float, Float) -> Float
getIm threeTuple
    | first threeTuple == 'R' = third threeTuple
    | first threeTuple == 'P' = second threeTuple * sin (third threeTuple)
    | otherwise = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getDist :: (Char, Float, Float) -> Float
getDist threeTuple 
    | first threeTuple == 'R' = sqrt(second threeTuple ^2 + third threeTuple ^2)
    | first threeTuple == 'P' = second threeTuple
    | otherwise = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

getAngle :: (Char, Float, Float) -> Float
getAngle threeTuple
    | first threeTuple == 'R' && third threeTuple > 0 = atan2 (third threeTuple) (second threeTuple)
    | first threeTuple == 'R' && third threeTuple < 0 = 2*pi - abs(atan2 (third threeTuple) (second threeTuple))
    | first threeTuple == 'R' && second threeTuple > 0 && third threeTuple == 0 = 0 
    | first threeTuple == 'R' && second threeTuple < 0 && third threeTuple == 0 = pi
    | first threeTuple == 'P' && third threeTuple > 0 = third threeTuple
    | otherwise = error "Invalid format. Correct format:\n \t('R' or 'P', float, float), R = Rectangular, P = Polar, with positive angle"

-- QUESTION 3: