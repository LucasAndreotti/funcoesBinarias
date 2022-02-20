-- PROGRAMAÇÃO FUNCIONAL
-- TRABALHO 1
-- GABRIELA ZORZO E LUCAS ANDREOTTI

-- 1. Definir uma função recursiva que recebe um número binário e retorna o valor equivalente em decimal natural.
bin2dec :: [Int] -> Int
bin2dec = foldl (\acc x -> x + 2*acc) 0
-- exemplo de entrada: bin2dec [1,0,1,0]
-- saída: 10

-- 2. Definir uma função recursiva que recebe um número decimal natural e retorna o valor equivalente em binário. 
dec2bin :: Int -> [Int]
dec2bin 0 = [0]
dec2bin 1 = [1]
dec2bin x = dec2bin ( x `quot` 2 ) ++ [ x `mod` 2 ]
-- exemplo de entrada: dec2bin 10
-- saída: [1,0,1,0]

-- 3. Definir uma função recursiva que recebe um número binário em complemento de dois e retorna o valor equivalente em decimal inteiro.
bincompl2dec :: [Int] -> Int
bincompl2dec (x:xs) = - x * 2 ^ ( length (x:xs) - 1 ) + bin2dec xs
-- exemplo de entrada: bincompl2dec [1,0,1,1,0]
-- saída: -10

-- 4. Definir uma função recursiva que recebe um número decimal inteiro e retorna o valor equivalente em binário complemento de dois.
dec2bincompl :: Int -> [Int]
dec2bincompl 0 = [0]
dec2bincompl (-1) = [1]
dec2bincompl x = if x < 0 then ( dec2bincomplneg x ) 
                 else ( [0] ++ dec2bin x )

dec2bincomplneg :: Int -> [Int]
dec2bincomplneg x = ( dec2bin ( ( bin2dec ( [1] ++ mapinv2bin (modulo x) ) ) + 1 ) ) 

invert :: Int -> Int
invert x = if x == 0 then 1
           else 0

mapinv2bin:: Int -> [Int]
mapinv2bin x = map invert ( dec2bin x )

modulo :: Int -> Int
modulo x = if x < 0 then (-x)
           else x
-- exemplo de entrada: dec2bincompl (-10)
-- saída: [1,0,1,1,0]

-- 5. Definir uma função recursiva que recebe dois números binários em complemento de dois e retorna a soma binária destes dois valores. 
somarbin :: [Int] -> [Int] -> [Int]
somarbin (x:xs) (y:ys) = dec2bincompl ( bincompl2dec (x:xs) + bincompl2dec (y:ys) )
-- exemplo de entrada: somarbin [1,0,1,0] [1,0,1,1]
-- saída: [1,0,1,0,1]

-- 6. Definir  uma  função  recursiva  que  recebe  dois  números  binários  em  complemento  de  dois  e  retorna  a subtração binária destes dois valores.
subtrairbin :: [Int] -> [Int] -> [Int]
subtrairbin (x:xs) (y:ys) = dec2bincompl ( bincompl2dec (x:xs) - bincompl2dec (y:ys) )
-- exemplo de entrada: subtrairbin [1,0,1,0] [1,0,1]
-- saída: [1,0,1]

-- 7. Definir uma função recursiva que recebe dois números binários e retorna a conjunção lógica bit a bit entre esses dois valores.
andbin :: [Int] -> [Int] -> [Int]
andbin [] [] = []
andbin (x:xs) (y:ys) = [( andbin' x y )] ++ andbin xs ys

andbin' :: Int -> Int -> Int
andbin' x y = if x == 1 && y == 1 then 1
              else 0
-- exemplo de entrada: andbin [1,0,1,0] [1,0,0,1]
-- saída: [1,0,0,0]

-- 8. Definir uma função recursiva que recebe dois números binários e retorna a disjunção lógica bit a bit entre esses dois valores.
orbin :: [Int] -> [Int] -> [Int]
orbin [] [] = []
orbin (x:xs) (y:ys) = [( orbin' x y )] ++ orbin xs ys

orbin' :: Int -> Int -> Int
orbin' x y = if x == 1 || y == 1 then 1
             else 0
-- exemplo de entrada: orbin [1,0,1,0] [1,0,0,1]
-- saída: [1,0,1,1]

-- 9. Definir uma função recursiva que recebe um número fracionário decimal por parâmetro e devolve uma tupla com dois números binários representando, respectivamente,a mantissa e o expoente para representação binária da fração(𝑚𝑎𝑛𝑡𝑖𝑠𝑠𝑎×10𝑒𝑥𝑝𝑜𝑒𝑛𝑡𝑒).
frac2bin :: Double -> ([Int],[Int])
frac2bin x = ( mantissa2bin x, expoente2bin x )

mantissa2bin :: Double -> [Int]
mantissa2bin x = dec2bin ( doubleToInt ( x * ( 10 ^ ( length ( parteDecimal x ) ) ) ) )

expoente2bin :: Double -> [Int]
expoente2bin x = dec2bin ( length ( parteInteira x ) )

parteInteira :: Double -> [Char]
parteInteira x = takeWhile ('.'<) (show x)

parteDecimal :: Double -> [Char]
parteDecimal x = dropWhile (=='.') ( dropWhile ('.'<) (show x) )

doubleToInt :: Double -> Int
doubleToInt x = round x :: Int
-- exemplo de entrada: frac2bin 10.11
-- saída: ([1,1,1,1,1,1,0,0,1,1],[1,0])

-- 10. Definir uma função recursiva que recebe uma tupla com dois números binários representando, respectivamente, a mantissa e o expoente de um número binário fracionário(𝑚𝑎𝑛𝑡𝑖𝑠𝑠𝑎×10𝑒𝑥𝑝𝑜𝑒𝑛𝑡𝑒), e retorna o correspondente valor fracionário decimal.
bin2frac :: ([Int],[Int]) -> Double
bin2frac (x,y) = ( mantissa2double ( bin2dec x ) ) * ( 10 ^ ( bin2dec y ) )

mantissa2double :: Int -> Double
mantissa2double x = ( fromIntegral x ) / ( 10 ^ ( length (show x ) ) )
-- exemplo de entrada: bin2frac ([1,1,1,1,1,1,0,0,1,1],[1,0])
-- saída: 10.11