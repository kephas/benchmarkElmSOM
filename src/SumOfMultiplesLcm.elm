module SumOfMultiplesLcm exposing (sumOfMultiples)

import Arithmetic

-- calculate Least Common Multiple (LCM)
lcm : Int -> Int -> Int
lcm a b =
    if b > 0 then
        -(Arithmetic.lcm a b)
    else
        Arithmetic.lcm a -b

-- compute all LCMs between a number and a list of numbers
computeLCMs : Int -> List Int -> List Int -> List Int
computeLCMs number list acc =
    case list of
        [] ->
            acc
        other::others ->                   
            computeLCMs number others (acc ++ [lcm number other])

-- add LCMs of divisors to the list of divisors (could be optimized by removing +/- pairs)
addLCMs : List Int -> List Int -> List Int               
addLCMs numbers acc =
    let
        aggregateLCMs number list =
            list ++ [number] ++ (computeLCMs number list [])
    in 
        case numbers of
            [] ->
                acc
            n::others ->
                addLCMs others (aggregateLCMs n acc)

-- add sums of divisors and their LCMs multiples
sumMyMultiples : List Int -> Int -> Int
sumMyMultiples numbers limit =    
    case numbers of
        [] ->
            0
        a::others ->
            let
               n = (limit - 1) // (abs a)
            in
               (a * n * (n + 1) // 2) + (sumMyMultiples others limit)
         
sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    sumMyMultiples (addLCMs divisors []) limit
                   
