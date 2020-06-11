module SumOfMultiplesNaive exposing (sumOfMultiples)

divisibleByAny : List Int -> Int -> Bool
divisibleByAny divisors number =
    List.any (\div -> remainderBy div number == 0) divisors


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    List.range 1 (limit - 1)
        |> List.filter (divisibleByAny divisors)
        |> List.sum
