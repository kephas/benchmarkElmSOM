module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import SumOfMultiplesLcm as Lcm
import SumOfMultiplesNaive as Naive


doSumOfMultiples implementation =
    \_ -> implementation [ 2, 4, 6, 9, 24 ] 200000


suite =
    describe "SumOfMultiples"
        [ benchmark "naive implementation" <|
            doSumOfMultiples Naive.sumOfMultiples
        , benchmark "LCM implementation" <|
            doSumOfMultiples Lcm.sumOfMultiples
        ]


main : BenchmarkProgram
main =
    program suite
