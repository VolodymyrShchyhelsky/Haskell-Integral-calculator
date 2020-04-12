import Integral
import Test.Tasty         (defaultMain, testGroup)
import Test.Tasty.HUnit   (assertBool, testCase)

main = defaultMain tests

tests = testGroup "Integral tests" [test1, test2, test3]

func1 :: Double -> Double
func1 x = x * x + sin(x) * sin(x)

func2 :: Double -> Double
func2 x = 1 / cos(x)

func3 :: Double -> Double
func3 x = (x + 1) * (x + 1)

test1 =
    testCase "Integrating 1 / cos(x) from 0 to 1" $ do
    let eps = 0.00001
    let res = integral (0, 1) func2
    let res_str = show $ res
    let res_output = "Expected value : 1.2262 \n received : " ++ res_str 
    assertBool res_output ((abs (res - 1.2262)) < eps)

test2 =
    testCase "Integrating x * x + sin(x)^3 from 0 to 1" $ do
    let eps = 0.0001
    let res = integral (0, 1) func1
    let res_str = show $ res
    let res_output = "Expected value : 0.606 \n received : " ++ res_str 
    assertBool res_output ((abs (res - 0.606)) < eps)

test3 =
    testCase "Integrating (x + 1) ^ 2 from 5 to 10" $ do
    let eps = 0.000001
    let res = integral (5, 10) func3
    let res_str = show $ res
    let res_output = "Expected value : 371.6666 \n received : " ++ res_str 
    assertBool res_output ((abs (res - 371.666666)) < eps)