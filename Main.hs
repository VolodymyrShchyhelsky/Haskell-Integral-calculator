import Control.Parallel
import Database.MySQL.Base
import Debug.Trace

type Func = Double -> Double

f_out :: Double -> IO(Double) 
f_out x = do
      print((x,x))
      return(x)
      
custom_function :: Double -> Double
custom_function x = (x + 1) * (x + 1)

calcForRange :: (Double, Double) -> Func -> Double
calcForRange (x1, x2) f = 2.0 * (f ((x1+x2) / 2.0))

partitionIterator :: [Double] -> Func -> [Double]
partitionIterator (x1:x2:other) f = par first_elem (seq other_list (first_elem : other_list))
      where other_list = partitionIterator (x2:other) f
            first_elem = calcForRange (x1,x2) f
partitionIterator [x1] _ = []

calcOnRanges :: [Double] -> Func -> Double
calcOnRanges partition f = sum (partitionIterator partition f)
      
createPartition :: [Double] -> [Double]
createPartition (x1:x2:other) = [x1, (x2 + x1) / 2.0] ++ (createPartition (x2:other))
createPartition (x1:_) = [x1]

integralIteration :: Double -> Double -> [Double] -> Func -> Double -> Double
integralIteration prev_sum lastn partition f prec
      | prec > abs( prev_sum - curr_sum ) = curr_sum
      | otherwise       = integralIteration curr_sum n (createPartition partition) f prec
      where
            n = lastn * 2.0
            curr_sum = prev_sum / 2.0 + (calcOnRanges partition f) / n

integral :: (Double, Double) -> Func -> Double
integral (a, b) f = let prec = 0.0000001
                    in ((b - a) / 2.0) * (integralIteration (f a + f b) 1.0 [a, b] f prec)

main = do 
      print $ integral (1,2) custom_function
