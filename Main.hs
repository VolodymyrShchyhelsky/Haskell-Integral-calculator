import Integral

custom_function :: Double -> Double
custom_function x = (x + 1) * (x + 1)

main = do 
      print $ integral (1,2) custom_function
