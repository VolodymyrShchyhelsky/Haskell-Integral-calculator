# shdimir-gmail.com
Haskell integral calculator
Compilation: <br>
cabal install --only-dependencies <br>
cabal new-configure --enable-tests <br>
cabal new-build <br>
For test running: <br>
cabal new-test <br>
For execution: <br>
cabal run <br>
Flags for running on multicores: <br>
+RTS -Nx (where N = number of cores) <br>