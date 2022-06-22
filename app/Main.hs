{-module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"-}

roe :: Double -> Double -> Double
roe netProfit equity = (netProfit / equity) * 100


der :: Double -> Double -> Double 
der liability equity = liability / equity


pbv :: Double -> Double -> Double
pbv stockPrice bv = stockPrice / bv

bv :: Double -> Double -> Double
bv equity stockShares = equity / stockShares

pbv' :: Double -> Double -> Double -> Double
pbv' stockPrice equity stockShares = stockPrice / (bv equity stockShares)


per :: Double -> Double -> Double
per stockPrice eps = stockPrice / eps

eps :: Double -> Double -> Double 
eps netProfit stockShares = netProfit / stockShares

per' :: Double -> Double -> Double -> Double
per' stockPrice netProfit stockShares = stockPrice / (eps netProfit stockShares)


eps10 :: Double -> Double -> [Double]
eps10 eps cagr = take 11 (eps : eps10 (eps * (cagr / 100) + eps) cagr)

totalEPS10 :: Double -> Double -> Double
totalEPS10 eps cagr = foldr (+) 0 (take 10 (reverse (eps10 eps cagr)))

totalEPS10' :: Double -> Double -> Double -> Double
totalEPS10' eps cagr inflation = (totalEPS10 eps cagr) - ((totalEPS10 eps cagr) * (inflation / 100))

intrinsicValue :: Double -> Double -> Double -> Double -> Double
intrinsicValue bv eps cagr inflation = round' (bv + (totalEPS10' eps cagr inflation))

mosPrice :: Double -> Double -> Double -> Double -> Double -> Double
mosPrice bv eps cagr inflation mos = round' ((intrinsicValue bv eps cagr inflation) * (1 - (mos / 100)))

round' :: Double -> Double
round' num = (fromIntegral . round $ num * f) / f
    where f = 10 ^ 2

main :: IO ()
main = do 
       putStr "StockCode: "
       stockCode <- getLine
       putStr "StockPrice: "
       stockPrice <- getLine
       putStr "StockShares: "
       stockShares <- getLine
       putStr "Liability: "
       liability <- getLine
       putStr "Equity: "
       equity <- getLine
       putStr "NetProfit: "
       netProfit <- getLine
       putStr "Inflation_10Years(%): "
       inflation <- getLine
       putStr "CAGR(%): "
       cagr <- getLine
       putStr "MOS(%): "
       mos <- getLine

       let roeResult = roe (read netProfit) (read equity)
       let derResult = der (read liability) (read equity)
       let bvResult  = bv  (read equity) (read stockShares)
       let pbvResult = pbv (read stockPrice) (bvResult)
       let epsResult = eps (read netProfit) (read stockShares)
       let perResult = per (read stockPrice) (epsResult)
       let intrinsicValueResult = intrinsicValue (bvResult) (epsResult) (read cagr) (read inflation)
       let mosPriceResult = mosPrice (bvResult) (epsResult) (read cagr) (read inflation) (read mos)

       putStrLn("\n" ++ stockCode ++ " RESULT: ")  
       putStrLn("ROE(%): " ++ show roeResult)
       putStrLn("DER(%): " ++ show derResult)
       putStrLn("BV: " ++ show bvResult)
       putStrLn("PBV: " ++ show pbvResult)
       putStrLn("EPS: " ++ show epsResult)
       putStrLn("PER: " ++ show perResult)
       putStrLn("Intrinsic Value: " ++ show intrinsicValueResult)
       putStrLn("MOSPrice: " ++ show mosPriceResult)