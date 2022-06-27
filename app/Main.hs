import Data.Time
import Data.Char

netProfit' :: Int -> Double -> Double
netProfit' quartal netProfit
    | quartal == 1 = (netProfit / 3 * 12)
    | quartal == 2 = (netProfit / 6 * 12)
    | quartal == 3 = (netProfit / 9 * 12)
    | quartal == 4 = netProfit

roe :: Double -> Double -> Double
roe netProfit equity = (netProfit / equity) * 100

der :: Double -> Double -> Double 
der liability equity = liability / equity

bv :: Double -> Double -> Double
bv equity stockShares = equity / stockShares

pbv :: Double -> Double -> Double
pbv stockPrice bv = stockPrice / bv

eps :: Double -> Double -> Double 
eps netProfit stockShares = netProfit / stockShares

per :: Double -> Double -> Double
per stockPrice eps = stockPrice / eps

eps10 :: Double -> Double -> [Double]
eps10 eps cagr = take 11 (eps : eps10 (eps * (cagr / 100) + eps) cagr)

totalEPS10 :: Double -> Double -> Double
totalEPS10 eps cagr = foldr (+) 0 (take 10 (reverse (eps10 eps cagr)))

totalEPS10' :: Double -> Double -> Double -> Double
totalEPS10' eps cagr inflation = (totalEPS10 eps cagr) - ((totalEPS10 eps cagr) * (inflation / 100))

intrinsicValue :: Double -> Double -> Double -> Double -> Double
intrinsicValue bv eps cagr inflation = bv + (totalEPS10' eps cagr inflation)

mosPrice :: Double -> Double -> Double -> Double -> Double -> Double
mosPrice bv eps cagr inflation mos = (intrinsicValue bv eps cagr inflation) * (1 - (mos / 100))

round' :: Double -> Double
round' num = (fromIntegral . round $ num * f) / f
    where f =  10 -- 10 ^ 2

rulesBase :: String -> Double -> Double -> Double -> Double -> String
rulesBase businessSector stockPrice roe der intrinsicValue
    | businessSector == "FINANCE" = rulesBaseCheck stockPrice roe der intrinsicValue
    | otherwise                   = rulesBaseCheck' stockPrice roe der intrinsicValue

rulesBaseCheck :: Double -> Double -> Double -> Double -> String
rulesBaseCheck stockPrice roe der intrinsicValue 
    | (roe >= 10.0) && (der <= 8.0) && (stockPrice < intrinsicValue)         = "Advice: Recommended investment, Reason: Ideal Condition, undervalue, and good performance\n"
    | (roe > 0 && roe < 10) && (der <= 8.0) && (stockPrice < intrinsicValue) = "Advice: Recommended investment, Reason: Undervalue even though profitability is not so good\n"
    | (roe < 0) && (der <= 8.0) && (stockPrice < intrinsicValue)             = "Advice: Watchlist, Reason: Undervalue with bad profitability\n"
    | (roe >= 10.0) && (der > 8.0) && (stockPrice < intrinsicValue)          = "Advice: Watchlist, Reason: Undervalue with high debt ratio\n"
    | (roe > 0 && roe < 10) && (der > 8.0) && (stockPrice < intrinsicValue)  = "Advice: Watchlist, Reason: Undervalue but profitability is not good and high debt ratio\n"
    | (roe < 0) && (der > 8.0) && (stockPrice < intrinsicValue)              = "Advice: Not recommended investment, Reason: Undervalue with bad profitabiltiy and high debt ratio\n"
    | (roe >= 10.0) && (der <= 8.0) && (stockPrice > intrinsicValue)         = "Advice: Watchlist, Reason: Good performance but not undervalue\n"
    | (roe < 0) && (der > 8.0) && (stockPrice > intrinsicValue)              = "Advice: Not recommended investment, Reason: Worst Condition, overvalue, and bad performance\n"
    | otherwise                                                              = "Advice: Not recommended investment, Reason: Not good condition or bad performance\n"

rulesBaseCheck' :: Double -> Double -> Double -> Double -> String
rulesBaseCheck' stockPrice roe der intrinsicValue 
    | (roe >= 10.0) && (der <= 2.0) && (stockPrice < intrinsicValue)         = "Advice: Recommended investment, Reason: Ideal Condition, undervalue, and good performance\n"
    | (roe > 0 && roe < 10) && (der <= 2.0) && (stockPrice < intrinsicValue) = "Advice: Recommended investment, Reason: Undervalue even though profitability is not so good\n"
    | (roe < 0) && (der <= 2.0) && (stockPrice < intrinsicValue)             = "Advice: Watchlist, Reason: Undervalue with bad profitability\n"
    | (roe >= 10.0) && (der > 2.0) && (stockPrice < intrinsicValue)          = "Advice: Watchlist, Reason: Undervalue with high debt ratio\n"
    | (roe > 0 && roe < 10) && (der > 2.0) && (stockPrice < intrinsicValue)  = "Advice: Watchlist, Reason: Undervalue but profitability is not good and high debt ratio\n"
    | (roe < 0) && (der > 2.0) && (stockPrice < intrinsicValue)              = "Advice: Not recommended investment, Reason: Undervalue with bad profitabiltiy and high debt ratio\n"
    | (roe >= 10.0) && (der <= 2.0) && (stockPrice > intrinsicValue)         = "Advice: Watchlist, Reason: Good performance but not undervalue\n"
    | (roe < 0) && (der > 2.0) && (stockPrice > intrinsicValue)              = "Advice: Not recommended investment, Reason: Worst Condition, overvalue, and bad performance\n"
    | otherwise                                                              = "Advice: Not recommended investment, Reason: Not good condition or bad performance\n"

main :: IO ()
main = do 
       timeStamp <- getZonedTime
       putStr "CompanyName: "
       companyName <- getLine
       putStr "BusinessSector: "
       businessSector <- getLine
       putStr "StockCode: "
       stockCode <- getLine
       putStr "StockPrice: "
       stockPrice <- getLine
       putStr "StockShares: "
       stockShares <- getLine
       putStr "Quartal: "
       quartal <- getLine
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

       let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
       let netProfitYear = netProfit' (read quartal) (read netProfit)
       let roeResult = roe (netProfitYear) (read equity)
       let derResult = der (read liability) (read equity)
       let bvResult  = bv  (read equity) (read stockShares)
       let pbvResult = pbv (read stockPrice) (bvResult)
       let epsResult = eps (netProfitYear) (read stockShares)
       let perResult = per (read stockPrice) (epsResult)
       let intrinsicValueResult = intrinsicValue (bvResult) (epsResult) (read cagr) (read inflation)
       let mosPriceResult = mosPrice (bvResult) (epsResult) (read cagr) (read inflation) (read mos)
       let conclusion = rulesBase (fmap toUpper businessSector) (read stockPrice) (roeResult) (derResult) (intrinsicValueResult) 

       putStrLn("\n" ++ (fmap toUpper companyName) ++ " (" ++ (fmap toUpper stockCode) ++ ")" ++ " Result: ")
       putStrLn("BusinessSector: " ++ (fmap toUpper businessSector))
       putStrLn("ROE(%): " ++ show (round' roeResult))
       putStrLn("DER(%): " ++ show (round' derResult))
       putStrLn("BV: " ++ show (round bvResult))
       putStrLn("PBV: " ++ show (round' pbvResult))
       putStrLn("EPS: " ++ show (round' epsResult))
       putStrLn("PER: " ++ show (round' perResult))
       putStrLn("Intrinsic Value: " ++ show (round intrinsicValueResult))
       putStrLn("MOSPrice: " ++ show (round mosPriceResult))
       putStrLn(conclusion)
       appendFile "database.txt" ((fmap toUpper companyName) ++ ";" ++ (fmap toUpper stockCode) ++ ";" ++ (fmap toUpper businessSector) ++ ";" ++ show (round' roeResult) ++ ";" ++ show (round' derResult) ++ ";" ++ show (round bvResult) ++ ";" ++ show (round' pbvResult) ++ ";" ++ show (round' epsResult) ++ ";" ++ show (round' perResult) ++ ";" ++ show (round intrinsicValueResult) ++ ";" ++ show (round mosPriceResult) ++ ";" ++ conclusion)
       appendFile "log_activity.txt" (timeStampResult ++ ";" ++ "CREATE DATA" ++ ";" ++ (fmap toUpper stockCode) ++ "\n")