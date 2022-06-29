-- IMPORT LIBRARY -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
import Data.Time
import Data.Char
import System.IO
import Control.Exception
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- FORMULA ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- RULESBASE -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rulesBase :: String -> Double -> Double -> Double -> Double -> String
rulesBase businessSector stockPrice roe der intrinsicValue
    | businessSector == "FINANCE" = rulesBaseCheck stockPrice roe der intrinsicValue
    | otherwise                   = rulesBaseCheck' stockPrice roe der intrinsicValue

rulesBaseCheck :: Double -> Double -> Double -> Double -> String
rulesBaseCheck stockPrice roe der intrinsicValue 
    | (roe >= 10.0) && (der <= 8.0) && (stockPrice < intrinsicValue)         = "Advice: Recommended investment, Reason: Ideal Condition, undervalue, and good performance"
    | (roe > 0 && roe < 10) && (der <= 8.0) && (stockPrice < intrinsicValue) = "Advice: Recommended investment, Reason: Undervalue even though profitability is not so good"
    | (roe < 0) && (der <= 8.0) && (stockPrice < intrinsicValue)             = "Advice: Watchlist, Reason: Undervalue with bad profitability"
    | (roe >= 10.0) && (der > 8.0) && (stockPrice < intrinsicValue)          = "Advice: Watchlist, Reason: Undervalue with high debt ratio"
    | (roe > 0 && roe < 10) && (der > 8.0) && (stockPrice < intrinsicValue)  = "Advice: Watchlist, Reason: Undervalue but profitability is not good and high debt ratio"
    | (roe < 0) && (der > 8.0) && (stockPrice < intrinsicValue)              = "Advice: Not recommended investment, Reason: Undervalue with bad profitabiltiy and high debt ratio"
    | (roe >= 10.0) && (der <= 8.0) && (stockPrice > intrinsicValue)         = "Advice: Watchlist, Reason: Good performance but not undervalue"
    | (roe < 0) && (der > 8.0) && (stockPrice > intrinsicValue)              = "Advice: Not recommended investment, Reason: Worst Condition, overvalue, and bad performance"
    | otherwise                                                              = "Advice: Not recommended investment, Reason: Not good condition or bad performance"

rulesBaseCheck' :: Double -> Double -> Double -> Double -> String
rulesBaseCheck' stockPrice roe der intrinsicValue 
    | (roe >= 10.0) && (der <= 2.0) && (stockPrice < intrinsicValue)         = "Advice: Recommended investment, Reason: Ideal Condition, undervalue, and good performance"
    | (roe > 0 && roe < 10) && (der <= 2.0) && (stockPrice < intrinsicValue) = "Advice: Recommended investment, Reason: Undervalue even though profitability is not so good"
    | (roe < 0) && (der <= 2.0) && (stockPrice < intrinsicValue)             = "Advice: Watchlist, Reason: Undervalue with bad profitability"
    | (roe >= 10.0) && (der > 2.0) && (stockPrice < intrinsicValue)          = "Advice: Watchlist, Reason: Undervalue with high debt ratio"
    | (roe > 0 && roe < 10) && (der > 2.0) && (stockPrice < intrinsicValue)  = "Advice: Watchlist, Reason: Undervalue but profitability is not good and high debt ratio"
    | (roe < 0) && (der > 2.0) && (stockPrice < intrinsicValue)              = "Advice: Not recommended investment, Reason: Undervalue with bad profitabiltiy and high debt ratio"
    | (roe >= 10.0) && (der <= 2.0) && (stockPrice > intrinsicValue)         = "Advice: Watchlist, Reason: Good performance but not undervalue"
    | (roe < 0) && (der > 2.0) && (stockPrice > intrinsicValue)              = "Advice: Not recommended investment, Reason: Worst Condition, overvalue, and bad performance"
    | otherwise                                                              = "Advice: Not recommended investment, Reason: Not good condition or bad performance"
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- EXECUTEABLE FUNCTIONS ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = login

login :: IO ()
login =  do 
         putStrLn ("\n-- Welcome to Value Investor Calculator Application --")
         putStrLn ("Login with your investor account")
         putStr ("Username: ")
         username <- getLine
         putStr ("Password: ")
         password <- withEcho False getLine
         putChar '\n'
         checkLogin username password

checkLogin :: String -> String -> IO ()
checkLogin username password 
    | (username == "Valdryan Ivandito") && (password == "kucingterbang@618") = succeedLogin 
    | otherwise                                                              = reLogin

succeedLogin :: IO ()
succeedLogin = do 
               timeStamp <- getZonedTime
               let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
               appendFile "log_file.txt" (timeStampResult ++ ";" ++ "LOGIN" ++ ";" ++ "User succeed login " ++ "\n")
               menu

reLogin :: IO ()
reLogin = do  
          timeStamp <- getZonedTime
          let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
          appendFile "log_file.txt" (timeStampResult ++ ";" ++ "FAILED" ++ ";" ++ "User failed login " ++ "\n")
          putStrLn ("\nInvalid Username or Password!")
          putStrLn ("Please choose the option : (1) -> Try Again ; (Press any button) -> Quit")
          putStr ("Option: ")
          option <- getLine
          case option of
               "1" -> login 
               _   -> return ()              

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

menu :: IO ()
menu = do 
       putStrLn ("\nWelcome Value-Investor, Can I help you ?")
       putStrLn ("Please choose the option : (1) Input Data ; (2) Read Data ; (3) Delete Data ; (4) Logout")
       putStr ("Option: ")
       option <- getLine
       case option of
            "1" -> inputData
            "2" -> readData
            "4" -> do 
                   putStrLn ("\nSee you again... happy investing :)\n")
                   timeStamp <- getZonedTime
                   let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                   appendFile "log_file.txt" (timeStampResult ++ ";" ++ "LOGOUT" ++ ";" ++ "User logged out " ++ "\n")
                   return ()
            _   -> menu
 
inputData :: IO ()
inputData = do 
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

            putStrLn ("\n" ++ (fmap toUpper companyName) ++ " (" ++ (fmap toUpper stockCode) ++ ")" ++ " Result: ")
            putStrLn ("BusinessSector: " ++ (fmap toUpper businessSector))
            putStrLn ("ROE(%): " ++ show (round' roeResult))
            putStrLn ("DER(%): " ++ show (round' derResult))
            putStrLn ("BV: " ++ show (round bvResult))
            putStrLn ("PBV: " ++ show (round' pbvResult))
            putStrLn ("EPS: " ++ show (round' epsResult))
            putStrLn ("PER: " ++ show (round' perResult))
            putStrLn ("Intrinsic Value: " ++ show (round intrinsicValueResult))
            putStrLn ("MOSPrice: " ++ show (round mosPriceResult))
            putStrLn (conclusion)
            appendFile "database_file.txt" (timeStampResult ++ ";" ++ (fmap toUpper companyName) ++ ";" ++ (fmap toUpper stockCode) ++ ";" ++ (fmap toUpper businessSector) ++ ";" ++ show (round' roeResult) ++ ";" ++ show (round' derResult) ++ ";" ++ show (round bvResult) ++ ";" ++ show (round' pbvResult) ++ ";" ++ show (round' epsResult) ++ ";" ++ show (round' perResult) ++ ";" ++ show (round intrinsicValueResult) ++ ";" ++ show (round mosPriceResult) ++ ";" ++ conclusion ++ "\n")
            appendFile "read_file.txt" (timeStampResult ++ "\n" ++ (fmap toUpper companyName) ++ " (" ++ (fmap toUpper stockCode) ++ ")" ++ " Result: " ++ "\n" ++ "BusinessSector: " ++ (fmap toUpper businessSector) ++ "\n" ++ "ROE(%): " ++ show (round' roeResult) ++ "\n" ++ "DER(%): " ++ show (round' derResult) ++ "\n" ++ "BV: " ++ show (round bvResult) ++ "\n" ++ "PBV: " ++ show (round' pbvResult) ++ "\n" ++ "EPS: " ++ show (round' epsResult) ++ "\n" ++ "PER: " ++ show (round' perResult) ++ "\n" ++ "Intrinsic Value: " ++ show (round intrinsicValueResult) ++ "\n" ++ "MOSPrice: " ++ show (round mosPriceResult) ++ "\n" ++ conclusion ++ "\n")
            appendFile "log_file.txt" (timeStampResult ++ ";" ++ "CREATE" ++ ";" ++ "Create data " ++ (fmap toUpper stockCode) ++ "\n")
            menu

readData :: IO ()
readData = do 
           database <- readFile "./read_file.txt"
           print database
           menu
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
