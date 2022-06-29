-- IMPORT LIBRARY -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
import Control.Monad.State (StateT, MonadState (get), MonadTrans (lift), execStateT)
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
executeState :: StateT String IO ()
executeState = do 
               state <- get
               lift (putStr (state))

main :: IO ()
main = login "Value Investor"

login :: String -> IO ()
login username = do 
                 execStateT executeState ("\n-- Welcome to Value Investor Calculator Application --\n")
                 execStateT executeState ("Login with your investor account\n")
                 execStateT executeState ("Username: ")
                 username <- getLine
                 execStateT executeState ("Password: ")
                 password <- withEcho False getLine
                 execStateT executeState ("\n")
                 checkLogin username password

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
                       old <- hGetEcho stdin
                       bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

checkLogin :: String -> String -> IO ()
checkLogin username password 
    | (username == "Valdryan Ivandito") && (password == "kucingterbang@618") = succeedLogin username
    | otherwise                                                              = reLogin username

succeedLogin :: String -> IO ()
succeedLogin username = do 
                        timeStamp <- getZonedTime
                        let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                        appendFile "log_file.txt" (timeStampResult ++ ";" ++ "LOGIN" ++ ";" ++ username ++ " succeed login" ++ "\n")
                        menu username

reLogin :: String -> IO ()
reLogin username = do  
                   timeStamp <- getZonedTime
                   let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                   appendFile "log_file.txt" (timeStampResult ++ ";" ++ "FAILED" ++ ";" ++ username ++ " failed login" ++ "\n")
                   execStateT executeState ("\nInvalid Username or Password!\n")
                   execStateT executeState ("Please choose the option : (1) -> Try Again ; (Press any button) -> Quit\n")
                   execStateT executeState ("Option: ")
                   option <- getLine
                   case option of
                        "1" -> login username
                        _   -> return ()              

menu :: String -> IO ()
menu username = do 
                execStateT executeState ("\nWelcome Value-Investor, Can I help you ?\n")
                execStateT executeState ("Please choose the option : (1) -> Input Data ; (2) -> Load Data ; (3) -> Read Log ; (Press any button) -> Logout\n")
                execStateT executeState ("Option: ")
                option <- getLine
                case option of
                     "1" -> inputData username
                     "2" -> readData username
                     "3" -> readLog username
                     _   -> do 
                            timeStamp <- getZonedTime
                            let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                            appendFile "log_file.txt" (timeStampResult ++ ";" ++ "LOGOUT" ++ ";" ++ username ++ " logged out" ++ "\n")
                            execStateT executeState ("\nSee you again... happy investing :)\n")
                            return ()
 
inputData :: String -> IO ()
inputData username = do 
                     timeStamp <- getZonedTime
                     execStateT executeState ("CompanyName: ")
                     companyName <- getLine
                     execStateT executeState ("BusinessSector: ")
                     businessSector <- getLine
                     execStateT executeState ("StockCode: ")
                     stockCode <- getLine
                     execStateT executeState ("StockPrice: ")
                     stockPrice <- getLine
                     execStateT executeState ("StockShares: ")
                     stockShares <- getLine
                     execStateT executeState ("Quartal: ")
                     quartal <- getLine
                     execStateT executeState ("Liability: ")
                     liability <- getLine
                     execStateT executeState ("Equity: ")
                     equity <- getLine
                     execStateT executeState ("NetProfit: ")
                     netProfit <- getLine
                     execStateT executeState ("Inflation_10Years(%): ")
                     inflation <- getLine
                     execStateT executeState ("CAGR(%): ")
                     cagr <- getLine
                     execStateT executeState ("MOS(%): ")
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

                     execStateT executeState ("\n" ++ (fmap toUpper companyName) ++ " (" ++ (fmap toUpper stockCode) ++ ")" ++ " Result: " ++ "\n")
                     execStateT executeState ("BusinessSector: " ++ (fmap toUpper businessSector) ++ "\n")
                     execStateT executeState ("ROE(%): " ++ show (round' roeResult) ++ "\n")
                     execStateT executeState ("DER(%): " ++ show (round' derResult) ++ "\n")
                     execStateT executeState ("BV: " ++ show (round bvResult) ++ "\n")
                     execStateT executeState ("PBV: " ++ show (round' pbvResult) ++ "\n")
                     execStateT executeState ("EPS: " ++ show (round' epsResult) ++ "\n")
                     execStateT executeState ("PER: " ++ show (round' perResult) ++ "\n")
                     execStateT executeState ("Intrinsic Value: " ++ show (round intrinsicValueResult) ++ "\n")
                     execStateT executeState ("MOSPrice: " ++ show (round mosPriceResult) ++ "\n")
                     execStateT executeState (conclusion ++ "\n")
                     
                     appendFile "read_file.txt" (timeStampResult ++ "\n" ++ (fmap toUpper companyName) ++ " (" ++ (fmap toUpper stockCode) ++ ")" ++ " Result: " ++ "\n" ++ "BusinessSector: " ++ (fmap toUpper businessSector) ++ "\n" ++ "ROE(%): " ++ show (round' roeResult) ++ "\n" ++ "DER(%): " ++ show (round' derResult) ++ "\n" ++ "BV: " ++ show (round bvResult) ++ "\n" ++ "PBV: " ++ show (round' pbvResult) ++ "\n" ++ "EPS: " ++ show (round' epsResult) ++ "\n" ++ "PER: " ++ show (round' perResult) ++ "\n" ++ "Intrinsic Value: " ++ show (round intrinsicValueResult) ++ "\n" ++ "MOSPrice: " ++ show (round mosPriceResult) ++ "\n" ++ conclusion ++ "\n" ++ "\n")
                     appendFile "database_file.txt" (timeStampResult ++ ";" ++ (fmap toUpper companyName) ++ ";" ++ (fmap toUpper stockCode) ++ ";" ++ (fmap toUpper businessSector) ++ ";" ++ show (round' roeResult) ++ ";" ++ show (round' derResult) ++ ";" ++ show (round bvResult) ++ ";" ++ show (round' pbvResult) ++ ";" ++ show (round' epsResult) ++ ";" ++ show (round' perResult) ++ ";" ++ show (round intrinsicValueResult) ++ ";" ++ show (round mosPriceResult) ++ ";" ++ conclusion ++ "\n")
                     appendFile "log_file.txt" (timeStampResult ++ ";" ++ "CREATE" ++ ";" ++ username ++ " created " ++ (fmap toUpper stockCode) ++ " data" ++ "\n")

                     menu username

readData :: String -> IO ()
readData username = do 
                    timeStamp <- getZonedTime
                    let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                    appendFile "log_file.txt" (timeStampResult ++ ";" ++ "READ" ++ ";" ++ username ++ " load all data " ++ "\n")
                    allData <- readFile "./read_file.txt"
                    execStateT executeState (allData ++ "\n")
                    menu username

readLog :: String -> IO ()
readLog username = do 
                    timeStamp <- getZonedTime
                    let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                    appendFile "log_file.txt" (timeStampResult ++ ";" ++ "READ" ++ ";" ++ username ++ " read all log " ++ "\n")
                    allData <- readFile "./log_file.txt"
                    execStateT executeState (allData ++ "\n")
                    menu username
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------