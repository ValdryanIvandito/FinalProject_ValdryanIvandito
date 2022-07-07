-- IMPORT LIBRARY -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
import Control.Monad.Trans.State (StateT, state, evalStateT, runStateT, get, modify, put, execStateT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import GHC.IO.Handle (hGetEcho, hSetEcho)
import GHC.IO.Handle.FD (stdin)
import Control.Exception (bracket_)
import Data.Char (toUpper)
import Data.Time
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
main = do
       execStateT executeState ("\n")
       execStateT executeState ("HINT : BEFORE USING THIS APPLICATION, PLEASE READ THE 'README.txt' FILE!\n")
       login "Value Investor"
                            
login :: String -> IO ()
login username = do
                 execStateT executeState ("------------------------------------------------\n")
                 execStateT executeState ("Welcome to Value Investing Calculator Application\n")
                 execStateT executeState ("------------------------------------------------\n")
                 execStateT executeState ("Hint : Please login with your investor account\n")
                 execStateT executeState ("Or You can use guest account\n")
                 execStateT executeState ("[Username: guest] [Password : SKYTROOPERS]\n\n")
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
    | (username == "admin") && (password == "LORDJEDI")                    = succeedLogin username
    | (username == "guest") && (password == "SKYTROOPERS")                 = succeedLogin username
    | (username == "Valdryan Ivandito") && (password == "fibonacci@0.618") = succeedLogin username
    | (username == "Sofian Fadli") && (password == "fibonacci@1.618")      = succeedLogin username
    | (username == "Gerry Fernando") && (password == "fibonacci@2.618")    = succeedLogin username
    | otherwise                                                            = reLogin username

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
                   execStateT executeState ("\n")
                   execStateT executeState ("------------------------------------------------\n")
                   execStateT executeState ("Invalid Username or Password!\n")
                   execStateT executeState ("------------------------------------------------\n")
                   execStateT executeState ("Choose menu\n")
                   execStateT executeState ("(1) Try Again\n")
                   execStateT executeState ("Press otherwise button to Quit\n")
                   execStateT executeState ("Option: ")
                   option <- getLine
                   case option of
                        "1" -> login username
                        _   -> return ()              

menu :: String -> IO ()
menu username = do 
                execStateT executeState ("\n")
                execStateT executeState ("------------------------------------------------\n")
                execStateT executeState ("Welcome Value Investor, can I help you?\n")
                execStateT executeState ("------------------------------------------------\n")
                execStateT executeState ("Hint : To input data you can use the sample data\n")
                execStateT executeState ("available in the 'SampleDataSource.txt' file\n\n")
                execStateT executeState ("Choose menu\n")
                execStateT executeState ("(1) Input Data\n")
                execStateT executeState ("(2) Read Data\n")
                execStateT executeState ("(3) Overwrite Data\n")
                execStateT executeState ("(4) Read Database\n")
                execStateT executeState ("(5) Read Log Data\n")
                execStateT executeState ("Press otherwise button to Quit\n")
                execStateT executeState ("Option: ")
                option <- getLine
                case option of
                     "1" -> inputData username
                     "2" -> readData username
                     "3" -> overwriteData username
                     "4" -> isAdminDatabase username
                     "5" -> isAdminLog username
                     _   -> do 
                            timeStamp <- getZonedTime
                            let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                            appendFile "log_file.txt" (timeStampResult ++ ";" ++ "LOGOUT" ++ ";" ++ username ++ " logged out" ++ "\n")
                            execStateT executeState ("\nSee you again... happy investing :)\n")
                            return ()
 
inputData :: String -> IO ()
inputData username = do 
                     execStateT executeState ("\n")
                     execStateT executeState ("------------------------------------------------\n")
                     execStateT executeState ("Input Data\n")
                     execStateT executeState ("------------------------------------------------\n")
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
                     execStateT executeState ("Year: ")
                     year <- getLine
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

                     execStateT executeState ("\n")
                     execStateT executeState ("------------------------------------------------\n")
                     execStateT executeState ("Result: \n")
                     execStateT executeState ("------------------------------------------------\n")
                     execStateT executeState ((fmap toUpper companyName) ++ " (" ++ (fmap toUpper stockCode) ++ ")" ++ " Result: " ++ "\n")
                     execStateT executeState ("BusinessSector: " ++ (fmap toUpper businessSector) ++ "\n")
                     execStateT executeState ("Year: " ++ year ++ "\n")
                     execStateT executeState ("Quartal: " ++ quartal ++ "\n")
                     execStateT executeState ("ROE(%): " ++ show (round' roeResult) ++ "\n")
                     execStateT executeState ("DER(%): " ++ show (round' derResult) ++ "\n")
                     execStateT executeState ("BV: " ++ show (round bvResult) ++ "\n")
                     execStateT executeState ("PBV: " ++ show (round' pbvResult) ++ "\n")
                     execStateT executeState ("EPS: " ++ show (round' epsResult) ++ "\n")
                     execStateT executeState ("PER: " ++ show (round' perResult) ++ "\n")
                     execStateT executeState ("Intrinsic Value: " ++ show (round intrinsicValueResult) ++ "\n")
                     execStateT executeState ("MOSPrice: " ++ show (round mosPriceResult) ++ "\n")
                     execStateT executeState (conclusion ++ "\n")
                     
                     appendFile (username ++ "_read_file.txt") (timeStampResult ++ "\n" ++ (fmap toUpper companyName) ++ " (" ++ (fmap toUpper stockCode) ++ ")" ++ " Result: " ++ "\n" ++ "BusinessSector: " ++ (fmap toUpper businessSector) ++ "\n" ++ "Year: " ++ year ++ "\n"++ "Quartal: " ++ quartal ++ "\n" ++ "ROE(%): " ++ show (round' roeResult) ++ "\n" ++ "DER(%): " ++ show (round' derResult) ++ "\n" ++ "BV: " ++ show (round bvResult) ++ "\n" ++ "PBV: " ++ show (round' pbvResult) ++ "\n" ++ "EPS: " ++ show (round' epsResult) ++ "\n" ++ "PER: " ++ show (round' perResult) ++ "\n" ++ "Intrinsic Value: " ++ show (round intrinsicValueResult) ++ "\n" ++ "MOSPrice: " ++ show (round mosPriceResult) ++ "\n" ++ conclusion ++ "\n" ++ "\n")
                     appendFile "database_file.txt" (timeStampResult ++ ";" ++ "Created by " ++ username ++ ";" ++ (fmap toUpper companyName) ++ ";" ++ (fmap toUpper stockCode) ++ ";" ++ (fmap toUpper businessSector) ++ ";" ++ year ++ ";" ++ quartal ++ ";" ++ show (round' roeResult) ++ ";" ++ show (round' derResult) ++ ";" ++ show (round bvResult) ++ ";" ++ show (round' pbvResult) ++ ";" ++ show (round' epsResult) ++ ";" ++ show (round' perResult) ++ ";" ++ show (round intrinsicValueResult) ++ ";" ++ show (round mosPriceResult) ++ ";" ++ conclusion ++ "\n")
                     appendFile "log_file.txt" (timeStampResult ++ ";" ++ "CREATE" ++ ";" ++ username ++ " created " ++ (fmap toUpper stockCode) ++ " data" ++ "\n")

                     menu username

overwriteData :: String -> IO ()
overwriteData username = do 
                     execStateT executeState ("\n")
                     execStateT executeState ("------------------------------------------------\n")
                     execStateT executeState ("Overwrite Data\n")
                     execStateT executeState ("------------------------------------------------\n")
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
                     execStateT executeState ("Year: ")
                     year <- getLine
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

                     execStateT executeState ("\n")
                     execStateT executeState ("------------------------------------------------\n")
                     execStateT executeState ("Result: \n")
                     execStateT executeState ("------------------------------------------------\n")
                     execStateT executeState ((fmap toUpper companyName) ++ " (" ++ (fmap toUpper stockCode) ++ ")" ++ " Result: " ++ "\n")
                     execStateT executeState ("BusinessSector: " ++ (fmap toUpper businessSector) ++ "\n")
                     execStateT executeState ("Year: " ++ year ++ "\n")
                     execStateT executeState ("Quartal: " ++ quartal ++ "\n")
                     execStateT executeState ("ROE(%): " ++ show (round' roeResult) ++ "\n")
                     execStateT executeState ("DER(%): " ++ show (round' derResult) ++ "\n")
                     execStateT executeState ("BV: " ++ show (round bvResult) ++ "\n")
                     execStateT executeState ("PBV: " ++ show (round' pbvResult) ++ "\n")
                     execStateT executeState ("EPS: " ++ show (round' epsResult) ++ "\n")
                     execStateT executeState ("PER: " ++ show (round' perResult) ++ "\n")
                     execStateT executeState ("Intrinsic Value: " ++ show (round intrinsicValueResult) ++ "\n")
                     execStateT executeState ("MOSPrice: " ++ show (round mosPriceResult) ++ "\n")
                     execStateT executeState (conclusion ++ "\n")
                     
                     writeFile (username ++ "_read_file.txt") (timeStampResult ++ "\n" ++ (fmap toUpper companyName) ++ " (" ++ (fmap toUpper stockCode) ++ ")" ++ " Result: " ++ "\n" ++ "BusinessSector: " ++ (fmap toUpper businessSector) ++ "\n" ++ "Year: " ++ year ++ "\n"++ "Quartal: " ++ quartal ++ "\n" ++ "ROE(%): " ++ show (round' roeResult) ++ "\n" ++ "DER(%): " ++ show (round' derResult) ++ "\n" ++ "BV: " ++ show (round bvResult) ++ "\n" ++ "PBV: " ++ show (round' pbvResult) ++ "\n" ++ "EPS: " ++ show (round' epsResult) ++ "\n" ++ "PER: " ++ show (round' perResult) ++ "\n" ++ "Intrinsic Value: " ++ show (round intrinsicValueResult) ++ "\n" ++ "MOSPrice: " ++ show (round mosPriceResult) ++ "\n" ++ conclusion ++ "\n" ++ "\n")
                     appendFile "database_file.txt" (timeStampResult ++ ";" ++ "Created by " ++ username ++ ";" ++ (fmap toUpper companyName) ++ ";" ++ (fmap toUpper stockCode) ++ ";" ++ (fmap toUpper businessSector) ++ ";" ++ year ++ ";" ++ quartal ++ ";" ++ show (round' roeResult) ++ ";" ++ show (round' derResult) ++ ";" ++ show (round bvResult) ++ ";" ++ show (round' pbvResult) ++ ";" ++ show (round' epsResult) ++ ";" ++ show (round' perResult) ++ ";" ++ show (round intrinsicValueResult) ++ ";" ++ show (round mosPriceResult) ++ ";" ++ conclusion ++ "\n")
                     appendFile "log_file.txt" (timeStampResult ++ ";" ++ "OVERWRITE" ++ ";" ++ username ++ " overwrited all data" ++ "\n")
                     appendFile "log_file.txt" (timeStampResult ++ ";" ++ "CREATE" ++ ";" ++ username ++ " created " ++ (fmap toUpper stockCode) ++ " data" ++ "\n")
                     menu username

readData :: String -> IO ()
readData username = do
                    execStateT executeState ("\n")
                    execStateT executeState ("------------------------------------------------\n")
                    execStateT executeState ("Read Data: \n")
                    execStateT executeState ("------------------------------------------------\n")
                    timeStamp <- getZonedTime
                    let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                    appendFile "log_file.txt" (timeStampResult ++ ";" ++ "READ" ++ ";" ++ username ++ " load all data " ++ "\n")
                    allData <- readFile ("./" ++ username ++ "_read_file.txt")
                    execStateT executeState (allData)
                    menu username

readLog :: String -> IO ()
readLog username = do 
                   timeStamp <- getZonedTime
                   let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                   appendFile "log_file.txt" (timeStampResult ++ ";" ++ "LOGIN" ++ ";" ++ username ++ " login" ++ "\n")
                   execStateT executeState ("\n")
                   execStateT executeState ("------------------------------------------------\n")
                   execStateT executeState ("Read Log Data: \n")
                   execStateT executeState ("------------------------------------------------\n")
                   timeStamp <- getZonedTime
                   let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                   appendFile "log_file.txt" (timeStampResult ++ ";" ++ "READ" ++ ";" ++ username ++ " read all log " ++ "\n")
                   allData <- readFile "./log_file.txt"
                   execStateT executeState (allData ++ "\n")
                   login username

readDatabase :: String -> IO ()
readDatabase username = do 
                        timeStamp <- getZonedTime
                        let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                        appendFile "log_file.txt" (timeStampResult ++ ";" ++ "LOGIN" ++ ";" ++ username ++ " login" ++ "\n")
                        execStateT executeState ("\n")
                        execStateT executeState ("------------------------------------------------\n")
                        execStateT executeState ("Read Database: \n")
                        execStateT executeState ("------------------------------------------------\n")
                        timeStamp <- getZonedTime
                        let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                        appendFile "log_file.txt" (timeStampResult ++ ";" ++ "READ" ++ ";" ++ username ++ " read database " ++ "\n")
                        allData <- readFile "./database_file.txt"
                        execStateT executeState (allData ++ "\n")
                        login username     

isAdminDatabase :: String -> IO ()
isAdminDatabase username
    | username /= "admin" = adminLoginDatabase username
    | otherwise           = readDatabase username

isAdminLog :: String -> IO ()
isAdminLog username
    | username /= "admin" = adminLoginLog username
    | otherwise           = readLog username

adminLoginDatabase :: String -> IO ()
adminLoginDatabase username = do 
                              execStateT executeState ("\n")
                              execStateT executeState ("------------------------------------------------\n")
                              execStateT executeState ("To read database need administrator authority!\n")
                              execStateT executeState ("------------------------------------------------\n")
                              execStateT executeState ("Hint : [Username: admin] [Password : LORDJEDI]\n\n")
                              execStateT executeState ("Admin username: ")
                              username <- getLine
                              execStateT executeState ("Admin Password: ")
                              password <- withEcho False getLine
                              execStateT executeState ("\n")
                              checkLoginAdminDatabase username password

adminLoginLog :: String -> IO ()
adminLoginLog username = do 
                         execStateT executeState ("\n")
                         execStateT executeState ("------------------------------------------------\n")
                         execStateT executeState ("To read log need administrator authority!\n")
                         execStateT executeState ("------------------------------------------------\n")
                         execStateT executeState ("Hint: [Username: admin] [Password : LORDJEDI]\n\n")
                         execStateT executeState ("Admin username: ")
                         username <- getLine
                         execStateT executeState ("Admin Password: ")
                         password <- withEcho False getLine
                         execStateT executeState ("\n")
                         checkLoginAdminLog username password

checkLoginAdminDatabase :: String -> String -> IO ()
checkLoginAdminDatabase username password 
    | (username == "admin") && (password == "LORDJEDI") = readDatabase username
    | otherwise                                         = failedLoginAdminDatabase username

checkLoginAdminLog :: String -> String -> IO ()
checkLoginAdminLog username password 
    | (username == "admin") && (password == "LORDJEDI") = readLog username
    | otherwise                                         = failedLoginAdminLog username

failedLoginAdminDatabase :: String -> IO ()
failedLoginAdminDatabase username = do 
                            timeStamp <- getZonedTime
                            let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                            appendFile "log_file.txt" (timeStampResult ++ ";" ++ "FAILED" ++ ";" ++ username ++ " failed administrator authority login" ++ "\n")
                            execStateT executeState ("\n")
                            execStateT executeState ("------------------------------------------------\n")
                            execStateT executeState ("Invalid Admin Username or Password!\n")
                            execStateT executeState ("------------------------------------------------\n")
                            execStateT executeState ("Choose menu\n")
                            execStateT executeState ("(1) Try Again\n")
                            execStateT executeState ("Press otherwise button to back main menu\n")
                            execStateT executeState ("Option: ")
                            option <- getLine
                            case option of
                                 "1" -> adminLoginDatabase username
                                 _   -> menu username

failedLoginAdminLog :: String -> IO ()
failedLoginAdminLog username = do 
                               timeStamp <- getZonedTime
                               let timeStampResult = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" timeStamp
                               appendFile "log_file.txt" (timeStampResult ++ ";" ++ "FAILED" ++ ";" ++ username ++ " failed administrator authority login" ++ "\n")
                               execStateT executeState ("\n")
                               execStateT executeState ("------------------------------------------------\n")
                               execStateT executeState ("Invalid Admin Username or Password!\n")
                               execStateT executeState ("------------------------------------------------\n")
                               execStateT executeState ("Choose menu\n")
                               execStateT executeState ("(1) Try Again\n")
                               execStateT executeState ("Press otherwise button to back main menu\n")
                               execStateT executeState ("Option: ")
                               option <- getLine
                               case option of
                                    "1" -> adminLoginLog username
                                    _   -> menu username
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------