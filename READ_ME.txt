1. ABOUT THE APPLICATION 
   This application is used to calculate financial ratios, intrinsic value, 
   and conclusions regarding stock investment recommendations
   Note : This application designed to calculation financial ratio and intrinsic value 
          for Indonesian Stock Market (IDX) only

2. HOW TO LOGIN
   You can login using the accounts below :
   [username: "admin"] | [password: "LORDJEDI"]              
   [username: "guest"] | [password: "SKYTROOPERS"]               
   [username: "Valdryan Ivandito"] | [password: "fibonacci@0.618"] 
   [username: "Sofian Fadli"] | [password: "fibonacci@1.618"] 
   [username: "Gerry Fernando"] | [password: "fibonacci@2.618"]

3. ABOUT MENU
   (1) Input Data -> To input data about stocks to be analyzed and then 
       it will generate data in the form of financial ratio parameters 
       with recommendation and reason that will be used by investors.
   (2) Read Data -> To load the data from the stock analysis data input 
       that the previous user created.
   (3) Overwrite Data -> To overwrite the data that the previous user created 
       and then user can create new data from beginning.
   (4) Read Database -> To read database_file.txt that contains all the data 
       inputted by each user.
   (5) Read Log -> To read all activities in the application

4. ABOUT FILE.txt
   (READ_ME.txt) -> A file that contains information about the application and HOW
   to use it.
   (SampleDataSource.txt) -> A file that contains sample data that the user can use
   to input data.
   ('username'_read_file.txt) -> A file that contains input data about stocks 
   to be analyzed by user.
   (database_file.txt) -> A file that contains all the data inputted by each user.
   (log_file.txt) -> A file that contains all activities in the application.

5. SAMPLE DATA 
   To input data you can use data from 'SampleDataSource.txt' file or list below :

        CompanyName           : PT Radiant Utama Interinsco TBK
        BusinessSector        : Energy
        StockCode             : RUIS
        StockPrice            : 180
        StockShares           : 770000000
        Year                  : 2022
        Quartal               : 1
        Liability             : 816423175419
        Equity                : 495003354730
        NetProfit             : 10713098359
        Inflation 10 Years(%) : 44.21
        CAGR(%)               : 3.5
        MOS(%)                : 50

        CompanyName           : Ace Hardware Indonesia TBK
        BusinessSector        : Service & Trade
        StockCode             : ACES
        StockPrice            : 700
        StockShares           : 17150000000
        Year                  : 2022
        Quartal               : 1
        Liability             : 1642014628404
        Equity                : 5635023325606
        NetProfit             : 153498597471
        Inflation 10 Years(%) : 44.21
        CAGR(%)               : 3.5
        MOS(%)                : 50

        CompanyName           : Bank Rakyat Indonesia (Persero) TBK
        BusinessSector        : Finance
        StockCode             : BBRI
        StockPrice            : 4000
        StockShares           : 150043411587
        Year                  : 2022
        Quartal               : 1
        Liability             : 1374292888000000
        Equity                : 272274732000000
        NetProfit             : 12167224000000
        Inflation 10 Years(%) : 44.21
        CAGR(%)               : 3.5
        MOS(%)                : 50

        CompanyName           : Telkom Indonesia (Persero) TBK
        BusinessSector        : Infrastructure & Telecomunication
        StockCode             : TLKM
        StockPrice            : 4050
        StockShares           : 99062216600
        Year                  : 2022
        Quartal               : 1
        Liability             : 126120000000000
        Equity                : 127794000000000
        NetProfit             : 6118000000000
        Inflation 10 Years(%) : 44.21
        CAGR(%)               : 3.5
        MOS(%)                : 50

    Notes : 
    - The reference for 10-year inflation data is the actual accumulation of inflation from 2011 - 2021 
      (https://www.bi.go.id/id/statistik/indikator/target-inflasi.aspx)
    - The reference for CAGR can conservatively use Indonesian mutual fund returns per year of approximately 6 - 8% 
      or even more conservative is the interest rate of banks in Indonesia of approximately 3.5%
      (https://www.bi.go.id/id/publikasi/ruang-media/news-release/Pages/sp_2413622.aspx)
    - The reference for MOS is subjectif depend on investor preferences