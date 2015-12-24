{- | Finance.Quote.Yahoo

Finance.Quote.Yahoo is a module to obtain quote information from
finance.yahoo.com, which delivers a csv file with data for various fields,
which are documented at http:\/\/www.gummy-stuff.org\/Yahoo-data.htm.

The homepage for this module is
http:\/\/www.b7j0c.org\/stuff\/haskell-yquote.xhtml

The license for this module is at
http:\/\/www.b7j0c.org\/stuff\/license.txt

Since this uses Data.Time.Format, ghc-6.6.1 or greater is required.

Error reporting is somewhat of a mixed model in this module. Where strict
errors of data construction occur, these will be noted as fatal error()
signals, so the error can be noted and fixed. An example of this would be
putting the start and end data in the wrong order for the retrieval of
historical quotes or the creation of a malformed URI. On the other hand,
I continue to propogate Nothing() for networking issues as there may be
external issues creating these errors for which one may want program
execution to continue. My personal tendency is to fail early when
possible and practical.


Exported functions:

getQuote, which takes a list of quote symbols (in the finance sense of
\"symbol\" - YHOO,GOOG etc), a list of fields, and
returns a Data.Map, where the keys are pairs (symbol,field) and
values are the returned Strings. Upon any problem, Nothing is
returned. I have not cast the data into stronger types than String since
Yahoo is inconsistent about what is returned in the csv. Fields often
contain punctuation, symbols, as well as numbers. So really, they are
Strings.

getHistoricalQuote, which takes a quote symbol, and two Data.Time.Calendar
Day types, one for the starting date to receive quote data, and one for the
end date. Yahoo does not let you choose the fields to see in historical
quotes, data is limited to price and volume information.

quoteRec - useful for debugging the quote URI to see if Yahoo is denying
the service.

Here is small complete program illustrating the use of this module

@
  module Main where
  import Finance\.Quote\.Yahoo
  import Data\.Time\.Calendar
  import Data\.Map
  quoteSymbolList = [\"YHOO\"] :: [QuoteSymbol]
  quoteFieldsList = [\"s\",\"l1\",\"c\"] :: [QuoteField]
  main = do
  q <- getQuote quoteSymbolList quoteFieldsList
  case q of
    Nothing -> error \"no map\"
    Just m -> case (Data.Map.lookup (\"YHOO\",\"l1\") m) of
                   Nothing -> print \"no match\"
                   Just a -> print a
  let startDate = Data.Time.Calendar.fromGregorian 2007 07 01
  let endDate = Data.Time.Calendar.fromGregorian 2007 07 03
  h <- getHistoricalQuote (head quoteSymbolList) startDate endDate Daily
  case h of
    Nothing -> error \"no historical\"
    Just l -> sequence $ Prelude.map print l
  return ()
@

-}
module Finance.Quote.Yahoo (getQuote,getHistoricalQuote,defaultQuoteFields,
                            baseQuoteURI,baseHistoricalURI,quoteReq,
                            QuoteField,QuoteSymbol,QuoteValue,Quote,
                            QuoteCurrency,QuoteFrequency(..),
                            HistoricalQuote(..)) where
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Network.HTTP.Conduit as H (simpleHttp)
import qualified Network.URI as U (escapeURIString,isUnescapedInURI)
import qualified Data.Time.Calendar as T (Day(..),fromGregorian)
import qualified Data.Time.Format as F (formatTime)
import qualified Data.Time.Format as L (defaultTimeLocale)
import qualified Data.Map as M (fromList,Map)
import qualified Data.List as D (intersperse)

import Debug.Trace

  {-
  License info:

  The license is a simple BSD3-style license available here:
  http://www.b7j0c.org/content/license.txt

-}

type QuoteField  = String
type QuoteSymbol = String
type QuoteValue  = String
type QuoteCSV = C.ByteString
type Quote = [(QuoteField,QuoteValue)]

-- | This is the base uri to get csv quotes. Exported.
baseQuoteURI :: String
baseQuoteURI = "http://download.finance.yahoo.com/d/quotes.csv"

-- | If you just want the name, latest price and change, use this. Exported.
defaultQuoteFields :: [QuoteField]
defaultQuoteFields = ["n","l1","c"]

-- | quoteReq will build a String representation of a Yahoo Finance CSV
-- request URI.
quoteReq :: [QuoteSymbol] -> [QuoteField] -> String
quoteReq symbols fields =
  U.escapeURIString U.isUnescapedInURI
  $ baseQuoteURI ++ "?s=" ++
  (join "+" symbols) ++ "&f=" ++ (concat fields)
  where
    join :: String -> [String] -> String
    join sep = concat . D.intersperse sep

-- | parseQuote will take a list of symbols, a list of fields, and the
-- csv data and return a Data.Map, as described below (see getQuote).
-- If there is a mismatch in the number of fields being zipped to
-- produce the map, an error is triggered.
parseQuote :: [QuoteSymbol] -> [QuoteField] -> QuoteCSV ->
              Maybe (M.Map (QuoteSymbol, QuoteField) QuoteValue)
parseQuote symbols fields csv =
  let l = concatMap (csplit ',') $ C.lines $ 
          C.filter (\c -> notElem c "\r\"") csv
      p = [(x,y) | x <- symbols, y <- fields] in
  case length p == length l of
    True -> Just (M.fromList $ zip p $ map C.unpack l)
    False -> error "mismatch in fields and returned data"

-- | getQuote will take a list of symbols, a list of fields, and will
-- return a Data.Map, where the key type is
-- (symbol,field)
-- and the value type is whatever quote value string is returned.
-- An example map entry:
--
-- key: \(\"YHOO\",\"c\"\), value: \"24.00\"
--
-- Which gives you the closing price (c) for the symbol YHOO.
--
-- NOTE!
-- This function does NOT alter the casing of the quote symbols passed
-- in the first parameter. These symbols are used as the first element
-- of the Map key tuple without altering them. Be careful! This function
-- is exported.
getQuote :: [QuoteSymbol] -> [QuoteField] ->
            IO (Maybe (M.Map (QuoteSymbol, QuoteField) QuoteValue))
getQuote symbols fields =
  fmap (parseQuote symbols fields) $ H.simpleHttp (quoteReq symbols fields)

-- | This is the base uri to get csv historical quote data. Exported.
baseHistoricalURI :: String
baseHistoricalURI = "http://ichart.finance.yahoo.com/table.csv"

-- | Float is not an fully appropriate currency type, beware. Exported.
type QuoteCurrency = Float

-- | HistoricalQuote reflects the row form of a yahoo historical quote:
-- Date,Open,High,Low,Close,Volume,Adj Close (taken from the csv itself).
-- Exported.
data HistoricalQuote =
      HistoricalQuote {
        symbol :: QuoteSymbol,
        date :: T.Day,
        open :: QuoteCurrency,
        high :: QuoteCurrency,
        low :: QuoteCurrency,
        close :: QuoteCurrency,
        adjclose :: QuoteCurrency,
        volume :: Int
        } deriving Show
                   
-- | QuoteFrequency - frequency for historical quotes. Exported.
data QuoteFrequency = Daily | Weekly | Monthly | Dividend
                                                 
-- | historicalQuoteReq will build a String representation of a
-- Yahoo Finance CSV historical quote request URI.
historicalQuoteReq :: QuoteSymbol -> T.Day -> T.Day -> QuoteFrequency -> String
historicalQuoteReq symbol start end freq = 
    let (startDay,startMonth,startYear) = dateArgs start
        (endDay,endMonth,endYear) = dateArgs end 
        freqChar = freqArg freq in
    U.escapeURIString U.isUnescapedInURI 
         $ baseHistoricalURI ++ "?s=" ++ symbol ++
           "&a=" ++ startMonth ++ "&b=" ++ startDay ++ "&c=" ++ startYear ++
           "&d=" ++ endMonth ++ "&e=" ++ endDay ++ "&f=" ++ endYear ++
           "&g=" ++ [freqChar]
    where
      -- Return the string args for the URI.
      -- Note when parsing months - for some odd reason Yahoo has decided
      -- that month numbers should be zero-based...06 = July, etc.
      dateArgs :: T.Day -> (String,String,String)
      dateArgs t = (day,month,year) where 
          dtl = L.defaultTimeLocale 
          day = F.formatTime dtl "%d" t
          month = show $ (read (F.formatTime dtl "%m" t) :: Int) - 1
          year = F.formatTime dtl "%Y" t

      -- Return the char Yahoo uses to denote various frequencies
      freqArg :: QuoteFrequency -> Char
      freqArg f = case f of 
                    Daily -> 'd'
                    Weekly -> 'w'
                    Monthly -> 'm'
                    Dividend -> 'v'

-- | parseHistorical takes the raw csv from Yahoo Finance and returns
-- a list of HistoricalQuote entries.
parseHistorical :: QuoteSymbol -> QuoteCSV -> Maybe [HistoricalQuote]
parseHistorical symbol' csv =
  let l = reverse $ map (csplit ',') $
          (tail . C.lines) $ C.filter (\c -> notElem c "\r") csv in
  Just $ map (makeHistoricalQuote . map C.unpack) l
  where
    -- Create a HistoricalQuote entry from a line from the csv.
    makeHistoricalQuote :: [String] -> HistoricalQuote
    makeHistoricalQuote l = 
      case (length l == 7) of
        False -> error("malformed line:" ++ (show l))
        True -> let
          date'     = makeDay (l!!0)
          open'     = read (l!!1) :: QuoteCurrency
          high'     = read (l!!2) :: QuoteCurrency
          low'      = read (l!!3) :: QuoteCurrency
          close'    = read (l!!4) :: QuoteCurrency
          adjclose' = read (l!!6) :: QuoteCurrency
          volume'   = read (l!!5) :: Int                 
          in HistoricalQuote { symbol   = symbol',
                               date     = date',
                               open     = open',
                               high     = high',
                               low      = low',
                               close    = close',
                               adjclose = adjclose',
                               volume   = volume' }
          where
            -- Create a Day type from the str date from the csv.
            makeDay :: String -> T.Day
            makeDay s = 
                let a = split '-' s in
                case (length a == 3) of 
                  False -> error("date field " ++ s ++ " malformed")
                  True -> let y = read (a!!0) :: Integer
                              m = read (a!!1) :: Int
                              d = read (a!!2) :: Int in
                          T.fromGregorian y m d

-- | getHistoricalQuote takes a stock symbol, start and end date ranges,
-- a quote frequency setting, and obtains the HistoricalQuote lines
-- for this given date range and quote frequency.
-- Supported frequencies are "Daily", "Weekly",
-- "Monthly" or "Dividend". Hopefully these are self-explanatory.
-- Nothing is returned on any error, but note if you ask for the quotes
-- based on Dividend frequency for a stock that pays no dividends, you
-- will not see Nothing, but just an empty result.
-- Check finance.yahoo.com to see how
-- far they offer quote history for a symbol you are interested in.
-- Note! Yahoo takes some liberties with dates due to weekends and
-- holidays and market closures. Exported.
--
-- Here is what a sample result looks like for one day in the history:
--
-- HistoricalQuote \{symbol \= \"YHOO\",
--                   date \= 2007-07-02\,
--                   open \= 27.19\,
--                   high \= 27.27\,
--                   low \= 26.76\,
--                   close \= 26.86\,
--                   adjclose \= 26.86\,
--                   volume \= 21011000\}
--
getHistoricalQuote :: QuoteSymbol -> T.Day -> T.Day -> QuoteFrequency -> 
                    IO (Maybe [HistoricalQuote])
getHistoricalQuote symbol start end freq = 
    case end > start of
      False -> error("start date must be earlier than end date")
      True -> do 
        let req = historicalQuoteReq symbol start end freq
        csv <- H.simpleHttp req
        return $ parseHistorical symbol csv

-- split copied from missingh
csplit :: Char -> C.ByteString -> [C.ByteString]
csplit delim s = if C.null rest
                 then [token]
                 else token : csplit delim (C.tail rest)
  where (token,rest) = C.span (/=delim) s
        
-- split copied from missingh
split :: Char -> String -> [String]
split delim s = if null rest
                then [token]
                else token : split delim (tail rest)
  where (token,rest) = span (/=delim) s
