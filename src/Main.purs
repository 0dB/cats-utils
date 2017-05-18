-- this will cause an error:
-- module Main (fromGermanFloat, toGermanFloat, round100) where

-- create HTML with `pulp build -O -t main.js`

module Main where

import Prelude (class Show, Unit, bind, const, map, pure, ($), (-), (/=), (<>), (==), (<=<), (<<<), show, (>>=), (*), (/), (>>>), (<$>), (<*>), discard)
import Control.Monad.Eff (Eff)
import Text.Parsing.CSV (Parsers, makeParsers)
import Text.Parsing.Parser (runParser)
import Data.List (List(Nil), elemIndex, filter, range, (:), reverse, take, drop, zipWith, intercalate)
import Data.Map as M
import Data.Int (fromString, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isNothing)
import Data.Either (Either(..), either)
import Data.Traversable (sequence, or, foldMap, fold)
import Data.String (take, split, Pattern(..)) as S
import Global as G

import Data.Monoid (mempty)
import Data.NonEmpty ((:|))
import DOM (DOM)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML (table, td, tr, th, h2) as H
import Text.Smolder.Markup (Markup, text) as H
import Flare (UI, string, radioGroup, boolean)
import Flare.Smolder (runFlareHTML)

-- The following values are the parameters to be checked each month.  Week starts on Monday, so if first of current
-- month is Tuesday, then daysFromPrevMonth = 1, if e. g. Saturday, then 5.

daysFromPrevMonth = 5 :: Int

badjobs = ("INTERFLEX" : "D1CS" : "ORGA" : "AZ" : "Sonstiges" : Nil) :: List String

month :: Int -> List Int
month offset = range (1 - offset) 31

daysInWeek = 7 :: Int

groupBy :: forall a. Int -> List (List a) -> List a -> List (List a)
groupBy n acc Nil = reverse acc
groupBy n acc r = groupBy n (take n r : acc) (drop n r)

weeks :: forall a. List a -> List (List a)
weeks m = groupBy daysInWeek Nil m

fromGermanFloat :: String -> Either String Number
fromGermanFloat g = case S.split (S.Pattern ",") g of
                      [""]    -> Right 0.0 -- the ".0" will be filtered out later in `toGermanFloat`
                      [p]     -> case fromString p of
                                   Just p' -> Right (toNumber p')
                                   Nothing -> Left ("Error: Could not read number from " <> p <> ".")
                      ["", _] -> Left ("Error: Bad format for hours in " <> g <> ".")
                      [p,  s] -> case fromString p, fromString s of
                                   Just p', Just s' -> Right (G.readFloat (p <> "." <> s))
                                   _      , _       -> Left ("Error: Could not read number from " <> p <> "," <> s <> ".")
                      _       -> Left ("Error: Could not read hour from " <> g <> ".")

toGermanFloat :: forall a. (Show a) => a -> Either String String
toGermanFloat n = case S.split (S.Pattern ".") (show n) of
                    [p]      -> Right p -- not expecting this case anymore (will be getting `4.0` instead of `4`)
                    [p, "0"] -> Right p -- show `0.0` as `0` etc.
                    [p, s]   -> Right (p <> "," <> s)
                    _        -> Left ("Error: Could not convert " <> show n <> "to German float.")

round100 :: Number -> Number
round100 n = toNumber (round (n * 100.0)) / 100.0

type Efforts = M.Map Int String

newtype Job = Job { job     :: String
                  , efforts :: Efforts }

parsedFileToJobs :: List (List String) -> Either String (List Job)
parsedFileToJobs ((_ : dates) : joblines) = readDates dates >>= readJobs (filter notEmpty $ joblines)

  where readDates :: List String -> Either String (List Int)
        readDates ds = sequence $ map readDay ds

        readJobs :: List (List String) -> List Int -> Either String (List Job)
        readJobs js ds = sequence $ map (readJob ds) js

        readDay :: String -> Either String Int
        readDay d = case fromString d of
                      Just date -> Right date
                      Nothing   -> Left ("Error: Could not read date " <> d <> ".")

        readJob :: List Int -> List String -> Either String Job
        readJob _   Nil          = Left "Error: No input line."
        readJob _  (job : Nil)   = Left ("Error: No efforts found after job name " <> job <> ".")
        readJob ds (job : hours) = do hs <- sequence $ map readHour hours
                                      let efforts = fold (zipWith M.singleton ds hs)
                                      pure $ Job { job, efforts }

        readHour :: String -> Either String String
        readHour = toGermanFloat <<< round100 <=< fromGermanFloat

        -- filter out empty line with just newline (actually only expected at the end)
        notEmpty :: List String -> Boolean
        notEmpty ("" : Nil) = false
        notEmpty _          = true

parsedFileToJobs _ = Left "Error: No input line found."

filterDateRange :: List Int -> List Job -> List Job
filterDateRange days jobs = map (go days) jobs
  where go :: List Int -> Job -> Job
        go days' (Job j) = Job (j { efforts = filteredEfforts days' j.efforts })

        filteredEfforts :: List Int -> Efforts -> Efforts
        filteredEfforts days' efforts = foldMap (filterEfforts efforts) days'

        filterEfforts :: Efforts -> Int -> Efforts
        filterEfforts es day = maybe M.empty (M.singleton day) (M.lookup day es)

nonZeroP :: Job -> Boolean
nonZeroP (Job { efforts }) = or $ map ((/=) "0") $ M.values efforts

goodJob :: Job -> Boolean
goodJob (Job { job }) = isNothing (elemIndex job badjobs)

filler = ("" : "" : "" : "" : Nil) :: List String

showJob :: List Int -> Job -> List String
showJob days (Job { job, efforts }) = job' <> filler <> foldMap (\d -> "" : showEffort d : Nil) days
  where job' = case S.take 1 job == "S" of
                 true  -> ("" : job : Nil)
                 false -> (job : "" : Nil)
        showEffort :: Int -> String
        showEffort day = fromMaybe "" (M.lookup day efforts)

showHeaderRow :: List Int -> List String
showHeaderRow days = ("EXTERN" : "INTERN" : Nil) <> filler <> foldMap (\d -> "" : show d : Nil) days

-- insert blank line every six rows (CATS only lets me add six lines at a time)

showJobs :: List Int -> List Job -> List (List String)
showJobs days = (:) (showHeaderRow days) <<< intercalate (("" : Nil) : Nil) <<< groupBy 6 Nil <<< map (showJob days)

-- date range (“week”) is used *twice*, first to find dates in the week being looked at
-- (and then for filtering) and then for rendering the week (`showJobs`)

groupFilterShow :: List Job -> List Int -> List (List String)
groupFilterShow js r = showJobs r $ filter nonZeroP $ filterDateRange r $ js

processWeeks :: Int -> List Job -> List (List (List String))
processWeeks offset jobs = map (groupFilterShow jobs) $ weeks (month offset)

processJobs :: Int -> List Job -> List (List (List String))
processJobs offset = processWeeks offset <<< filter goodJob

-- convert from `Either ParseError` to `Either String`

switchEither :: forall a b. String -> Either a b -> Either String b
switchEither text = either (const (Left text)) Right

-- TODO: How to get copy and paste to work without using " " to recognize EOL?
excelParsers = makeParsers '\'' "\t" " " :: Parsers String

table :: forall e. List (List String) -> H.Markup e
table (h : rs) = go h <> foldMap row rs
  where row r = H.tr $ foldMap cell r
        cell i = H.td (H.text i)
        cell' i = H.th (H.text i)
        go h' = H.tr $ foldMap cell' h'
table rs = mempty

table' :: forall e. List (List (List String)) -> H.Markup e
table' ts = H.table $ foldMap table ts

renderInput :: forall e. String -> Weekday -> Boolean -> H.Markup e
renderInput s i b = (if b then H.h2 (H.text "Parsed Input") <>
                               (case input of
                                  Right input' -> table' (input' : Nil)
                                  Left err     -> H.text err)
                          else mempty) <>
                    H.h2 (H.text "Output") <>
                    (case output of
                       Right output' -> table' output'
                       Left err      -> H.text err)
  where input  = switchEither "Error: Parsing CSV failed!" $ runParser s excelParsers.file
        output = input >>= parsedFileToJobs >>= processJobs (toInt i) >>> pure

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

toString :: Weekday -> String
toString Monday = "Monday"
toString Tuesday = "Tuesday"
toString Wednesday = "Wednesday"
toString Thursday = "Thursday"
toString Friday = "Friday"
toString Saturday = "Saturday"
toString Sunday = "Sunday"

toInt :: Weekday -> Int
toInt Monday = 0
toInt Tuesday = 1
toInt Wednesday = 2
toInt Thursday = 3
toInt Friday = 4
toInt Saturday = 5
toInt Sunday = 6

ui2 :: forall e e'. UI e (H.Markup e')
ui2 = renderInput <$>
      string "Raw Input: " "" <*>
      radioGroup "First of month: " (Monday :| [Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]) toString <*>
      boolean "Show parsed input: " true

main :: forall a. Eff ( dom :: DOM, channel :: CHANNEL | a ) Unit
main = runFlareHTML "controls2" "output2" ui2
