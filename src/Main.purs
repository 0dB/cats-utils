-- this will cause an error:
-- module Main (fromGermanFloat, toGermanFloat, round100) where

-- create HTML with `pulp build -O -t main.js`

module Main where

import Prelude (class Show, Unit, bind, const, map, pure, ($), (-), (/=), (<>), (==), (<=<), (<<<), show, (>>=), (*), (/), (>>>), (<$>), (<*>), otherwise)
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
import Flare (UI, textarea, radioGroup, intSlider)
import Flare.Smolder (runFlareHTML)

badjobs = ("INTERFLEX" : "D1CS" : "ORGA" : "AZ" : "Sonstiges" : Nil) :: List String

month :: Int -> List Int
month offset = range (1 - offset) 31

daysInWeek = 7 :: Int

groupBy :: forall a. Int -> List (List a) -> List a -> List (List a)
groupBy n acc Nil = reverse acc
groupBy n acc r = groupBy n (take n r : acc) (drop n r)

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
        readDay d | Just date <- fromString d = Right date
                  | otherwise                 = Left ("Error: Could not read date " <> d <> ".")

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

filler = ("" : "" : Nil) :: List String

internalExternal :: List String -> List String
internalExternal (job : efforts) = job' <> efforts
  where job' = case S.take 1 job == "S" of
                 true  -> ("" : job : Nil)
                 false -> (job : "" : Nil)
internalExternal Nil = Nil

formatRowForCATS :: List String -> List String
formatRowForCATS (job : efforts) = (job : Nil) <> filler <> foldMap (\e -> "" : e : Nil) efforts
formatRowForCATS j = j

showJob' :: List Int -> Job -> List String
showJob' days (Job { job, efforts }) = job : foldMap (\d -> showEffort d : Nil) days
  where showEffort :: Int -> String
        showEffort day = fromMaybe "" (M.lookup day efforts)

showJob :: List Int -> Job -> List String
showJob days = showJob' days >>> formatRowForCATS >>> internalExternal

-- Move formatRowForCATS and internalExternal (using map) into higher level function used by showJobs and this higher
-- level function just processes header row differently than the remaining rows.  Can I successively get showJobs from
-- showJobs' by just composing functions?

showHeaderRow' :: List Int -> List String
showHeaderRow' days = ("" : Nil) <> foldMap (\d -> show d : Nil) days

prefixHeader :: List String -> List String
prefixHeader (c : cs) = ("EXTERN" : "INTERN" : Nil) <> cs
prefixHeader Nil = Nil

showHeaderRow :: List Int -> List String
showHeaderRow = showHeaderRow' >>> formatRowForCATS >>> prefixHeader

-- insert blank line every six rows (CATS only lets me add six lines at a time)

emptyRow = (("" : "" : "" : "" : "" : "" : "" : "" : "" : "" : "" : "" : "" : "" : "" : "" : "" : "" : Nil) : Nil)  :: List (List String)

showJobs :: List Int -> List Job -> List (List String)
showJobs days = (:) (showHeaderRow days) <<<
                intercalate emptyRow <<<
                groupBy 6 Nil <<<
                map (showJob days)

showJobs' :: List Int -> List Job -> List (List String)
showJobs' days = (:) (showHeaderRow' days) <<<
                 map (showJob' days)

-- date range (“week”) is used *twice*, first to find dates in the week being looked at
-- (and then for filtering) and then for rendering the week (`showJobs`)

groupFilterShow :: List Job -> List Int -> List (List String)
groupFilterShow js r = showJobs r $ filter nonZeroP $ filterDateRange r $ js

groupFilterShowStaggered :: List Job -> List Int -> List (List String)
groupFilterShowStaggered js r = showJobs' (range 1 31) $ filter nonZeroP $ filterDateRange r $ js

groupFilterShowNoFilter :: List Job -> List Int -> List (List String)
groupFilterShowNoFilter js r = showJobs' r $ filter nonZeroP $ filterDateRange r $ js

-- convert from `Either ParseError` to `Either String`

switchEither :: forall a b. String -> Either a b -> Either String b
switchEither text = either (const (Left text)) Right

-- TODO: How to get copy and paste to work without using " " to recognize EOL?

excelParsers = makeParsers '\'' "\t" "\n" :: Parsers String

table :: forall e. List (List String) -> H.Markup e
table (h : rs) = go h <> foldMap row rs
  where row r = H.tr $ foldMap cell r
        cell i = H.td (H.text i)
        cell' i = H.th (H.text i)
        go h' = H.tr $ foldMap cell' h'
table rs = mempty

table' :: forall e. List (List (List String)) -> H.Markup e
table' ts = H.table $ foldMap table ts

renderInput :: forall e. String -> Weekday -> Int -> H.Markup e
renderInput s i r = H.h2 (H.text "Output") <>
                    case r of 0 -> case raw of
                                     Right input' -> table' (input' : Nil)
                                     Left err     -> H.text err
                              1 -> case month' of
                                     Right n  -> table' n
                                     Left err -> H.text err
                              2 -> case monthS of
                                     Right m  -> table' m
                                     Left err -> H.text err
                              3 -> case weeks of
                                     Right m  -> table' m
                                     Left err -> H.text err
                              4 -> case weeksC of
                                     Right output' -> table' output'
                                     Left err      -> H.text err
                              otherwise -> mempty

  where raw        = switchEither "Error: Parsing CSV failed!" $ runParser s excelParsers.file
        parsed     = raw    >>= parsedFileToJobs
        month'     = parsed >>=                    (\jobs -> let groups = groupBy 31 Nil (month 0) in
                                                             map (groupFilterShowNoFilter jobs) groups) >>> pure
        monthS     = parsed >>= filter goodJob >>> (\jobs -> let groups = groupBy 7 Nil (month (toInt i)) in
                                                             map (groupFilterShowStaggered jobs) groups) >>> pure
        weeks      = parsed >>= filter goodJob >>> (\jobs -> let groups = groupBy 7 Nil (month (toInt i)) in
                                                             map (groupFilterShowNoFilter jobs) groups) >>> pure
        weeksC     = parsed >>= filter goodJob >>> (\jobs -> let groups = groupBy 7 Nil (month (toInt i)) in
                                                             map (groupFilterShow jobs) groups) >>> pure

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

instance showWeekday :: Show Weekday where
  show :: Weekday -> String
  show Monday    = "Monday"
  show Tuesday   = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday  = "Thursday"
  show Friday    = "Friday"
  show Saturday  = "Saturday"
  show Sunday    = "Sunday"

toInt :: Weekday -> Int
toInt Monday    = 0
toInt Tuesday   = 1
toInt Wednesday = 2
toInt Thursday  = 3
toInt Friday    = 4
toInt Saturday  = 5
toInt Sunday    = 6

ui2 :: forall e e'. UI e (H.Markup e')
ui2 = renderInput <$>
      textarea "Raw Input" "" <*>
      radioGroup "First of month" (Monday :| [Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]) show <*>
      intSlider "Morph" 0 4 0

main :: forall a. Eff ( dom :: DOM, channel :: CHANNEL | a ) Unit
main = runFlareHTML "controls2" "output2" ui2
