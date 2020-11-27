-- this will cause an error:
-- module Main (fromGermanFloat, toGermanFloat, round100) where

-- create HTML with `pulp build -O -t main.js`

-- TODO 2020-11 Write test case and then add data type for one row. Then extend by one more field in the
-- row.

module Main (JHours(..), XHours(..), SHours(..), spread', renderToHTML, main) where

import Prelude (class Show, Unit, bind, const, map, pure, ($), (-), (/=), (<>), (==), (<=<), (<<<), show, (>>=), (*), (/), (>>>), (<$>), (<*>), otherwise, (<=), min ,(+))
import Control.Monad.Eff (Eff)
import Text.Parsing.CSV (Parsers, makeParsers)
import Text.Parsing.Parser (runParser)
import Data.List (List(Nil), elemIndex, filter, range, (:), reverse, take, drop, zipWith, intercalate)
import Data.Map as M
import Data.Int (fromString, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isNothing)
import Data.Either (Either(..), either)
import Data.Traversable (sequence, or, foldMap, fold, foldr)
import Data.String (take, split, Pattern(..)) as S
import Global as G

import Data.Monoid (mempty)
import Data.NonEmpty ((:|))
import DOM (DOM)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML (table, td, tr, th, h2) as HTML
import Text.Smolder.Markup (Markup, text) as MU
import Text.Smolder.Renderer.String (render) as TSRS
import Flare (UI, textarea, radioGroup, intSlider)
import Flare.Smolder (runFlareHTML)

import Data.Eq (class Eq)

import Data.Tuple

badjobs = ("INTERFLEX" : "D1CS" : "ORGA" : "AZ" : "Sonstiges" : Nil) :: List String

month :: Int -> List Int
month offset = range (1 - offset) 31

daysInWeek = 7 :: Int

-- Group a list into groups of certain size, returning list of lists

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

-- turn 4.0 into 4,0

toGermanFloat :: forall a. (Show a) => a -> Either String String
toGermanFloat n = case S.split (S.Pattern ".") (show n) of
                    [p]      -> Right p -- not expecting this case anymore (will be getting `4.0` instead of `4`)
                    [p, "0"] -> Right p -- show `0.0` as `0` etc.
                    [p, s]   -> Right (p <> "," <> s)
                    _        -> Left ("Error: Could not convert " <> show n <> "to German float.")

round100 :: Number -> Number
round100 n = toNumber (round (n * 100.0)) / 100.0

-- Create list of jobs where each job is the name of a project or account and has a map of date and hours
-- (efforts)

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

-- TODO 2020-11 Make a type out of this so that I see what "fields" CATS expects and don't send too few or
-- too many fields.

formatRowForCATS :: List String -> List String
formatRowForCATS (job : efforts) = (job : Nil) <> filler <> foldMap (\e -> "" : e : Nil) efforts
formatRowForCATS j = j

showJob' :: List Int -> Job -> List String
showJob' days (Job { job, efforts }) = job : foldMap (\d -> showEffort d : Nil) days
  where showEffort :: Int -> String
        showEffort day = fromMaybe "" (M.lookup day efforts)

-- PureScript uses <<< rather than . for right-to-left composition of functions. This is to avoid a
-- syntactic ambiguity with . being used for property access and name qualification. There is also a
-- corresponding >>> operator for left-to-right composition.
-- https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md
-- (f <<< g) x = f (g x)
-- https://stackoverflow.com/questions/29881695/what-does-the-triple-less-than-sign-do-in-purescript

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
-- TODO 2020-11 Make this safer by introducing type with certain amount of fields (maybe even named)

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

-- for splitting "Sonstiges"

-- Idea. Filter list of jobs by "Sonstiges", turn each hit (even though it should only be one, but you never
-- know) from Job to List of SHours, get weighting from the "good jobs", turn into JHours, call `spread` for
-- each hit, turn result from XHours to Job again and add to the list of jobs. They will show up as a
-- separate row in the output. Add comment for this row to output, in the field after the account number.

-- Idea. Switch from List SHours to Map?

-- here
-- toUnfoldable :: M.Map Int String -> List (Tuple Int String)

-- this could be simplified by just using Number instead of String for hours and inside efforts.

jobToSHours :: Job -> Either String (List SHours)
jobToSHours (Job { efforts }) = sequence $ map (\(Tuple day s) -> fromGermanFloat s >>= \hours -> pure $ SHours { day, hours }) $ M.toUnfoldable efforts

jobToJHours :: Job -> Either String (List JHours)
jobToJHours (Job { job, efforts }) = sequence $ map (\h -> fromGermanFloat h >>= \hours -> pure $ JHours { task : job, hours }) $ M.values efforts

-- how to turn list into Map
-- efforts : fold (map (\e -> M.singleton 0 e) es'')
-- or just
-- efforts : fold (map (M.singleton 0) es'')

mapOverEfforts :: Job -> M.Map Int String
mapOverEfforts (Job { efforts }) = M.mapWithKey (\k v -> v) efforts

divideAllEfforts :: Job -> Number -> Either String Job
divideAllEfforts (Job { job, efforts }) x = if x == 0.0 then Left "Divide by zero error."
                                              else do es <- sequence $ map fromGermanFloat $ M.values efforts -- FIXME must map over all of efforts, to keep data type
                                                      let es' = map (\y -> y / x) es
                                                      es'' <- sequence $ map toGermanFloat $ es'
                                                      pure $ Job { job
                                                                 , efforts : fold (map (M.singleton 0) es'') } -- FIXME : Need to get day into this

-- use zipwith?
-- efforts = fold (zipWith M.singleton ds hs)
-- Job is String Int String (why not Number instead of String?)

-- FIXME I actually need List XHours -> List Job. And to group XHours by task. Otherwise I get a separate line in the output for each day
-- of the task instead of one line with all days in a week.

xHoursToJob :: XHours -> Either String Job
xHoursToJob (XHours { day, task, hours }) = do hours' <- toGermanFloat $ round100 hours
                                               pure $ Job { job : task
                                                          , efforts : M.singleton day hours' }

nameThisFunction :: List Job -> Either String (List (List SHours))
nameThisFunction js = sequence $ map jobToSHours $ filter (\(Job {job}) -> job == "Sonstiges") js

totalEffortsOfJob :: Job -> Either String Number
totalEffortsOfJob (Job { efforts }) = do es <- sequence $ map fromGermanFloat $ M.values efforts
                                         pure $ foldr (\x y -> x + y) 0.0 es

totalEfforts :: List Job -> Either String Number
totalEfforts js = do efforts <- sequence $ map totalEffortsOfJob $ filter goodJob js
                     pure $ foldr (\x y -> x + y) 0.0 efforts

derive instance eqXHours :: Eq XHours

instance showXHours :: Show XHours where
  show (XHours x) = show (x.day) <> " " <> x.task <> " " <> show (x.hours)

newtype SHours = SHours { day   :: Int
                        , hours :: Number }

newtype JHours = JHours { task  :: String
                        , hours :: Number }

newtype XHours = XHours { day   :: Int
                        , task  :: String
                        , hours :: Number }

spread :: List SHours -> List JHours -> List XHours -> List XHours
spread (SHours s : srest) (JHours j : jrest) acc | s.hours <= 0.0 = spread srest              (JHours j : jrest) acc
                                                 | j.hours <= 0.0 = spread (SHours s : srest) jrest              acc
                                                 | otherwise      = spread (SHours s { hours = s.hours - x } : srest)
                                                                           (JHours j { hours = j.hours - x } : jrest)
                                                                           (XHours { day : s.day, task : j.task, hours : x } : acc)
                                                                      where x = min s.hours j.hours

spread _                  _                  acc                  = reverse acc

spread' :: List SHours -> List JHours -> List XHours
spread' a b = spread a b Nil

-- TODO: How to get copy and paste to work without using " " to recognize EOL?

excelParsers = makeParsers '\'' "\t" "\n" :: Parsers String

table :: forall e. List (List String) -> MU.Markup e
table (h : rs) = go h <> foldMap row rs
  where row r = HTML.tr $ foldMap cell r
        cell i = HTML.td (MU.text i)
        cell' i = HTML.th (MU.text i)
        go h' = HTML.tr $ foldMap cell' h'
table rs = mempty

table' :: forall e. List (List (List String)) -> MU.Markup e
table' ts = HTML.table $ foldMap table ts

-- third parameter is input field

renderInput :: forall e. Weekday -> Int -> String -> MU.Markup e
renderInput i r s = HTML.h2 (MU.text "Output") <>
                    case r of 0 -> case raw of
                                     Right input' -> table' (input' : Nil)
                                     Left err     -> MU.text err
                              1 -> case month' of
                                     Right m  -> table' m
                                     Left err -> MU.text err
                              2 -> case monthS of
                                     Right m  -> table' m
                                     Left err -> MU.text err
                              3 -> case weeks of
                                     Right w  -> table' w
                                     Left err -> MU.text err
                              4 -> case weeksC of
                                     Right w  -> table' w
                                     Left err -> MU.text err
                              otherwise -> mempty

  where raw        = switchEither "Error: Parsing CSV failed!" $ runParser s excelParsers.file
        parsed     = raw    >>= parsedFileToJobs -- this is where to also spread out "Sonstiges" into "good" jobs
        month'     = parsed >>=                    (\jobs -> let groups = groupBy 31 Nil (month 0) in
                                                             map (groupFilterShowNoFilter jobs) groups) >>> pure
        monthS     = parsed >>= filter goodJob >>> (\jobs -> let groups = groupBy 7 Nil (month (toInt i)) in
                                                             map (groupFilterShowStaggered jobs) groups) >>> pure
        weeks      = parsed >>= filter goodJob >>> (\jobs -> let groups = groupBy 7 Nil (month (toInt i)) in
                                                             map (groupFilterShowNoFilter jobs) groups) >>> pure
        weeksC     = parsed >>= filter goodJob >>> (\jobs -> let groups = groupBy 7 Nil (month (toInt i)) in
                                                             map (groupFilterShow jobs) groups) >>> pure

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

renderToHTML :: String -> String
renderToHTML = TSRS.render <<< renderInput Monday 4

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

ui :: forall e e'. UI e (MU.Markup e')
ui = renderInput <$>
     radioGroup "First of month" (Monday :| [Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]) show <*>
     intSlider "Morph" 0 4 0  <*>
     textarea "Raw Input" ""

main :: forall a. Eff ( dom :: DOM, channel :: CHANNEL | a ) Unit
main = runFlareHTML "controls" "output" ui
