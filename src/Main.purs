-- create HTML with `spago bundle-app`

-- TODO 2020-11 Write test case and then add data type for one row. Then extend by one more field in the
-- row.

module Main (JHours(..), Common, XHours(..), spread', renderToHTML, renderToHTML', main, Job(Job), Efforts, multiplyAllEfforts, totalEfforts) where

import Prelude (class Show, Unit, bind, const, map, pure, ($), (-), (/=), (<>), (==), (<<<), show
               , (>>=), (*), (/), (>>>), (<$>), (<*>), otherwise, (<=), min ,(+))
import Effect (Effect)
import Text.Parsing.CSV (Parsers, makeParsers)
import Text.Parsing.Parser (runParser)
import Data.List (List(Nil), elemIndex, filter, range, (:), reverse, take, drop, zipWith
                 ,intercalate, groupBy)
import Data.List.Types (NonEmptyList)
import Data.Map as M
import Data.Int (fromString, round, toNumber)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Either (Either(..), either)
import Data.Traversable (sequence, or, foldMap, fold, foldr)
import Data.String (take, split, Pattern(..)) as S
import Global as G

import Data.Monoid (mempty)
import Data.NonEmpty ((:|))
import Control.Monad.Free (Free)
import Text.Smolder.HTML (table, td, tr, th, h2) as HTML
import Text.Smolder.Markup (Markup, MarkupM, text) as MU
import Text.Smolder.Renderer.String (render) as TSRS
import Flare (UI, textarea, radioGroup, intSlider)
import Flare.Smolder (runFlareHTML)

import Data.Eq (class Eq)

badtasks = ("INTERFLEX" : "D1CS" : "ORGA" : "AZ" : "Sonstiges" : Nil) :: List String

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

-- see
-- https://stackoverflow.com/questions/51986883/how-to-combine-rows-of-record-types-in-purescript-is-there-any-alternative-to
-- "The definition of Common uses parentheses, not curly braces. That is because Common is a row, not a record. You can
-- make a record out of it [by prepending Record]." "What you want to do in PureScript is prefer "naked" records and
-- only resort to wrapping them in newtype when you really have to."

type Common = ( task  :: String
              , hours :: Number )

type JHours = Record Common

type XHours = { day :: Int | Common }

-- derive instance eqXHours :: Eq XHours

--instance showXHours :: Show XHours where
--  show x = show (x.day) <> " " <> x.task <> " " <> show (x.hours)

parsedFileToXHours :: List (List String) -> Either String (List XHours)
parsedFileToXHours ((_ : dates) : joblines) = readDates dates >>= readJobs (filter notEmpty joblines)

  where readDay :: String -> Either String Int
        readDay d | Just date <- fromString d = Right date
                  | otherwise                 = Left ("Error: Could not read date " <> d <> ".")

        readDates :: List String -> Either String (List Int)
        readDates ds = sequence $ map readDay ds

        readHour :: String -> Either String Number
        readHour = fromGermanFloat

        readJob :: List Int -> List String -> Either String (List XHours)
        readJob _   Nil         = Left "Error: No input line."
        readJob _  (task : Nil) = Left ("Error: No efforts found after job name " <> task <> ".")
        readJob ds (task : hs)  = zipWith (\day hours -> { task, day, hours } ) ds <$>
                                  sequence (map readHour hs)

        readJobs :: List (List String) -> List Int -> Either String (List XHours)
        readJobs js ds = fold <$> sequence (map (readJob ds) js)

        -- filter out empty line with just newline (actually only expected at the end)
        notEmpty :: List String -> Boolean
        notEmpty ("" : Nil) = false
        notEmpty _          = true

parsedFileToXHours _ = Left "Error: No input line found."

goodJob :: forall r. { task :: String | r } -> Boolean
goodJob { task } = isNothing (elemIndex task badtasks)

-- TIL toUnfoldable :: M.Map Int String -> List (Tuple Int String)

-- TIL how to turn List into Map
-- fold (map (M.singleton 0) x)

-- TIL Data.Map is also traversable so I can apply sequence()

spread :: forall r. List { day :: Int, hours :: Number | r } -> List JHours -> List XHours -> List XHours
spread (s : srest) (j : jrest) acc | s.hours <= 0.0 = spread srest              (j : jrest) acc
                                   | j.hours <= 0.0 = spread (s : srest) jrest              acc
                                   | otherwise      = spread (s { hours = s.hours - x } : srest)
                                                             (j { hours = j.hours - x } : jrest)
                                                             ({ day : s.day, task : j.task, hours : x } : acc)
                                                      where x = min s.hours j.hours

spread _ _ acc = reverse acc

spread' :: forall r. List { day :: Int, hours :: Number | r } -> List JHours -> List XHours
spread' a b = spread a b Nil

multiplyAllEfforts :: forall r. Number -> List { hours :: Number | r } -> List { hours :: Number | r }
multiplyAllEfforts f = map (\xh@{ hours : h } -> xh { hours = (h * f) })

totalEfforts :: forall r. List { hours :: Number | r } -> Number
totalEfforts = foldr (\{ hours } y -> hours + y) 0.0

groupByTask :: forall r. List { task :: String | r } -> List (NonEmptyList { task :: String | r })
groupByTask = groupBy (\{ task : taskA }
                        { task : taskB } ->
                       taskA == taskB)

spreadSonstiges :: List XHours -> List XHours
spreadSonstiges xhs = spread' shs jhours
                      where shs :: List XHours
                            shs = filter (\{ task } -> task == "Sonstiges") xhs

                            ghs :: List XHours
                            ghs = filter goodJob xhs

                            factoredghs :: List XHours
                            factoredghs = multiplyAllEfforts (totalEfforts shs / totalEfforts ghs) ghs

                            sumJHours :: forall r. NonEmptyList { hours :: Number, task :: String | r } -> JHours
                            sumJHours = foldr (\{ hours : hoursA, task }
                                                { hours : hoursB } ->
                                               { task, hours : hoursA + hoursB })
                                        { task : "", hours : 0.0 }

                            jhours :: List JHours
                            jhours = map sumJHours $ groupByTask $ factoredghs

-- Create list of jobs where each job is the name of a project or account and has a map of date and hours
-- (efforts)

type Efforts = M.Map Int Number

newtype Job = Job { job     :: String
                  , efforts :: Efforts }

derive instance eqJob :: Eq Job

-- cheap and dirty show for now
instance doshowJob :: Show Job where
  show = show <<< fold <<< map (\x -> x <> " ") <<< showJob (range 1 31)

xHoursToJobs :: List XHours -> List Job
xHoursToJobs = groupByTask >>> map (map xHoursToJob) >>> map mergeJobs
               where xHoursToJob :: XHours -> Job
                     xHoursToJob { day, task, hours } = Job { job : task , efforts : M.singleton day hours }

                     mergeJobs :: NonEmptyList Job -> Job
                     mergeJobs = foldr (\(Job { job : jobA, efforts : effortsA })
                                         (Job { job : jobB, efforts : effortsB }) ->
                                        (Job { job : jobA, efforts : effortsA <> effortsB }))
                                 (Job { job : "Nothing", efforts : M.empty })

filterDateRange :: List Int -> List Job -> List Job
filterDateRange days jobs = map (go days) jobs
  where go :: List Int -> Job -> Job
        go days' (Job j) = Job (j { efforts = filteredEfforts days' j.efforts })

        filteredEfforts :: List Int -> Efforts -> Efforts
        filteredEfforts days' efforts = foldMap (filterEfforts efforts) days'

        filterEfforts :: Efforts -> Int -> Efforts
        filterEfforts es day = maybe M.empty (M.singleton day) (M.lookup day es)

nonZeroP :: Job -> Boolean
nonZeroP (Job { efforts }) = or $ map ((/=) 0.0) $ M.values efforts

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

myShowN :: Number -> String
myShowN n = case (toGermanFloat (round100 n)) of
              Right n' -> n'
              Left  e  -> e

showJob' :: List Int -> Job -> List String
showJob' days (Job { job, efforts }) = job : foldMap (\d -> showEffort d : Nil) days
  where showEffort :: Int -> String
        showEffort day = case M.lookup day efforts of
                           Just n -> myShowN n
                           Nothing -> ""

showJob :: List Int -> Job -> List String
showJob days = showJob' days >>> formatRowForCATS >>> internalExternal

-- Move formatRowForCATS and internalExternal (using map) into higher level function used by showJobs and this higher
-- level function just processes header row differently than the remaining rows. Can I successively get showJobs from
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

-- Renamed to myGroupBy. Because there is a groupBy in Data.List which I might still need.

myGroupBy :: forall a. Int -> List (List a) -> List a -> List (List a)
myGroupBy n acc Nil = reverse acc
myGroupBy n acc r = myGroupBy n (take n r : acc) (drop n r)

showJobs :: List Int -> List Job -> List (List String)
showJobs days = (:) (showHeaderRow days) <<<
                intercalate emptyRow <<<
                myGroupBy 6 Nil <<<
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

table :: forall e. List (List String) -> MU.Markup e
table (h : rs) = go h <> foldMap row rs
  where row r = HTML.tr $ foldMap cell r
        cell i = HTML.td (MU.text i)
        cell' i = HTML.th (MU.text i)
        go h' = HTML.tr $ foldMap cell' h'
table rs = mempty

table' :: forall e. List (List (List String)) -> MU.Markup e
table' ts = HTML.table $ foldMap table ts

month :: Int -> List Int
month offset = range (1 - offset) 31

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
                              5 -> case weeksC' of
                                     Right w  -> table' w
                                     Left err -> MU.text err
                              otherwise -> mempty

  where raw        = switchEither "Error: Parsing CSV failed!" $ runParser s excelParsers.file
        parsed     = raw    >>= parsedFileToXHours
        month'     = parsed >>= xHoursToJobs >>> (\jobs -> let groups = myGroupBy 31 Nil (month 0) in
                                                   map (groupFilterShowNoFilter jobs) groups) >>> pure
        monthS     = parsed >>= filter goodJob >>> xHoursToJobs >>> (\jobs -> let groups = myGroupBy 7 Nil (month (toInt i)) in
                                                                      map (groupFilterShowStaggered jobs) groups) >>> pure
        weeks      = parsed >>= filter goodJob >>> xHoursToJobs >>>
                     (\jobs -> let groups = myGroupBy 7 Nil (month (toInt i)) in
                       map (groupFilterShowNoFilter jobs) groups) >>> pure
        weeksC     = parsed >>= filter goodJob >>> xHoursToJobs >>> (\jobs -> let groups = myGroupBy 7 Nil (month (toInt i)) in
                                                                       map (groupFilterShow jobs) groups) >>> pure
        weeksC'    = do pjs <- parsed
                        let jobs = xHoursToJobs $ (filter goodJob pjs) <> spreadSonstiges pjs
                        pure (let groups = myGroupBy 7 Nil (month (toInt i)) in
                               map (groupFilterShow jobs) groups)

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

renderToHTML :: String -> String
renderToHTML = TSRS.render <<< renderInput Monday 4

renderToHTML' :: String -> String
renderToHTML' = TSRS.render <<< renderInput Sunday 5

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

-- ui :: forall e e'. UI e (MU.Markup e')
ui :: forall t. UI (Free (MU.MarkupM t) Unit)
ui = renderInput <$>
     radioGroup "First of month" (Monday :| [Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]) show <*>
     intSlider "Morph" 0 5 5  <*>
     textarea "Raw Input" ""

main :: Effect Unit
main = runFlareHTML "controls" "output" ui
