-- this will cause an error:
-- module Main (fromGermanFloat, toGermanFloat, round100) where

-- OLD: create HTML with `pulp build -O -t main.js`

-- NEW: create HTML with `spago bundle-app`

-- TODO 2020-11 Write test case and then add data type for one row. Then extend by one more field in the
-- row.

module Main (JHours(..), XHours(..), SHours(..), spread', renderToHTML, renderToHTML', main, Job(Job), Efforts, multiplyAllEfforts, totalEfforts) where

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

month :: Int -> List Int
month offset = range (1 - offset) 31

daysInWeek = 7 :: Int

-- Group a list into groups of certain size, returning list of lists

-- Renamed to myGroupBy. Because there is a groupBy in Data.List which I might still need.

myGroupBy :: forall a. Int -> List (List a) -> List a -> List (List a)
myGroupBy n acc Nil = reverse acc
myGroupBy n acc r = myGroupBy n (take n r : acc) (drop n r)

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

type Efforts = M.Map Int Number

newtype Job = Job { job     :: String
                  , efforts :: Efforts }

derive instance eqJob :: Eq Job

-- cheap and dirty show for now
instance doshowJob :: Show Job where
  show = show <<< fold <<< map (\x -> x <> " ") <<< showJob (range 1 31)

newtype SHours = SHours { day   :: Int
                        , hours :: Number }

newtype JHours = JHours { task  :: String
                        , hours :: Number }

newtype XHours = XHours { day   :: Int
                        , task  :: String
                        , hours :: Number }

derive instance eqXHours :: Eq XHours

instance showXHours :: Show XHours where
  show (XHours x) = show (x.day) <> " " <> x.task <> " " <> show (x.hours)

parsedFileToJobs :: List (List String) -> Either String (List XHours)
parsedFileToJobs ((_ : dates) : joblines) = readDates dates >>= readJobs (filter notEmpty $ joblines)

  where readDay :: String -> Either String Int
        readDay d | Just date <- fromString d = Right date
                  | otherwise                 = Left ("Error: Could not read date " <> d <> ".")

        readDates :: List String -> Either String (List Int)
        readDates ds = sequence $ map readDay ds

        readHour :: String -> Either String Number
        readHour = fromGermanFloat

        readJob :: List Int -> List String -> Either String (List XHours)
        readJob _   Nil          = Left "Error: No input line."
        readJob _  (job : Nil)   = Left ("Error: No efforts found after job name " <> job <> ".")
        readJob ds (job : hours) = do hs <- sequence $ map readHour hours
                                      pure $ zipWith (\d h -> XHours { task : job, day : d, hours : h  } )  ds hs

        readJobs :: List (List String) -> List Int -> Either String (List XHours)
        readJobs js ds = do xhs <- sequence $ map (readJob ds) js
                            pure $ fold xhs

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
nonZeroP (Job { efforts }) = or $ map ((/=) 0.0) $ M.values efforts

goodJob :: XHours -> Boolean
goodJob (XHours { task }) = isNothing (elemIndex task badtasks)

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

-- PureScript uses <<< rather than . for right-to-left composition of functions. This is to avoid a
-- syntactic ambiguity with . being used for property access and name qualification. There is also a
-- corresponding >>> operator for left-to-right composition.
-- https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md
-- (f <<< g) x = f (g x)
-- https://stackoverflow.com/questions/29881695/what-does-the-triple-less-than-sign-do-in-purescript

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

-- toUnfoldable :: M.Map Int String -> List (Tuple Int String)

-- how to turn List into Map
-- efforts : fold (map (\e -> M.singleton 0 e) es'')
-- or just
-- efforts : fold (map (M.singleton 0) es'')

-- TIL Data.Map is also traversable so I can apply sequence()

xHoursToSHours :: List XHours -> List SHours
xHoursToSHours = map (\(XHours { hours, day } ) -> SHours { day, hours })

xHoursToJHours :: List XHours -> List JHours
xHoursToJHours = map (\(XHours { hours, task } ) -> JHours { task, hours })

sumJHours :: NonEmptyList JHours -> JHours
sumJHours = foldr (\(JHours { task : taskA, hours : hoursA })
                    (JHours { task : taskB, hours : hoursB }) ->
                   JHours { task : taskA, hours : hoursA + hoursB })
            (JHours { task : "None", hours : 0.0 })

groupJHours :: List JHours -> List (NonEmptyList JHours)
groupJHours = groupBy (\(JHours { task : taskA })
                        (JHours { task : taskB }) ->
                       taskA == taskB)

totalEfforts :: List XHours -> Number
totalEfforts = foldr (\(XHours { hours } ) y -> hours + y) 0.0

multiplyAllEfforts :: Number -> List XHours -> List XHours
multiplyAllEfforts x = map (\(XHours { task, hours : h, day } ) -> XHours { hours : (h * x), day, task })

spread :: List SHours -> List JHours -> List XHours -> List XHours
spread (SHours s : srest) (JHours j : jrest) acc | s.hours <= 0.0 = spread srest              (JHours j : jrest) acc
                                                 | j.hours <= 0.0 = spread (SHours s : srest) jrest              acc
                                                 | otherwise      = spread (SHours s { hours = s.hours - x } : srest)
                                                                           (JHours j { hours = j.hours - x } : jrest)
                                                                           (XHours { day : s.day, task : j.task, hours : x } : acc)
                                                                      where x = min s.hours j.hours

spread _ _ acc = reverse acc

spread' :: List SHours -> List JHours -> List XHours
spread' a b = spread a b Nil

xHoursToJob :: XHours -> Job
xHoursToJob (XHours { day, task, hours }) = Job { job : task , efforts : M.singleton day hours }

groupxhs :: List XHours -> List (NonEmptyList XHours)
groupxhs = groupBy (\(XHours { task : taskA })
                     (XHours { task : taskB }) ->
                    taskA == taskB)

mergeJobs :: NonEmptyList Job -> Job
mergeJobs = foldr (\(Job { job : jobA, efforts : effortsA })
                    (Job { job : jobB, efforts : effortsB }) ->
                   (Job { job : jobA, efforts : effortsA <> effortsB }))
            (Job { job : "Nothing", efforts : M.empty })

xHoursToJobs :: List XHours -> List Job
xHoursToJobs = groupxhs >>> map (map xHoursToJob) >>> map mergeJobs

spreadSonstiges :: List XHours -> List XHours
spreadSonstiges xhs = spread' shours jhours
                      where shs = filter (\(XHours { task }) -> task == "Sonstiges") xhs
                            ghs = filter goodJob xhs

                            hoursghs = totalEfforts ghs
                            hoursshs = totalEfforts shs

                            factor = hoursshs / hoursghs
                            factoredghs = multiplyAllEfforts factor ghs

                            -- get number of Sonstiges hours that need to be distributed
                            shours :: List SHours
                            shours = xHoursToSHours shs

                            -- get matching number of weighted job hours
                            jhours :: List JHours
                            jhours = map sumJHours $ groupJHours $ xHoursToJHours factoredghs

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
                              5 -> case weeksC' of
                                     Right w  -> table' w
                                     Left err -> MU.text err
                              otherwise -> mempty

  where raw        = switchEither "Error: Parsing CSV failed!" $ runParser s excelParsers.file
        parsed     = raw    >>= parsedFileToJobs
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
