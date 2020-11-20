module Test.Main where

import Prelude
-- import Prelude (Unit, bind, discard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (assert, ASSERT)

-- from ConvertFile.purs
import Main (renderToHTML) as Main
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Encoding (Encoding(..)) -- 2017-02-28 this provides `UTF8` (required since PS update)

-- for spread function
import Data.List (List(..), reverse, (:))
-- from https://discourse.purescript.org/t/issue-with-simple-code-from-purescript-by-example/231
derive instance eqXHours :: Eq XHours

-- traverse_ (the version with underscore) throws away return value

-- for splitting "sonstiges"
-- TODO 2020-11 Move this to own module

newtype SHours = SHours { day   :: String
                        , hours :: Number }

newtype JHours = JHours { task  :: String
                        , hours :: Number }

newtype XHours = XHours { day   :: String
                        , task  :: String
                        , hours :: Number }

testS :: List SHours
testS = ( SHours { day : "day 1", hours : 10.0 }
        : SHours { day : "day 2", hours : 10.0 }
        : SHours { day : "day 3", hours : 10.0 }
        : Nil)

testJ1 :: List JHours
testJ1 = ( JHours { task : "task 1", hours : 15.0 }
         : JHours { task : "task 2", hours : 15.0 }
         : Nil)

testX1 :: List XHours
testX1 = ( XHours { day : "day 1", task : "task 1", hours : 10.0}
         : XHours { day : "day 2", task : "task 1", hours : 5.0}
         : XHours { day : "day 2", task : "task 2", hours : 5.0}
         : XHours { day : "day 3", task : "task 2", hours : 10.0}
         : Nil)

testJ2 :: List JHours
testJ2 = ( JHours { task : "task 1", hours : 15.0 }
         : JHours { task : "task 2", hours : 15.0 }
         : JHours { task : "task 3", hours : 15.0 }
         : Nil)

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


main :: forall e. Eff (fs :: FS, exception :: EXCEPTION, console :: CONSOLE, assert :: ASSERT | e) Unit
main =
  do log ("Test: ")
     input <- readTextFile UTF8 ("test/input01.txt")
     let output = (Main.renderToHTML input) <> "\n"
     writeTextFile UTF8 ("test/output01.html") output
     reference <- readTextFile UTF8 ("test/reference01.html")
     assert (reference == output)
     assert (spread' testS testJ1 == testX1)
