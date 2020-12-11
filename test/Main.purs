module Test.Main where

import Data.List (List(..), (:), zipWith)
import Data.Map as M
import Data.Traversable (foldMap, fold)
import Effect (Effect)
import Effect.Console (log)
import Main (JHours(..), Job(Job), SHours(..), XHours(..), multiplyAllEfforts, renderToHTML, renderToHTML', spread', totalEfforts)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Prelude (Unit, bind, discard, show, ($), (<>), (==))
import Test.Assert (assert)

testS :: List SHours
testS = ( SHours { day : 1, hours : 10.0 }
        : SHours { day : 2, hours : 10.0 }
        : SHours { day : 3, hours : 10.0 }
        : Nil )

testJ1 :: List JHours
testJ1 = ( JHours { task : "task 1", hours : 15.0 }
         : JHours { task : "task 2", hours : 15.0 }
         : Nil )

testX1 :: List XHours
testX1 = ( XHours { day : 1, task : "task 1", hours : 10.0 }
         : XHours { day : 2, task : "task 1", hours : 5.0 }
         : XHours { day : 2, task : "task 2", hours : 5.0 }
         : XHours { day : 3, task : "task 2", hours : 10.0 }
         : Nil )

testJ2 :: List JHours
testJ2 = ( JHours { task : "task 1", hours : 7.5 }
         : JHours { task : "task 2", hours : 7.5 }
         : JHours { task : "task 3", hours : 7.5 }
         : JHours { task : "task 4", hours : 7.5 }
         : Nil )

testX2 :: List XHours
testX2 = ( XHours { day : 1, task : "task 1", hours :  7.5 }
         : XHours { day : 1, task : "task 2", hours :  2.5 }
         : XHours { day : 2, task : "task 2", hours :  5.0 }
         : XHours { day : 2, task : "task 3", hours :  5.0 }
         : XHours { day : 3, task : "task 3", hours :  2.5 }
         : XHours { day : 3, task : "task 4", hours :  7.5 }
         : Nil )

dummyJob :: Job
dummyJob = Job { job : "dummy"
               , efforts : fold $ zipWith M.singleton (15 : 5 : 1 : Nil) (15.0 : 5.0 : 1.0 : Nil) }

dummyJobHalf :: Job
dummyJobHalf = Job { job : "dummy"
                   , efforts : fold $ zipWith M.singleton (15 : 5 : 1 : Nil) (7.5 : 2.5 : 0.5: Nil) }

main :: Effect Unit
main =
  do input0 <- readTextFile UTF8 ("test/input00.txt")
     let output0 = (renderToHTML input0) <> "\n"
         output01 = (renderToHTML' input0) <> "\n" -- new spread function. Not working correctly yet
         output1 = spread' testS testJ1
         output2 = spread' testS testJ2
     writeTextFile UTF8 ("test/output00.html") output0
     writeTextFile UTF8 ("test/output01.html") output01
     reference0 <- readTextFile UTF8 ("test/reference00.html")
     log $ foldMap (\x -> show x <> "\n") output1
     log $ foldMap (\x -> show x <> "\n") output2
     assert (reference0 == output0) -- testing main function (data that is used for CATS)
     assert (output1 == testX1)     -- testing new spread function
     log "testing x2"
     assert (output2 == testX2)
     log $ show dummyJob
     -- part of new spread functions
     assert (multiplyAllEfforts 0.5 dummyJob == dummyJobHalf)
     assert (totalEfforts (dummyJob : Nil) == 21.0)
