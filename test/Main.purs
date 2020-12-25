module Test.Main where

import Data.List (List(..), (:))
import Data.Traversable (foldMap)
import Effect (Effect)
import Effect.Console (log)
import Main (JHours, XHours, multiplyAllEfforts, renderToHTML, renderToHTML', spread', totalEfforts)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Prelude (Unit, bind, discard, show, ($), (<>), (==))
import Test.Assert (assert)

testS :: List XHours
testS = ({ day : 1, hours : 10.0, task : "" }
         : { day : 2, hours : 10.0, task : "" }
         : { day : 3, hours : 10.0, task : "" }
         : Nil )

testJ1 :: List JHours
testJ1 = ({ task : "task 1", hours : 15.0 }
          : { task : "task 2", hours : 15.0 }
          : Nil )

testX1 :: List XHours
testX1 = ({ day : 1, task : "task 1", hours : 10.0 }
          : { day : 2, task : "task 1", hours : 5.0 }
          : { day : 2, task : "task 2", hours : 5.0 }
          : { day : 3, task : "task 2", hours : 10.0 }
          : Nil )

testJ2 :: List JHours
testJ2 = ({ task : "task 1", hours : 7.5 }
          : { task : "task 2", hours : 7.5 }
          : { task : "task 3", hours : 7.5 }
          : { task : "task 4", hours : 7.5 }
          : Nil )

testX2 :: List XHours
testX2 = ({ day : 1, task : "task 1", hours :  7.5 }
          : { day : 1, task : "task 2", hours :  2.5 }
          : { day : 2, task : "task 2", hours :  5.0 }
          : { day : 2, task : "task 3", hours :  5.0 }
          : { day : 3, task : "task 3", hours :  2.5 }
          : { day : 3, task : "task 4", hours :  7.5 }
          : Nil )

testX1Half :: List XHours
testX1Half = ({ day : 1, task : "task 1", hours : 5.0 }
              : { day : 2, task : "task 1", hours : 2.5 }
              : { day : 2, task : "task 2", hours : 2.5 }
              : { day : 3, task : "task 2", hours : 5.0 }
              : Nil )

main :: Effect Unit
main =
  do input0 <- readTextFile UTF8 ("test/input00.txt")
     let output0  = (renderToHTML input0) <> "\n"
         output01 = (renderToHTML' input0) <> "\n"
         output1 = spread' testS testJ1
         output2 = spread' testS testJ2
     writeTextFile UTF8 ("test/output00.html") output0
     writeTextFile UTF8 ("test/output01.html") output01
     reference0  <- readTextFile UTF8 ("test/reference00.html")
     reference01 <- readTextFile UTF8 ("test/reference01.html")
     log $ foldMap (\x -> show x <> "\n") output1
     log $ foldMap (\x -> show x <> "\n") output2
     assert (reference0  == output0) -- testing main function (data that is used for CATS)
     assert (reference01 == output01)
     assert (output1 == testX1)     -- testing new spread function
     log "testing x2"
     assert (output2 == testX2)
     -- part of new spread functions
     assert (multiplyAllEfforts 0.5 testX1 == testX1Half)
     assert (totalEfforts testX1 == 30.0)
