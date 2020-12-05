module Test.Main where

import Prelude (Unit, bind, discard, show, ($), (<>), (==))
import Main (JHours(..), SHours(..), XHours(..), renderToHTML, spread')
import Data.List (List(..), (:))

import Effect (Effect)
import Effect.Console (log)

import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Encoding (Encoding(..))

import Test.Assert (assert)

import Data.Traversable (foldMap)

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

main :: Effect Unit
main =
  do input0 <- readTextFile UTF8 ("test/input00.txt")
     let output0 = (renderToHTML input0) <> "\n"
         output1 = spread' testS testJ1
         output2 = spread' testS testJ2
     writeTextFile UTF8 ("test/output00.html") output0
     reference0 <- readTextFile UTF8 ("test/reference00.html")
     log $ foldMap (\x -> show x <> "\n") output1
     log $ foldMap (\x -> show x <> "\n") output2
     assert (reference0 == output0) -- testing main function (data that is used for CATS)
     assert (output1 == testX1)     -- testing new spread function
     assert (output2 == testX2)
