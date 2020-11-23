module Test.Main where

import Prelude (Unit, bind, discard, show, ($), (<>), (==))
import Main (JHours(..), SHours(..), XHours(..), renderToHTML, spread')
import Data.List (List(..), (:))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Encoding (Encoding(..))

import Test.Assert (assert, ASSERT)

import Data.Traversable -- (sequence, or, foldMap, fold)

testS :: List SHours
testS = ( SHours { day : "day 1", hours : 10.0 }
        : SHours { day : "day 2", hours : 10.0 }
        : SHours { day : "day 3", hours : 10.0 }
        : Nil )

testJ1 :: List JHours
testJ1 = ( JHours { task : "task 1", hours : 15.0 }
         : JHours { task : "task 2", hours : 15.0 }
         : Nil )

testX1 :: List XHours
testX1 = ( XHours { day : "day 1", task : "task 1", hours : 10.0 }
         : XHours { day : "day 2", task : "task 1", hours : 5.0 }
         : XHours { day : "day 2", task : "task 2", hours : 5.0 }
         : XHours { day : "day 3", task : "task 2", hours : 10.0 }
         : Nil )

testJ2 :: List JHours
testJ2 = ( JHours { task : "task 1", hours : 7.5 }
         : JHours { task : "task 2", hours : 7.5 }
         : JHours { task : "task 3", hours : 7.5 }
         : JHours { task : "task 4", hours : 7.5 }
         : Nil )

testX2 :: List XHours
testX2 = ( XHours { day : "day 1", task : "task 1", hours :  7.5 }
         : XHours { day : "day 1", task : "task 2", hours :  2.5 }
         : XHours { day : "day 2", task : "task 2", hours :  5.0 }
         : XHours { day : "day 2", task : "task 3", hours :  5.0 }
         : XHours { day : "day 3", task : "task 3", hours :  2.5 }
         : XHours { day : "day 3", task : "task 4", hours :  7.5 }
         : Nil )

main :: forall e. Eff (fs :: FS, exception :: EXCEPTION, console :: CONSOLE, assert :: ASSERT | e) Unit
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
