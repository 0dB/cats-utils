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

main :: forall e. Eff (fs :: FS, exception :: EXCEPTION, console :: CONSOLE, assert :: ASSERT | e) Unit
main =
  do log ("Test: ")
     input <- readTextFile UTF8 ("test/input01.txt")
     let output = (renderToHTML input) <> "\n"
         output2 = spread' testS testJ1
     writeTextFile UTF8 ("test/output01.html") output
     reference <- readTextFile UTF8 ("test/reference01.html")
     log $ foldMap (\x -> show x <> "\n") output2
     assert (reference == output)
     assert (output2 == testX1)
