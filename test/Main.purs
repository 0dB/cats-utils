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

-- traverse_ (the version with underscore) throws away return value

main :: forall e. Eff (fs :: FS, exception :: EXCEPTION, console :: CONSOLE, assert :: ASSERT | e) Unit
main =
  do log ("Test: ")
     input <- readTextFile UTF8 ("test/input01.txt")
     let output = (Main.renderToHTML input) <> "\n"
     writeTextFile UTF8 ("test/output01.html") output
     reference <- readTextFile UTF8 ("test/reference01.html")
     assert (reference == output)
