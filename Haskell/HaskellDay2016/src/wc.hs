import Control.Applicative
import Data.Traversable
import Lens.Family (to)
import Control.Foldl (Fold)
import Data.Text (Text)
import Prelude hiding (lines, words)

import qualified Control.Foldl      as Fold
import qualified Control.Foldl.Text as Fold.Text
import qualified Data.Text          as Text
import qualified Turtle

lines, words, chars :: Fold Text Int
lines = Fold.length
words = Fold.handles (to Text.words . traverse) Fold.length
chars = Fold.Text.length + lines

total :: Fold Text (Int, Int, Int)
total = (,,) <$> lines <*> words <*> chars

main :: IO ()
main = do
    x <- Turtle.fold Turtle.stdin total
    print x
