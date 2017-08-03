
import Control.Lens.Operators
import Data.List
import Data.Set(Set(..))
import Data.Text(Text(..))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead

main = do
    numCases <- getDecimal
    sequence $ Data.List.replicate numCases doTestCase

doTestCase :: IO ()
doTestCase = do
    text <- TextIO.getLine
    index <- getIndex
    let char = getFromSubsequences index text in
        putChar char >> putChar '\n'

getFromSubsequences :: Int -> Text -> Char
getFromSubsequences index text = (Text.concat . Set.toList . textSubsequences) text `Text.index` index

textSubsequences :: Text -> Set Text
textSubsequences text
  | Text.null text = Set.empty
  -- | otherwise = Set.fromList (Text.tails text) `Set.union` textSubsequences (Text.dropEnd 1 text)
  | otherwise = foldr Set.insert (textSubsequences $ Text.dropEnd 1 text) (Text.tails text)

getIndex :: IO Int
getIndex = getDecimal <&> subtract 1

getDecimal :: IO Int
getDecimal = TextIO.getLine <&> \text -> either error fst (TextRead.decimal text)
