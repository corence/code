
import qualified Data.Text as Text
import Data.Text(Text(..))
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import Data.List
import Control.Monad
import Control.Lens.Operators
import Control.Monad.Reader
import qualified Data.Set as Set
import Data.Set(Set(..))

main = do
    numCasesLine <- TextIO.getLine
    let numCasesOrNot = TextRead.decimal numCasesLine
    case numCasesOrNot of
        Left err -> error err
        Right (numCases, chaff) ->
            sequence $ Data.List.replicate numCases doTestCase

doTestCase :: IO ()
doTestCase = do
    text <- TextIO.getLine
    index <- getIndex
    let char = getFromSubsequences index text in
        putChar char >> putChar '\n'

--textSubsequences :: Text -> [Text]
--textSubsequences

getFromSubsequences :: Int -> Text -> Char
getFromSubsequences index text = (Text.concat . map head . group . sort . textSubsequences) text `Text.index` index

textSubsequences :: Text -> [Text]
textSubsequences x = [x]

-- filters duplicates from a sorted list
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:[]) = [x]
uniq (x:y:zs)
  | x == y = uniq (x:zs)
  | otherwise = x : uniq (y:zs)

getIndex :: IO Int
getIndex = TextIO.getLine <&> \text -> either error (subtract 1 . fst) (TextRead.decimal text)
