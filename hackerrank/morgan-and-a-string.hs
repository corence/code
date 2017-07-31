import Control.Applicative

getRead :: Read a => IO a
getRead = read <$> getLine

main = do
  numCases <- getRead
  sequence (replicate numCases doTestCase)

doTestCase :: IO ()
doTestCase = do
  let output = liftA2 meldStrings getLine getLine
  output >>= putStrLn

meldStrings :: String -> String -> String
meldStrings [] [] = []
meldStrings [] bs = bs
meldStrings as [] = as
meldStrings (a:as) (b:bs)
  | a < b = a : meldStrings as (b:bs)
  | otherwise = b : meldStrings (a:as) bs
