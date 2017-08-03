
-- Reader r a
-- where r is some “environment” and a is some value you create from that environment. And thanks to the type alias above you can just about do that.

import Control.Monad.Reader

tom :: Reader String String
tom = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
    t <- tom
    j <- jerry
    return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = (runReader tomAndJerry) "Who is this?"


-- Imagine this is a directory
type Config = FilePath

load :: Config -> String -> IO String
load config x = readFile (config ++ x)

loadRevision :: Config -> Int -> IO String
loadRevision config x = load config ("history" ++ show x ++ ".txt")

loadAll :: Config -> Int -> String -> IO (String, String)
loadAll config x y = do
    a <- load config y
    b <- loadRevision config x
    return (a, b)

main = pure ()
