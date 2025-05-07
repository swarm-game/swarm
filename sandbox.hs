import Data.Text.IO qualified as TIO
import Swarm.Language.Pipeline qualified as SLP


-- :reload Swarm.Language.Parser.Term
-- :l sandbox.hs
main :: IO ()
main = do
    putStrLn "Hello, Steven!"
    source <- TIO.readFile "data/test/language-snippets/errors/missing-end.sw"
    let result = SLP.processTerm source
    case result of 
        Left err -> do
            putStrLn $ show err
        Right syntax -> do
            putStrLn $ show syntax
    return ()