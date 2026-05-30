{- HLINT ignore "Avoid restricted function" -}
module ImportChain (benchImportChain, genImportChain) where

import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import Control.Monad (forM_)
import Data.Text.IO qualified as T -- GHC >9.6 use .Utf8
import Swarm.Failure (SystemFailure)
import Swarm.Language.Cache
import Swarm.Language.Module (Module)
import Swarm.Language.Pipeline (processSource)
import Swarm.Language.Syntax (Elaborated)
import Swarm.Pretty (prettyString)
import Swarm.Util.GlobalCache
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO)

benchImportChain :: Benchmark
benchImportChain =
  bgroup
    "Import chain"
    [ bench "import a_000" $ whnfAppIO importFile 0
    , bench "import a_001" $ whnfAppIO importFile 1
    , bench "import a_010" $ whnfAppIO importFile 10
    , bench "import a_020" $ whnfAppIO importFile 20
    , bench "import a_050" $ whnfAppIO importFile 50
    , bench "import a_100" $ whnfAppIO importFile 100
    ]

longImports :: String
longImports = "example/long_import/"

importFile :: Int -> IO (Module Elaborated)
importFile i = do
  let path = pathA i
  resetCache moduleCache
  importText <- T.readFile path
  res <- runM . runError @SystemFailure $ processSource (Just path) Nothing importText
  case res of
    Left e -> fail $ "Failed to process " <> path <> ": " <> prettyString e
    Right v -> pure v

genImportChain :: IO ()
genImportChain = do
  writeFile (pathA 0) "def a_000 = 0 end\n"
  forM_ [1 .. 100] $ \i ->
    writeFile (pathA i) $
      unlines
        [ "import \"" <> fmtA (i - 1) <> "\""
        , unwords ["def", fmtA i, "= 1 +", fmtA (i - 1), "end"]
        ]

fmtA :: Int -> String
fmtA i = let x = show i in "a_" <> replicate (3 - length x) '0' <> x

pathA :: Int -> String
pathA i = longImports <> "/" <> fmtA i <> ".sw"
