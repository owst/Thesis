import Control.Applicative ( (<$>) )
import Control.Monad ( forM, forM_ )
import Data.List ( intercalate )
import System.Process ( callCommand )
import System.Directory ( removeFile )
import Text.Printf( printf )

toRun = [ ("buffer-slow", stringify [3,6,9,12])
        , ("token_ring", stringify [1,2,4,5])
        , ("conjunction_tree", ["1 10", "2 4", "4 2", "12 2"])
        , ("disjunction_tree", ["1 8", "2 3", "3 2", "15 1"])
        , ("clique", stringify [2,4,7,9])
        , ("subsets", stringify [3, 6, 9, 12])
        , ("over", stringify [1,3,4,5])
        , ("hartstone", stringify [2, 4, 8, 10])
        , ("iterated_choice", stringify [1,2,3,4])
        , ("replicator", stringify [1,3,6,8])
        , ("dac", stringify [10, 25, 50, 75])
        , ("philo", stringify [1,2,4,6])
        , ("cyclic", stringify [1,2,4,6])
        , ("counter", stringify [1,2,4,8])
        ]
  where
    stringify = map show

exec :: String -> String -> IO (String, Double, Bool)
exec name param = do
    let penrose = "./dist/build/Penrose/Penrose Comp_NFASlow "
    callCommand $ concat [ penrose
                         , "examples/"
                         , name
                         , ".netdecomp "
                         , param
                         , " > output"
                         ]
    -- Get the maximum composition size
    callCommand
        "tail -n2 output | head -n1 | sed 's/.*: //' > maxComp"
    -- Remove the leading space and trailing 's' from the runtime
    callCommand
        "tail -n8 output | head -n1 | sed 's/\\(\\s*\\|s\\)//g' > time"
    callCommand
        "grep doublecircle output > reachable || true"

    maxComp <- head . lines <$> readFile "maxComp"
    time <- read <$> readFile "time"
    reachable <- (not . null) <$> readFile "reachable"
    mapM_ removeFile ["time", "maxComp", "output", "reachable"]
    return (maxComp, time, reachable)

runIt :: String -> String -> IO (String, Double, Bool)
runIt name param = do
    let howMany = 5
    results <- forM [1..howMany] $ const (exec name param)
    let (maxComp, reachable) = (\(c, _, r) -> (c, r)) . head $ results
        avgTime = (/ howMany) . sum . map (\(_, x, _) -> x) $ results
    return (maxComp, avgTime, reachable)

main =
    forM_ toRun $ \(name, args) ->
    forM_ args $ \arg -> do
        (size, time, reachable) <- runIt name arg
        putStrLn . intercalate "\t" $
            [name, arg, size, printf "%.3f" time, show reachable]
