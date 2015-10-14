import Control.Arrow ( first, second )
import Control.Applicative ( (<$>) )
import Data.Function ( on )
import Data.List ( groupBy, intercalate )
import System.FilePath ( (<.>) )

ext = "tsv"
baseName = "penrose_data"

main = do
    inputLines <- (map takeName) . lines <$> readFile (baseName <.> ext)
    let groups = groupBy ((==) `on` fst) inputLines
    mapM_ doWriteFile groups

takeName :: String -> (String, [String])
takeName = first head . splitAt 1 . splitOn '\t'

splitOn :: Char -> String -> [String]
splitOn needle haystack = case foldr go ([], []) haystack of
                    (res, cs) -> if not (null cs) then cs : res else res
  where
    go :: Char -> ([String], String) -> ([String], String)
    go c (res, nonTabs)
        | c == needle = if not (null nonTabs)
                            then (nonTabs : res, [])
                            else (res, [])
        | otherwise = (res, c : nonTabs)

doWriteFile lines =
    let nameContents = map (second $ intercalate "\t") lines
        name = fst . head $ nameContents
        contents = map snd nameContents
        fileName = baseName ++ "_" ++ (map escapeName name) <.> ext
    in writeFile fileName $ unlines contents

escapeName '_' = '-'
escapeName c = c
