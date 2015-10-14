import Data.Maybe
import Data.Ord
import Data.List
import Data.List.Split
import Safe
import System.Directory
import System.Environment

main = interact $ unlines . map highlight . lines

highlight inputLine =
    let fields = splitOn "\t" inputLine
        compareFields x y =
            case (readMay x, readMay y) of
                (Nothing, Nothing) -> EQ
                (Nothing, Just _) -> GT
                (Just _, Nothing) -> LT
                (Just x', Just y') -> compare x' (y' :: Double)
        minimum = minimumBy compareFields fields
        highlighted = map (doHighlight minimum) fields
    in intercalate "\t" highlighted

doHighlight min f =
    let f' = if isMin f then wrap "highlightedResult" f else f
    in if isFail f then wrap "failureResult" f' else f'
  where
    isMin =
        if isJust (readMay min :: Maybe Double) then (== min) else const False

wrap latexName f = "\\" ++ latexName ++ "{" ++ f ++ "}"

isFail f = (readMay f :: Maybe Double) == Nothing
