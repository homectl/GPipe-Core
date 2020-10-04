module Graphics.GPipe.Internal.Debug where

import Data.List (intercalate)
import qualified Debug.Trace as Trace

traceIt :: Show a => String -> a -> a
traceIt t a = Trace.trace (t ++ " = " ++ show a) a

traceList :: Show a => String -> [a] -> [a]
traceList t as = Trace.trace (t ++ " = [\n\t" ++ intercalate "\n\t" (map show as) ++ "\n]") as
