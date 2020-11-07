module Graphics.GPipe.Internal.Debug where

import Control.Monad
import Data.List (intercalate, isInfixOf)
import qualified Debug.Trace as Trace
import Graphics.GL.Core45
import Graphics.GL.Types

traceIt :: Show a => String -> a -> a
traceIt t a = Trace.trace (t ++ " = " ++ show a) a

traceList :: Show a => String -> [a] -> [a]
traceList t as = Trace.trace (t ++ " = [\n\t" ++ intercalate "\n\t" (map show as) ++ "\n]") as

checkGlError :: String -> IO ()
checkGlError title = do
    e <- glGetError
    when (e /= GL_NO_ERROR) $ do
        error $ "[" ++ title ++ "] GL error -> " ++ getErrorMessage e

getErrorMessage :: GLenum -> String
getErrorMessage errorCode = maybe ("Unknown error code " ++ show errorCode) fst $ lookup errorCode
    [ (GL_NO_ERROR, ("GL_NO_ERROR", "-"))
    , (GL_INVALID_ENUM, ("GL_INVALID_ENUM", "Given when an enumeration parameter is not a legal enumeration for that function. This is given only for local problems; if the spec allows the enumeration in certain circumstances, where other parameters or state dictate those circumstances, then GL_INVALID_OPERATION is the result instead."))
    , (GL_INVALID_VALUE, ("GL_INVALID_VALUE", "Given when a value parameter is not a legal value for that function. This is only given for local problems; if the spec allows the value in certain circumstances, where other parameters or state dictate those circumstances, then GL_INVALID_OPERATION is the result instead."))
    , (GL_INVALID_OPERATION, ("GL_INVALID_OPERATION", "Given when the set of state for a command is not legal for the parameters given to that command. It is also given for commands where combinations of parameters define what the legal parameters are."))
    , (GL_STACK_OVERFLOW, ("GL_STACK_OVERFLOW", "Given when a stack pushing operation cannot be done because it would overflow the limit of that stack's size."))
    , (GL_STACK_UNDERFLOW, ("GL_STACK_UNDERFLOW", "Given when a stack popping operation cannot be done because the stack is already at its lowest point."))
    , (GL_OUT_OF_MEMORY, ("GL_OUT_OF_MEMORY", "Given when performing an operation that can allocate memory, and the memory cannot be allocated. The results of OpenGL functions that return this error are undefined; it is allowable for partial execution of an operation to happen in this circumstance."))
    , (GL_INVALID_FRAMEBUFFER_OPERATION, ("GL_INVALID_FRAMEBUFFER_OPERATION", "Given when doing anything that would attempt to read from or write/render to a framebuffer that is not complete."))
    , (GL_CONTEXT_LOST, ("GL_CONTEXT_LOST", "Given if the OpenGL context has been lost, due to a graphics card reset."))
    ]
