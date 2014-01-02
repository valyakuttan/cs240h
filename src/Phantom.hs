
module Main where
import Data.IORef

newtype Ref t a = Ref (IORef a)

data ReadOnly
data ReadWrite

newRef :: a -> IO (Ref ReadWrite a)
newRef a = Ref `fmap` newIORef a

readRef :: Ref t a -> IO a
readRef (Ref ref) = readIORef ref

writeRef :: Ref ReadWrite a -> a -> IO ()
writeRef (Ref ref) v = writeIORef ref v

readOnly :: Ref t a -> Ref ReadOnly a
readOnly (Ref ref) = Ref ref

data Expr a = Num Int
            | Str String
            | Op BinOp (Expr a) (Expr a)
              deriving (Show)

-- This is unchanged.
data BinOp = Add | Concat
             deriving (Show)

interp :: Expr a -> Expr a
interp x@(Num _)       = x
interp x@(Str _)       = x
interp (Op Add a b)    = Num (i a + i b)
  where i x = case interp x of Num a -> a
interp (Op Concat a b) = Str (i a ++ i b)
  where i x = case interp x of Str y -> y

