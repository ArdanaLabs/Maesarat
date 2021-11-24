-- | Provides utilities.
--
-- @since 0.1.0.0
module Maesarat.Utils
  ( (<!>),
    (</>),
    mToE,
    errToM,
    showLeft,
  )
where

import Control.Applicative (Alternative (..))

-- $setup
-- >>> import Data.Functor (($>))

-- | @mx <!> my@ determines the first non-empty argument, base on its
-- underlying 'Alternative' instance. This is useful for combining monads that
-- we want to "short-circuit".
--
-- ==== __Examples__
-- >>> -- will not print second
-- >>> (putStrLn "first" $> (Just 7)) <!> (putStrLn "second" $> (Just 2))
-- Just 7
--
-- >>> -- _will_ print second
-- >>> (putStrLn "first" $> Nothing) <!> (putStrLn "second" $> (Just 2))
-- Just 2
--
-- >>> pure [] <!> pure [4,5,6]
-- [4,5,6]
--
-- @since 0.1.0.0
(<!>) :: (Alternative f, Eq (f a), Monad m) => m (f a) -> m (f a) -> m (f a)
mx <!> my = do
  x <- mx
  if x /= empty
    then mx
    else my

infixl 3 <!>

-- | @x </> y@ appends @x@ and @y@ together, taking care that we have
-- exactly one @/@ between them.
--
-- ==== __Examples __
-- >>> "one" </> "two"
-- "one/two"
--
-- >>> "one/" </> "two"
-- "one/two"
--
-- >>> "one" </> "/two"
-- "one/two"
--
-- >>> "one/" </> "/two"
-- "one/two"
--
-- @since 0.1.0.0
(</>) :: FilePath -> FilePath -> FilePath
l </> r = appendSlash l <> dropSlash r

infixr 5 </>

appendSlash :: FilePath -> FilePath
appendSlash [] = []
appendSlash ['/'] = ['/']
appendSlash [c] = [c, '/']
appendSlash (x : xs) = x : appendSlash xs

dropSlash :: FilePath -> FilePath
dropSlash [] = []
dropSlash ('/' : xs) = xs
dropSlash xs = xs

-- | Maps 'Either' to 'Maybe' for when we do not care about the return value,
-- only the presence of errors.
--
-- >>> errToM (Left "error")
-- Just "error"
--
-- >>> errToM @String (Right True)
-- Nothing
--
-- @since 0.1.0.0
errToM :: Either err a -> Maybe err
errToM (Left err) = Just err
errToM _ = Nothing

-- | Maps 'Maybe' to 'Either'.
--
-- @since 0.1.0.0
mToE :: e -> Maybe a -> Either e a
mToE err Nothing = Left err
mToE _ (Just x) = Right x

-- | Show for @'Either' e a@ for when we do not have a @Show a@ constraint.
--
-- @since 0.1.0.0
showLeft :: Show e => Either e a -> String
showLeft (Left e) = "Left " <> show e
showLeft (Right _) = "Right _"
