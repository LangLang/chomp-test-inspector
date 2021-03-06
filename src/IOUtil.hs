module IOUtil (foreverUntilIO, foldUntilIO, foreverWhileIO) where

-- Repeat an IO action until another IO operation returns true 
foreverUntilIO :: IO Bool -> IO () -> IO ()
foreverUntilIO testIO loopIO = do
  cond <- loopIO >> testIO
  if cond
    then return ()
    else foreverUntilIO testIO loopIO

-- Repeat an IO action until another IO operation returns true 
foldUntilIO :: (a -> b -> a) -> a -> IO Bool -> IO b -> IO a
foldUntilIO f a testIO loopIO = do
  b <- loopIO
  cond <- testIO
  let a' = f a b
  if cond
    then return a'
    else foldUntilIO f a' testIO loopIO

-- Repeat an IO action every time an IO operation returns true until it doesn't 
foreverWhileIO :: IO Bool -> IO () -> IO ()
foreverWhileIO check loop = do
  cond <- check
  if cond
    then loop >> (foreverWhileIO check loop)
    else return ()
