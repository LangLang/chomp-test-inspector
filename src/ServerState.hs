module ServerState (ServerState(..)) where

-- State of the server process (used by message dispatcher(s) and the termination loop)
data ServerState = Active | Terminating | Terminated
  deriving Eq
