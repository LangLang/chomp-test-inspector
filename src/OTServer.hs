-- This module more or less replaces Control.OperationTransform.Server
-- The FileStore requires operational transforms to be merged separately from applying it to the 
-- file data in order to keep only the last saved (persistent) revision of the file data in the
-- cache. (This allows the file store to perform a diff with the file stored on disk at any time)
module OTServer (Revision, merge, mergeAtRevision, applyAtRevision) where

-- Standard modules
--import Data.Text (Text)
import Control.Monad (foldM, liftM)
import Data.Functor ((<$>))

-- Supporting modules
-- https://github.com/timjb/haskell-operational-transformation
import Control.OperationalTransformation

type Revision = Integer

-- Compose an operation with a sequence of preceding operations
-- This function expects the list of preceding operations to be passed to it in reverse order  
merge :: OTOperation op => [op] -> op -> Either String [op]
merge ops op = liftM (:ops) (foldM transformFst op (reverse ops))
  where
    transformFst :: OTOperation op => op -> op -> Either String op 
    transformFst a b = fst <$> transform a b
  
-- Compose an operation with all preceding operations starting from the given revision
-- This function expects the list of preceding operations to be passed to it in reverse order
mergeAtRevision :: OTOperation op => [op] -> Revision -> op -> Either String [op]
mergeAtRevision ops opRev op = do
  concurrentOps <- if opRev > fromIntegral (length ops)
    then Left $ "unknown revision number " ++ show opRev 
    else Right $ take (length ops - fromInteger opRev) ops
  merge concurrentOps op

---- Apply a sequence of operations to a document starting at the given revision number  
applyAtRevision :: OTSystem doc op => [op] -> Revision -> doc -> Either String doc 
applyAtRevision ops docRev doc = do
  unappliedOps <- if docRev > fromIntegral (length ops)
    then Left $ "unknown revision number " ++ show docRev 
    else Right $ take (length ops - fromInteger docRev) ops
  foldM (flip apply) doc unappliedOps
