{-# LANGUAGE FlexibleContexts #-}
module Concurrent where


import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Database.Persist.Sqlite        ( SqlBackend )

import qualified Control.Concurrent            as C

import           Binah.Infrastructure

-- TODO: Figure out the types
forkTIO :: TaggedT TIO () -> TaggedT TIO C.ThreadId
forkTIO = TaggedT . TIO . C.forkIO . runTIO . unTag
