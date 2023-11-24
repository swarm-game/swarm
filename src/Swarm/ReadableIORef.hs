-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Ensures that access to an 'IORef' is read-only
-- by hiding behind a newtype.
module Swarm.ReadableIORef (mkReadonly, ReadableIORef, readIORef) where

import Data.IORef (IORef)
import Data.IORef qualified as R (readIORef)

newtype ReadableIORef a = ReadableIORef (IORef a)

mkReadonly :: IORef a -> ReadableIORef a
mkReadonly = ReadableIORef

readIORef :: ReadableIORef a -> IO a
readIORef (ReadableIORef ref) = R.readIORef ref
