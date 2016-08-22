-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

module DevelMain
    ( -- * Test module
      update
    )
    where

import Control.Concurrent.STM
import Control.Monad
import Rapid


update :: IO ()
update =
    rapid 0 $ \r -> do
        mv1 <- createRef r "var1" newEmptyTMVarIO
        mv2 <- createRef r "var2" newEmptyTMVarIO

        start r "producer" $
            mapM_ (atomically . putTMVar mv1) [0 :: Integer ..]

        restart r "consumer" $
            forever . atomically $ do
                x <- takeTMVar mv1
                putTMVar mv2 (x, "blubb")

        -- For debugging the update action:
        replicateM_ 3 $
            atomically (takeTMVar mv2) >>= print
