{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Types        (GQLRootResolver (..), ResM, gqlResolver)
import GHC.Generics ( Generic )
import Files.Files                    ( allDBEntry
                                                , lookupDBEntry
                                                )
import Web.Scotty
import Control.Monad.IO.Class (liftIO)

import Lib
import Schema (Deity, DeityArgs, name)

data Query = Query
  { deity :: DeityArgs -> ResM Deity,
    deities :: ()     -> ResM [Deity]
  } deriving (Generic)


resolveDeity :: DeityArgs -> ResM Deity
resolveDeity args =
	gqlResolver $ lookupDBEntry (name args)


resolveDeities :: () -> ResM [Deity]
resolveDeities _ = gqlResolver $ allDBEntry

resolveQuery :: Query
resolveQuery = Query { deity = resolveDeity, deities = resolveDeities }

rootResolver :: GQLRootResolver IO Query () ()
rootResolver = GQLRootResolver
    { queryResolver        = return resolveQuery
    , mutationResolver     = return ()
    , subscriptionResolver = return ()
    }

gqlApi :: B.ByteString -> IO B.ByteString
gqlApi =
	interpreter rootResolver

main :: IO ()
main = 
	scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)
