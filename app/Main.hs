{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as B

import Data.Morpheus (interpreter)
import Data.Morpheus.Types (GQLRootResolver (..), GQLType, IORes, Undefined(..), liftEither  )
import GHC.Generics ( Generic )
import Files.Files ( allDBEntry, lookupDBEntry)
import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Flow

import Lib
import Schema (Deity, DeityArgs, name)

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    deities :: ()     -> m [Deity]
  } deriving (Generic, GQLType)


resolveDeity :: DeityArgs -> IORes e Deity
resolveDeity args =
	name args
		|> lookupDBEntry
		|> liftEither


resolveDeities :: () -> IORes e [Deity]
resolveDeities _ =
	liftEither allDBEntry

resolveQuery :: Query m
resolveQuery =
	Query { deity = resolveDeity, deities = resolveDeities }

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver
    { queryResolver        = resolveQuery
    , mutationResolver     = Undefined
    , subscriptionResolver = Undefined
    }

gqlApi :: B.ByteString -> IO B.ByteString
gqlApi =
	interpreter rootResolver

main :: IO ()
main =
	scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)
