{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}

module Schema where

import Data.Morpheus.Kind ( KIND, OBJECT)
import Data.Morpheus.Types ( GQLType(..), ResM, gqlResolver)
import Data.Text ( Text )
import GHC.Generics ( Generic )
import Lib

data Query = Query
  { deity :: DeityArgs -> ResM Deity
  } deriving (Generic)

data Deity = Deity
  { fullName :: Text         -- Non-Nullable Field
  , power    :: Maybe Text   -- Nullable Field
  } deriving (Generic, GQLType)

type instance KIND Deity = OBJECT

data DeityArgs = DeityArgs
  { name      :: Text        -- Required Argument
  , mythology :: Maybe Text  -- Optional Argument
  } deriving (Generic)

