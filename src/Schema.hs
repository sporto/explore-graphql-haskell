{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}

module Schema where

import Data.Morpheus.Kind (OBJECT)
import Data.Morpheus.Types ( GQLType(..) )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import Lib
import           Data.Aeson                     ( FromJSON )

-- data Query = Query
--   { deity :: DeityArgs -> ResM Deity
--   } deriving (Generic)

data Deity = Deity
  { fullName :: Text         -- Non-Nullable Field
  , power    :: Maybe Text   -- Nullable Field
  } deriving (Generic, FromJSON)

instance GQLType Deity where
	type KIND Deity = OBJECT

data DeityArgs = DeityArgs
  { name      :: Text        -- Required Argument
  , mythology :: Maybe Text  -- Optional Argument
  } deriving (Generic)

