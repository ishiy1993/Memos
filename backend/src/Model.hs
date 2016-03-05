module Model where

import Control.Monad.Reader
import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Servant

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Memo
    content Text
    tag Text
    date Text
    deriving Generic
|]

instance ToJSON Memo
instance FromJSON Memo

data MemoInfo = MemoInfo { id :: Int64
                         , cn :: Text
                         } deriving Generic

instance ToJSON MemoInfo
instance FromJSON MemoInfo
