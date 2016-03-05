module API where

import Data.Int (Int64)
import Servant

import Model

type API = "memos" :> Get '[JSON] [MemoInfo]
      :<|> "memos" :> ReqBody '[JSON] Memo :> Post '[JSON] Memo
      :<|> "memos" :> Capture "id" Int64 :> Get '[JSON] Memo
      :<|> "memos" :> Capture "id" Int64 :> ReqBody '[JSON] Memo :> Put '[JSON] Memo
      :<|> "memos" :> Capture "id" Int64 :> Delete '[JSON] ()

api :: Proxy API
api = Proxy
