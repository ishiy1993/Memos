module Server (memosServer) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either
import Data.Int (Int64)
import Database.Persist
import Database.Persist.Sqlite
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import Servant

import API
import Model

memosServer :: IO ()
memosServer = do
    runDb $ runMigration migrateAll
    startApp

runDb = runSqlite "memos.db"

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = memosCors $ logStdoutDev $ serve api server

memosCors :: Middleware
memosCors = cors $ const $ Just memoResourcePolicy

memoResourcePolicy :: CorsResourcePolicy
memoResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
        , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

server :: Server API
server = getMemos
    :<|> postMemo
    :<|> getDetail
    :<|> putMemo
    :<|> deleteMemo
        where
            getMemos :: EitherT ServantErr IO [MemoInfo]
            getMemos = do
                memos <- lift $ runDb $ selectList [] []
                let res = map entity2MemoInfo memos
                return res
                    where
                        entity2MemoInfo :: Entity Memo -> MemoInfo
                        entity2MemoInfo (Entity memoId memo) = MemoInfo (fromSqlKey memoId) (memoContent memo)
            postMemo :: Memo -> EitherT ServantErr IO Memo
            postMemo memo = do
                key <- lift $ runDb $ insert memo
                res <- lift $ runDb $ get key
                case res of
                  Nothing -> left err404
                  Just m -> return m
            getDetail :: Int64 -> EitherT ServantErr IO Memo
            getDetail id = do
                let key = toSqlKey id
                res <- lift $ runDb $ get key
                case res of
                  Nothing -> left err404
                  Just m -> return m
            putMemo :: Int64 -> Memo -> EitherT ServantErr IO Memo
            putMemo id memo = do
                let key = toSqlKey id
                lift $ runDb $ repsert key memo
                res <- lift $ runDb $ get key
                case res of
                  Nothing -> left err404
                  Just m -> return m
            deleteMemo :: Int64 -> EitherT ServantErr IO ()
            deleteMemo id = do
                let key = toSqlKey id :: Key Memo
                lift $ runDb $ delete key
