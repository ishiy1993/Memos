module Client (memosCLI) where

import Control.Monad.Trans.Either
import Data.Int (Int64)
import Data.Text (pack, unpack)
import Data.Time
import Servant
import Servant.Client

import API
import Model

memosCLI :: [String] -> IO ()
memosCLI ["list"] = do
    res <- runEitherT $ getMemos
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right mis -> mapM_ (putStrLn . mis2str) mis 
          where
              mis2str :: MemoInfo -> String
              mis2str (MemoInfo id cn) = show id ++ " " ++ unpack cn
memosCLI ["post", cn, tg] = do
    dt <- getDate
    let m = Memo (pack cn) (pack tg) (pack dt)
    res <- runEitherT $ postMemo m
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right (Memo cn' tg' dt') -> putStr . unlines $
          [ "==Post=="
          , unpack tg' ++ " " ++ unpack dt'
          , "Content: " ++ unpack cn'
          ]
memosCLI ["get", id] = do
    res <- runEitherT $ getDetail (read id)
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right (Memo cn tg dt) -> putStr . unlines $
          [ "Id: " ++ id
          , unpack tg ++ " " ++ unpack dt
          , "Content: " ++ unpack cn
          ]
memosCLI ["update", id, cn, tg] = do
    dt <- getDate
    let memo = Memo (pack cn) (pack tg) (pack dt)
    res <- runEitherT $ putMemo (read id) memo
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right (Memo cn' tg' dt') -> putStr . unlines $
          [ "==Update=="
          , unpack tg' ++ " " ++ unpack dt'
          , "Content: " ++ unpack cn'
          ]
memosCLI ["delete", id] = do
    res <- runEitherT $ deleteMemo (read id)
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right _ -> putStrLn "Success"
memosCLI _ = putStr . unlines $
    [ "Usage:"
    , ""
    , "$ memo-cli list"
    , "$ memo-cli post content tag"
    , "$ memo-cli get id"
    , "$ memo-cli update id content tag"
    , "$ memo-cli delete id"
    ]

getDate :: IO String
getDate = formatTime defaultTimeLocale "%Y/%-m/%-d %T" <$> getZonedTime

getMemos :: EitherT ServantError IO [MemoInfo]
postMemo :: Memo -> EitherT ServantError IO Memo
getDetail :: Int64 -> EitherT ServantError IO Memo
putMemo :: Int64 -> Memo -> EitherT ServantError IO Memo
deleteMemo :: Int64 -> EitherT ServantError IO ()

memos = client api url
    where
        url = BaseUrl Http "localhost" 8080

getMemos :<|> postMemo :<|> getDetail :<|> putMemo :<|> deleteMemo = memos
