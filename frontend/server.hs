#!/usr/bin/env stack
-- stack runghc --resolver lts-3.11

import Servant
import Network.Wai
import Network.Wai.Handler.Warp

type API = Raw

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = serveDirectory "./dist"

main :: IO ()
main = do
    putStrLn "Start at 8081..."
    run 8081 app
