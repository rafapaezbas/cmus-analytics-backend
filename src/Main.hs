{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.Trans(liftIO,lift,MonadIO)
import System.IO
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (pack,unpack)
import Data.Maybe

import Database.MongoDB( PortID( PortNumber ) )
import Database.MongoDB    (Action, Document, Document, Value, access,
                            allCollections, close, connect, delete, exclude, find,
                            host,findOne, insertMany, master, project, rest,
                            select, sort, Val, at, (=:))

main :: IO ()
main = scotty 3000 $ do

    get "/logs/:id" $ do
	id <- param "id"
	log <- liftIO $ selectById $ unpack id
        --html $ mconcat ["<h1>", pack  $ look "content" $ fromJust log , "</h1>"]
        --html $ mconcat ["<h1>", pack $ at "content" $ fromJust log , "</h1>"]
	text $ pack $ at "content" $ fromJust log

    post "/readbody" $ do
        b <- body
	let decodedBody = unpack(decodeUtf8 b)
        liftIO(putStr(decodedBody))
        liftIO(hFlush stdout)
        text $ decodeUtf8 b



run::MonadIO m => Action m a -> m a 
run action = do
        pipe <- liftIO(connect $ host "127.0.0.1")
	access pipe master "cmus-analytics" action

selectById :: (MonadIO m, Val v) => v -> m (Maybe Document)
selectById id = run $ findOne(select ["id" =: id] "logs")
