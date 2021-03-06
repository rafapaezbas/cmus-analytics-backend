{-# LANGUAGE OverloadedStrings #-}
import Data.Digest.Pure.MD5 (md5)
import Web.Scotty
import Network.Wai.Middleware.Cors
import Control.Monad.Trans(liftIO,MonadIO)
import Control.Monad.Fail
import System.IO
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.ByteString.Lazy.Char8 as LB
import Data.Text.Lazy as LT (pack,unpack)
import Data.Maybe
import Data.Time.Clock.POSIX
import Web.Scotty.TLS
import Database.MongoDB    (Action, Document, Value, access,
                            allCollections, upsert, connect,
                            host,findOne, insertMany, master, 
                            select,aggregate, Val, at, (=:))

main :: IO ()
main = scottyTLS 443 "/home/ubuntu/ssl-certs/cert.key" "/home/ubuntu/ssl-certs/cert.crt" $ do

    middleware simpleCors

    get "/logs/random" $ do
	logs <- liftIO $ selectRandom 1
	text $ LT.pack $ getContent $ Just $ Prelude.head logs

    get "/logs/:id" $ do
	id <- param "id"
	log <- liftIO $ selectById $ LT.unpack id
	text $ LT.pack $ getContent log

    post "/logs" $ do
        b <- body
	let decodedBody = LT.unpack(decodeUtf8 b)
        let id = getMD5 decodedBody
	i <- liftIO $ upsertLog id decodedBody
        text $ LT.pack $ show id

--setup database connection
run::MonadIO m => Action m a -> m a 
run action = do
        pipe <- liftIO(connect $ host "127.0.0.1")
	access pipe master "cmus-analytics" action

--Select document by id field, 
selectById :: (MonadIO m, Val v) => v -> m (Maybe Document)
selectById id = run $ findOne(select ["id" =: id] "logs")

--Select random document
selectRandom :: (MonadIO m,MonadFail m) => Integer -> m [Document]
selectRandom size = run $ aggregate "logs" [["$sample" =: [ "size" =: size ]]]

--get field content from a document (if document does not exist, return emtpy string)
getContent::Maybe Document -> String
getContent document = case document of
	Just x ->  at "content" x
	Nothing -> ""

-- Upsert (inserts only if not existing) log with id and content
upsertLog::MonadIO m => String -> String -> m ()
upsertLog id body = run $ upsert (select ["id" =: id] "logs") ["id" =: id, "content" =: body]

--get MD5 of a string representation
getMD5::String -> String
getMD5 a = Prelude.map removeQuotes $ show $ md5 $ LB.pack a

--Change double quotes by blank char
removeQuotes:: Char -> Char
removeQuotes '"' = ' '
removeQuotes c = c
