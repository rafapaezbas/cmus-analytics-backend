{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.Trans(liftIO,lift,MonadIO)
import System.IO
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (pack,unpack)
import Data.Maybe
import Data.Time.Clock.POSIX
import Database.MongoDB    (Action, Document, Document, Value, access,
                            allCollections, insert, close, connect, delete, exclude, find,
                            host,findOne, insertMany, master, project, rest,
                            select, liftDB, sort, Val, at, (=:))

main :: IO ()
main = scotty 3000 $ do

    get "/logs/:id" $ do
	id <- param "id"
	log <- liftIO $ selectById $ unpack id
	text $ pack $ getContent log

    post "/logs" $ do
        id <- liftIO $ getTimeInMillis
        b <- body
	let decodedBody = unpack(decodeUtf8 b)
	i <- liftIO $ insertLog (show id) decodedBody
        text $ pack $ show id

--setup database connection
run::MonadIO m => Action m a -> m a 
run action = do
        pipe <- liftIO(connect $ host "127.0.0.1")
	access pipe master "cmus-analytics" action

--Select document by id field, 
selectById :: (MonadIO m, Val v) => v -> m (Maybe Document)
selectById id = run $ findOne(select ["id" =: id] "logs")

--get field content from a document (if document does not exist, return emtpy string)
getContent::Maybe Document -> String
getContent document = case document of
	Just x ->  at "content" x
	Nothing -> ""

--getTimeInMillis
getTimeInMillis ::Integral b => IO b
getTimeInMillis = round `fmap` getPOSIXTime

-- Insert log with id and content
insertLog::MonadIO m => String -> String -> m Value
insertLog id body = run $ insert "logs" ["id" =: id, "content" =: body]
