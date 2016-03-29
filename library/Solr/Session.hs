module Solr.Session
(
  Session,
  request,
  -- * Execution
  Error(..),
  run,
)
where

import Solr.Prelude
import qualified Network.HTTP.Client
import qualified Data.ByteString.Lazy
import qualified Data.ByteString
import qualified Solr.Request
import qualified Solr.HTTPRequestEncoder
import qualified HTTPResponseDecoder


newtype Session a =
  Session (ReaderT Env (ExceptT Error IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

type Env =
  (ByteString, Network.HTTP.Client.Manager)

data Error =
  Error_Decoding !Text |
  Error_Transport !Network.HTTP.Client.HttpException

run :: Session a -> ByteString -> Network.HTTP.Client.Manager -> IO (Either Error a)
run (Session impl) baseURL httpManager =
  runExceptT (runReaderT impl (baseURL, httpManager))

request :: Solr.Request.Request a b -> a -> Session b
request (Solr.Request.Request encoderProducer decoder) input =
  Session $
  ReaderT $
  \(baseURL, httpManager) ->
    ExceptT $
    fmap (either (Left . Error_Transport) (either (Left . Error_Decoding) Right)) $
    try $
    Network.HTTP.Client.withResponse (request baseURL) httpManager $
    responseDecoder
  where
    request baseURL =
      Solr.HTTPRequestEncoder.encoder_request (encoderProducer baseURL) input
    responseDecoder =
      HTTPResponseDecoder.run decoder
