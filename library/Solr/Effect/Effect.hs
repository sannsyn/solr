module Solr.Effect.Effect
where

import Solr.Prelude
import qualified Network.HTTP.Client
import qualified Data.ByteString.Lazy
import qualified Data.ByteString
import qualified Solr.Request
import qualified Solr.HTTPRequestEncoder
import qualified HTTPResponseDecoder


newtype Effect a =
  Effect (ReaderT Resource (ExceptT Error IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

data Resource =
  Resource ByteString Network.HTTP.Client.Manager

data Error =
  Error_Decoding !Text |
  Error_Transport !Network.HTTP.Client.HttpException
  deriving (Show)

run :: Effect a -> Resource -> IO (Either Error a)
run (Effect impl) resource =
  runExceptT (runReaderT impl resource)

request :: Solr.Request.Request a b -> a -> Effect b
request (Solr.Request.Request encoderProducer decoder) input =
  Effect $
  ReaderT $
  \(Resource baseURL httpManager) ->
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
