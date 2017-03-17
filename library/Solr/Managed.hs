module Solr.Managed
(
  resource,
)
where

import Solr.Prelude
import Control.Monad.Managed.Safe
import qualified Network.HTTP.Client as A
import qualified Network.HTTP.Client.TLS as B
import qualified Solr.Effect.Effect as A


tlsManager :: Managed A.Manager
tlsManager =
  managed (bracketOnError acquire release)
  where
    acquire =
      A.newManager B.tlsManagerSettings
    release _ =
      return ()

resource :: ByteString {-^ Base URL-} -> Managed A.Resource
resource baseURL =
  A.Resource <$> pure baseURL <*> tlsManager
