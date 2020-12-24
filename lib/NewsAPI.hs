{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module NewsAPI
  ( runEverything
  ) where


import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           Servant.Types.SourceT   (foreach)

apiRoot :: String
apiRoot = "newsapi.org"

data NewsResult = NewsResult { status       :: String,
                               totalResults :: Int,
                               articles     :: [ArticleResult] } deriving (Show, Eq, Generic)

data ArticleResult = ArticleResult { source      :: Source,
                                     author      :: String,
                                     title       :: String,
                                     description :: String,
                                     url         :: String,
                                     urlToImage  :: String,
                                     publishedAt :: String,
                                     content     :: String } deriving (Show, Eq, Generic)

data Source = Source { id   :: String,
                       name :: String } deriving (Show, Eq, Generic)

instance FromJSON NewsResult
instance FromJSON ArticleResult
instance FromJSON Source

type FilterString = String
type APIKey       = String

type NewsAPI = "v2/everything"
  :> QueryParam "q" String
  :> QueryParam "apiKey" String
  :> Get '[JSON] NewsResult

newsAPI :: Proxy NewsAPI
newsAPI = Proxy

everything = client newsAPI

runEverything :: APIKey -> FilterString -> IO (Either ClientError NewsResult)
runEverything key term = do
   manager' <- newManager tlsManagerSettings
   runClientM (everything (Just term) (Just key)) (mkClientEnv manager' (BaseUrl Http apiRoot 80 ""))

