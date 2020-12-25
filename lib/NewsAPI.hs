{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module NewsAPI
  ( runEverything
  , EverythingQueryParameters (..)
  , defaultEverythingParams 
  ) where


import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.List               (intercalate)
import           Data.Maybe
import           Data.Proxy
import           Data.Time
import           Data.Time.Clock
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

data EverythingQueryParameters = EverythingQueryParameters
  { apiKey         :: Maybe String,
    q              :: Maybe String,
    qInTitle       :: Maybe String,
    sources        :: Maybe [String],
    domains        :: Maybe [String],
    excludeDomains :: Maybe [String],
    from           :: Maybe Day,
    to             :: Maybe Day,
    language       :: Maybe String,
    sortBy         :: Maybe SortType,
    pageSize       :: Maybe Int,
    page           :: Maybe Int } deriving (Show, Eq, Generic)

data SortType = Relevancy
              | Popularity
              | PublishedAt deriving (Show, Eq, Generic)

instance FromJSON NewsResult
instance FromJSON ArticleResult
instance FromJSON Source
instance FromJSON SortType


defaultEverythingParams :: EverythingQueryParameters
defaultEverythingParams = EverythingQueryParameters 
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

type FilterString = String
type APIKey       = String
type CSVString    = String

type NewsAPI = "v2/everything"
  :> QueryParam "apiKey" String
  :> QueryParam "q" String
  :> QueryParam "qInTitle" String
  :> QueryParam "sources" CSVString
  :> QueryParam "domains" CSVString
  :> QueryParam "excludeDomains" CSVString
  :> QueryParam "from" Day
  :> QueryParam "to" Day
  :> QueryParam "language" String
  :> QueryParam "sortBy" String
  :> QueryParam "pageSize" Int
  :> QueryParam "page" Int
  :> Get '[JSON] NewsResult


newsAPI :: Proxy NewsAPI
newsAPI = Proxy

everything = client newsAPI

runEverything :: EverythingQueryParameters -> IO (Either ClientError NewsResult)
runEverything (EverythingQueryParameters apiKey q qInTitle sources domains excludeDomains from to language sortBy pageSize page) = do
  manager'     <- newManager tlsManagerSettings
  producedFrom <- produceDate from
  producedTo   <- produceDate to
  runClientM (everything apiKey q qInTitle sourceCSV domainCSV excludeCSV (Just producedFrom) (Just producedTo) language sortType pageSize page) (mkClientEnv manager' (BaseUrl Http apiRoot 80 ""))
    where
      sourceCSV = sources >>= toCSVString
      domainCSV = domains >>= toCSVString
      excludeCSV = excludeDomains >>= toCSVString
      sortType = sortBy >>= (Just . show)

produceDate :: Maybe Day -> IO Day
produceDate Nothing  = utctDay <$> getCurrentTime
produceDate (Just d) = pure d

toCSVString :: [String] -> Maybe CSVString
toCSVString [] = Nothing
toCSVString xs = Just (intercalate "," xs)

