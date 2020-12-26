
import           Control.Monad
import           Data.Either
import           NewsAPI
import           System.Environment
import           Test.HUnit

main :: IO ()
main = do
  res <- runTestTT tests
  print res

tests = TestList [queryWithoutAPIKeyShouldFail, queryWithAPIKeyShouldSucceed]

queryWithoutAPIKeyShouldFail :: Test
queryWithoutAPIKeyShouldFail = TestCase (do
  queryRes    <- isLeft <$> runEverything defaultEverythingParams
  assertBool "Produced a Right result, despite no APIKey" queryRes)


queryWithAPIKeyShouldSucceed :: Test
queryWithAPIKeyShouldSucceed = TestCase(do
  envApiKey <- getEnv "NEWS_API_KEY"
  let mAPIKey = Just envApiKey
  queryRes <- isRight <$> runEverything defaultEverythingParams { apiKey = mAPIKey }
  assertBool "Produced a Left result, despite having a valid APIKey" queryRes)
