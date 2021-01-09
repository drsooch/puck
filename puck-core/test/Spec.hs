import           Parsing
import           Test.HUnit

main :: IO ()
main = do
    _ <- runTestTT parsingTests
    return ()
