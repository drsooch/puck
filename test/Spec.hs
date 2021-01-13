import           Parsing
import           Test.HUnit

main :: IO ()
main = do
    _ <- runtestTT parsingTests
    return ()
