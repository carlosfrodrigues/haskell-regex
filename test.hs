import Regex
import Test.Hspec

main :: IO ()
main = hspec $ do
describe "generateRegexReq" $ do
    it "generates the Regex Requirements" $
        generateRegexReq "a*c*" `shouldBe` 
        [RegexReq {characters = Value "a", minValue = Finite 0, maxValue = Infinite},RegexReq {characters = Value "c", minValue = Finite 0, maxValue = Infinite}]

{-
input:
let reg = generateRegexReq "a*c*"
executeMultipleRegex reg "acaacc"

output:
Just ["ac","acc","aac","aacc"]
-}