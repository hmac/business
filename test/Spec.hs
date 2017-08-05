import Test.Hspec
import Data.Dates
import Business

main :: IO ()
main = hspec $ do
  describe "datesBetween" $ do
    it "returns the dates between the two given dates" $ do
      datesBetween  date1 date2 `shouldBe` [date1, DateTime 2017 1 2 0 0 0]
        where date1 = DateTime 2017 1 1 0 0 0
              date2 = DateTime 2017 1 3 0 0 0
