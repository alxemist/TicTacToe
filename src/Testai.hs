import Test.Hspec
import Data.Maybe
import Validuoti

main :: IO ()
main = hspec $ do
  describe "Validuoja" $ do
    it "Turetu grazinti True, nes x laimejo" $ do
      validate "d1:0d1:v1:x1:xi2e1:yi2ee1:1d1:v1:o1:xi0e1:yi1ee1:2d1:v1:x1:xi2e1:yi0ee1:3d1:v1:o1:xi0e1:yi2ee1:4d1:v1:x1:xi2e1:yi1eee" `shouldBe` True