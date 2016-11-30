import Test.Hspec
import Data.Maybe
import Validuoti
import Pulti

main :: IO ()
main = hspec $ do
  describe "Validuoja" $ do
    it "Turetu grazinti True, nes x laimejo" $ do
      validate "d1:0d1:v1:x1:xi2e1:yi2ee1:1d1:v1:o1:xi0e1:yi1ee1:2d1:v1:x1:xi2e1:yi0ee1:3d1:v1:o1:xi0e1:yi2ee1:4d1:v1:x1:xi2e1:yi1eee" `shouldBe` True

    it "Turetu grazinti False, nes dar niekas nelaimejo ir zaidimas nepasibaige" $ do
      validate "d1:0d1:v1:x1:xi0e1:yi0ee1:1d1:v1:o1:xi2e1:yi1eee" `shouldBe` False

    it "Patikrina, ar padaro ejima i koordinate 1,1" $ do
      turn "d1:0d1:v1:x1:xi0e1:yi0ee1:1d1:v1:o1:xi2e1:yi1eee" `shouldBe` Just ("d1:v1:x1:xi1e1:yi1eee")

    it "Turetu grazinti True, nes x laimejo" $ do
      validate "d1:0d1:v1:x1:xi2e1:yi0ee1:1d1:v1:o1:xi0e1:yi0ee1:2d1:v1:x1:xi2e1:yi1ee1:3d1:v1:o1:xi0e1:yi1ee1:4d1:v1:x1:xi1e1:yi0ee1:5d1:v1:o1:xi1e1:yi1ee1:6d1:v1:x1:xi2e1:yi2eee" `shouldBe` True

    it "Turetu grazinti True, nes x laimejo ir zaidimas baigesi" $ do
      validate "d1:0d1:v1:x1:xi1e1:yi0ee1:1d1:v1:o1:xi0e1:yi0ee1:2d1:v1:x1:xi2e1:yi2ee1:3d1:v1:o1:xi0e1:yi2ee1:4d1:v1:x1:xi1e1:yi1ee1:5d1:v1:o1:xi2e1:yi0ee1:6d1:v1:x1:xi2e1:yi1ee1:7d1:v1:o1:xi1e1:yi2ee1:8d1:v1:x1:xi0e1:yi1eee" `shouldBe` True