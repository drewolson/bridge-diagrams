module Bridge.ParseAndFormatSpec
  ( spec,
  )
where

import Bridge.Text.Formatter qualified as Formatter
import Bridge.Text.Parser qualified as Parser
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec (Expectation, Spec, describe, it, parallel, shouldBe)
import Text.RawString.QQ (r)

trimLines :: Text -> Text
trimLines = Text.unlines . fmap Text.stripEnd . Text.lines

isDiagram :: Either Text Text -> Text -> Expectation
isDiagram actual expected = trimLines <$> actual `shouldBe` Right (Text.drop 1 expected)

spec :: Spec
spec = parallel do
  describe "single handle" do
    it "handles a single hand" do
      let expected =
            [r|
♠AKxxx
♥KQx
♦Txx
♣xx
|]
      let result = Formatter.format <$> Parser.parse "akxxx kqx txx xx"
      result `isDiagram` expected

    it "handles a single hand without all cards" do
      let expected =
            [r|
♠AKx
♥KQx
♦
♣
|]
      let result = Formatter.format <$> Parser.parse "akx kqx - -"
      result `isDiagram` expected

  describe "double dummy" do
    it "doesn't need all cards" do
      let expected =
            [r|
           ♠AKx
           ♥
           ♦
           ♣
♠Tx         -----     ♠Qx
♥          |  N  |    ♥
♦          |W   E|    ♦
♣          |  S  |    ♣
            -----
           ♠Jx
           ♥
           ♦
           ♣
|]

      let result = Formatter.format <$> Parser.parse "akx - - -; qx - - -; jx - - -; tx - - -"
      result `isDiagram` expected
