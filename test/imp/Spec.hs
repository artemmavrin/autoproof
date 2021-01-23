import Data.List (intercalate)
import Data.Prop (Context, Formula, Sequent, debug)
import Data.Prop.Parser (unsafeParseSequent)
import Data.Prop.Proof.Implication (proveImp)
import Data.Prop.Utils (PrettyPrintable (pretty))
import qualified Data.Set as Set (toList)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

main :: IO ()
main = do
  -- Load known provable and unprovable sequents
  provable <- loadSequents "test/imp/examples/provable.sequents"
  unprovable <- loadSequents "test/imp/examples/unprovable.sequents"

  -- Run tests
  hspec $ do
    describe "proveImp" $ do
      mapM_ assertProvable provable
      mapM_ assertUnprovable unprovable

loadSequents :: String -> IO [Sequent String]
loadSequents filename = do
  contents <- readFile filename
  return (unsafeParseSequent <$> lines contents)

prettySequent :: PrettyPrintable a => Context a -> Formula a -> String
prettySequent c a = case Set.toList c of
  [] -> "|- " ++ pretty a
  c' -> intercalate ", " (map pretty c') ++ " |- " ++ pretty a

assertProvable :: (Ord a, PrettyPrintable a, Show a) => Sequent a -> SpecWith ()
assertProvable (c, a) = it ("provable: " ++ prettySequent c a) $ do
  debug <$> proveImp c a `shouldBe` Just (Right ())

assertUnprovable :: (Ord a, PrettyPrintable a, Show a) => Sequent a -> SpecWith ()
assertUnprovable (c, a) = it ("unprovable: " ++ prettySequent c a) $ do
  proveImp c a `shouldBe` Nothing
