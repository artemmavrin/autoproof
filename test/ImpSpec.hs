import Data.List (intercalate)
import Data.Prop.Internal.Utils (PrettyPrintable (pretty))
import Data.Prop.Proof.Implication (prove)
import Data.Prop.Proof.Types (debug)
import Data.Prop.Types (Formula (Var), (-->))
import Test.Hspec (describe, hspec, it, shouldBe)

-- Known provable sequents
provable :: [([Formula Char], Formula Char)]
provable =
  [ ([Var 'a'], Var 'a'),
    ([], Var 'a' --> Var 'a'),
    ([], Var 'a' --> Var 'b' --> Var 'a'),
    ([], (Var 'c' --> Var 'a') --> (Var 'c' --> Var 'a' --> Var 'b') --> Var 'c' --> Var 'b'),
    ([Var 'a', Var 'b'], Var 'a'),
    ([Var 'a' --> Var 'b', Var 'a'], Var 'b'),
    ([Var 'b' --> Var 'c' --> Var 'd', Var 'a' --> Var 'b', Var 'a'], Var 'c' --> Var 'd'),
    ([Var 'a' --> Var 'b', (Var 'a' --> Var 'b') --> Var 'c'], Var 'c'),
    ( [ (Var 'a' --> Var 'b') --> (Var 'c' --> Var 'd') --> Var 'e',
        Var 'e' --> Var 'b',
        Var 'a' --> Var 'b',
        Var 'c' --> Var 'd'
      ],
      Var 'b'
    ),
    ( [ Var 'c',
        Var 'c' --> Var 'a' --> Var 'b',
        Var 'b' --> Var 'd',
        Var 'b' --> Var 'c' --> Var 'b',
        Var 'c' --> (Var 'a' --> Var 'b') --> Var 'd'
      ],
      (Var 'a' --> Var 'b') --> Var 'd'
    )
  ]

-- Known unprovable sequents
unprovable :: [([Formula Char], Formula Char)]
unprovable =
  [ ([], Var 'a'),
    ([Var 'a'], Var 'b'),
    ([], Var 'a' --> Var 'b'),
    ([], ((Var 'a' --> Var 'b') --> Var 'a') --> Var 'a'),
    ([Var 'a' --> Var 'b', Var 'b' --> Var 'a'], Var 'a')
  ]

main :: IO ()
main = hspec $ do
  describe "prove" $ do
    mapM_
      ( \(c, a) -> do
          it ("provable: " ++ prettySequent c a) $ do
            debug <$> prove c a `shouldBe` Just (Right ())
      )
      provable

    mapM_
      ( \(c, a) -> do
          it ("unprovable: " ++ prettySequent c a) $ do
            prove c a `shouldBe` Nothing
      )
      unprovable

prettySequent :: PrettyPrintable a => [Formula a] -> Formula a -> String
prettySequent [] a = "|- " ++ pretty a
prettySequent c a = intercalate ", " (map pretty c) ++ " |- " ++ pretty a
