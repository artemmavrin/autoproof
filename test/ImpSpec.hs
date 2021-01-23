import Data.List (intercalate)
import Data.Prop (Formula (Var), debug, (-->))
import Data.Prop.Proof.Implication (proveImp)
import Data.Prop.Utils (PrettyPrintable (pretty))
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
    ),
    ( [ Var 'a',
        Var 'a' --> Var 'b',
        Var 'b' --> Var 'c',
        Var 'c' --> Var 'd'
      ],
      Var 'a' --> Var 'd'
    ),
    ( [ Var 'a',
        Var 'b',
        Var 'a' --> (Var 'b' --> Var 'c'),
        Var 'c' --> Var 'd'
      ],
      Var 'a' --> Var 'd'
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
  describe "proveImp" $ do
    mapM_
      ( \(c, a) -> do
          it ("provable: " ++ prettySequent c a) $ do
            debug <$> proveImp c a `shouldBe` Just (Right ())
      )
      provable

    mapM_
      ( \(c, a) -> do
          it ("unprovable: " ++ prettySequent c a) $ do
            proveImp c a `shouldBe` Nothing
      )
      unprovable

prettySequent :: PrettyPrintable a => [Formula a] -> Formula a -> String
prettySequent [] a = "|- " ++ pretty a
prettySequent c a = intercalate ", " (map pretty c) ++ " |- " ++ pretty a
