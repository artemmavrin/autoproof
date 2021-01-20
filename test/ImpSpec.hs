import Proof.Implication
  ( debug,
    prove,
  )
import Data.Prop.Types (Formula (Var), (-->))
import Test.Hspec (describe, hspec, it, shouldBe)

-- Known provable sequents
provable :: [([Formula Int], Formula Int)]
provable =
  [ ([Var 0], Var 0),
    ([], Var 0 --> Var 0),
    ([], Var 0 --> Var 1 --> Var 0),
    ([], (Var 2 --> Var 0) --> (Var 2 --> Var 0 --> Var 1) --> Var 2 --> Var 1),
    ([Var 0, Var 1], Var 0),
    ([Var 0 --> Var 1, Var 0], Var 1),
    ([Var 1 --> Var 2 --> Var 3, Var 0 --> Var 1, Var 0], Var 2 --> Var 3),
    ([Var 0 --> Var 1, (Var 0 --> Var 1) --> Var 2], Var 2),
    ( [ (Var 0 --> Var 1) --> (Var 2 --> Var 3) --> Var 4,
        Var 4 --> Var 1,
        Var 0 --> Var 1,
        Var 2 --> Var 3
      ],
      Var 1
    ),
    ( [ Var 2,
        Var 2 --> Var 0 --> Var 1,
        Var 1 --> Var 3,
        Var 1 --> Var 2 --> Var 1,
        Var 2 --> (Var 0 --> Var 1) --> Var 3
      ],
      (Var 0 --> Var 1) --> Var 3
    )
  ]

-- Known unprovable sequents
unprovable :: [([Formula Int], Formula Int)]
unprovable =
  [ ([], Var 0),
    ([Var 0], Var 1),
    ([], Var 0 --> Var 1),
    ([], ((Var 0 --> Var 1) --> Var 0) --> Var 0),
    ([Var 0 --> Var 1, Var 1 --> Var 0], Var 0)
  ]

main :: IO ()
main = hspec $ do
  describe "prove" $ do
    it "checking known provable sequents" $ do
      mapM_ (\(c, a) -> debug <$> prove c a `shouldBe` Just Nothing) provable

    it "checking known unprovable sequents" $ do
      mapM_ (\(c, a) -> prove c a `shouldBe` Nothing) unprovable
