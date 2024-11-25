{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lib1Tests, lib2Tests, propertyTests]

-- Lib1 Tests
lib1Tests :: TestTree
lib1Tests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
    null Lib1.completions @?= False
  ]

-- Lib2 Tests
lib2Tests :: TestTree
lib2Tests = testGroup "Lib2 tests"
  
  [ 
  
  ]

-- Arbitrary for Lib2.Query
instance Arbitrary Lib2.Query where
  arbitrary = oneof
    [ Lib2.NewOrder <$> genOrderList
    , Lib2.RemoveOrder <$> genName <*> arbitrary
    , Lib2.AddPizzaToOrder <$> genName <*> arbitrary
    , Lib2.ListOrders <$> genName
    ]

-- Helpers for generating components
genName :: Gen String
genName = elements ["John", "Alice", "Bob"]

genOrderList :: Gen [(String, Lib2.Order)]
genOrderList = listOf1 ((,) <$> genName <*> arbitrary)

-- Arbitrary for Lib2.Order
instance Arbitrary Lib2.Order where
  arbitrary = oneof
    [ Lib2.SimpleOrder <$> arbitrary <*> arbitrary
    , Lib2.OrderBundle <$> listOf1 arbitrary <*> listOf arbitrary <*> arbitrary
    ]

-- Arbitrary for Lib2.Pizza
instance Arbitrary Lib2.Pizza where
  arbitrary = Lib2.Pizza
    <$> arbitrary
    <*> arbitrary
    <*> listOf1 arbitrary
    <*> choose (1, 5)  -- Reasonable quantity range

-- Arbitrary for Lib2.Size, Lib2.Crust, Lib2.Topping, etc.
instance Arbitrary Lib2.Size where
  arbitrary = elements [Lib2.Small, Lib2.Medium, Lib2.Large]

instance Arbitrary Lib2.Crust where
  arbitrary = elements [Lib2.Thin, Lib2.Thick, Lib2.Stuffed]

instance Arbitrary Lib2.Topping where
  arbitrary = elements [Lib2.Pepperoni, Lib2.Mushrooms, Lib2.Onions, Lib2.Sausage, Lib2.Bacon, Lib2.ExtraCheese]

instance Arbitrary Lib2.OrderType where
  arbitrary = elements [Lib2.Delivery, Lib2.Pickup]

instance Arbitrary Lib2.PaymentMethod where
  arbitrary = elements [Lib2.CreditCard, Lib2.Cash, Lib2.MobilePayment]

instance Arbitrary Lib2.OrderDetails where
  arbitrary = Lib2.OrderDetails <$> arbitrary <*> arbitrary


instance Arbitrary Lib3.Statements where
  arbitrary = frequency
    [ (3, Lib3.Single <$> arbitrary)  -- Single query
    , (1, Lib3.Batch <$> listOf1 arbitrary)  -- Batch of queries
    ]

propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ testProperty "parseStatements . renderStatements == Right (s, \"\")" $
      \s -> let rendered = Lib3.renderStatements s
                parsed = Lib3.parseStatements rendered
            in counterexample
                 ("Rendered: " ++ rendered ++ "\nParsed: " ++ show parsed)
                 (parsed == Right (s, ""))
  ]


