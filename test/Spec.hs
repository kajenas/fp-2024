{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
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
genOrderList :: Gen [(String, Lib2.Order)]
genOrderList = listOf1 ((,) <$> genName <*> genSimpleOrder)

-- Generate a random name
genName :: Gen String
genName = listOf1 $ elements ['a'..'z']

-- Generate only orders
genSimpleOrder :: Gen Lib2.Order
genSimpleOrder = Lib2.SimpleOrder <$> arbitrary <*> arbitrary


instance Arbitrary Lib2.Order where
 arbitrary = genSimpleOrder

-- Modify Pizza to have a limited number of toppings
instance Arbitrary Lib2.Pizza where
 arbitrary = Lib2.Pizza
    <$> arbitrary
    <*> arbitrary
    <*> listOf (elements [Lib2.Pepperoni, Lib2.Mushrooms]) -- Limit topping choices for simplicity
    <*> choose (1, 3) -- Limit quantity range

-- Keep other arbitrary instances mostly the same
instance Arbitrary Lib2.Size where
 arbitrary = elements [Lib2.Small, Lib2.Medium, Lib2.Large]

instance Arbitrary Lib2.Crust where
 arbitrary = elements [Lib2.Thin, Lib2.Thick]

instance Arbitrary Lib2.Topping where
 arbitrary = elements [Lib2.Pepperoni, Lib2.Mushrooms]

instance Arbitrary Lib2.OrderType where
 arbitrary = elements [Lib2.Delivery, Lib2.Pickup]

instance Arbitrary Lib2.PaymentMethod where
 arbitrary = elements [Lib2.CreditCard, Lib2.Cash]

instance Arbitrary Lib2.OrderDetails where
 arbitrary = Lib2.OrderDetails <$> arbitrary <*> arbitrary


instance Arbitrary Lib2.Query where
 arbitrary = oneof
  [ Lib2.NewOrder <$> genOrderList
  , Lib2.RemoveOrder <$> genName <*> genSimpleOrder
  , Lib2.AddPizzaToOrder <$> genName <*> arbitrary
  , Lib2.ListOrders <$> genName
  ]

-- Lib3 statements
instance Arbitrary Lib3.Statements where
 arbitrary = frequency
  [ (3, Lib3.Single <$> arbitrary) 
  , (1, Lib3.Batch <$> listOf1 (arbitrary @Lib2.Query))
  ]

-- Property test
propertyTests :: TestTree
propertyTests = testGroup "Property tests"
 [ QC.testProperty "parseStatements . renderStatements == Right query" $  -- Test for statement integrity
   \query -> 
     let 
       rendered = Lib3.renderStatements query 
       parsed = Lib3.parseStatements rendered 
     in 
       counterexample ("Original query: " ++ show query ++ 
                       "\nRendered: " ++ rendered ++ 
                       "\nParsed result: " ++ show parsed) $ 
         case parsed of 
           Right (parsedQuery, "") -> query == parsedQuery 
           _ -> False
      , QC.testProperty "Parsing invalid statements returns an error" $
   \invalidInput ->
     case Lib3.parseStatements invalidInput of
       Left _ -> True  -- Should return an error for invalid input
       _ -> False

 , QC.testProperty "Empty batch statements can be parsed" $
   Lib3.parseStatements (Lib3.renderStatements (Lib3.Batch [])) 
     === Right (Lib3.Batch [], "")

 , QC.testProperty "Large batch statements maintain integrity" $
   \largeBatch ->
     let 
       rendered = Lib3.renderStatements (Lib3.Batch largeBatch)
       parsed = Lib3.parseStatements rendered
     in
       case parsed of
         Right (Lib3.Batch parsedBatch, "") -> 
           length parsedBatch == length largeBatch
         _ -> False

  , QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
      ]
