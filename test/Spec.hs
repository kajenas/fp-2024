{-# LANGUAGE ImportQualifiedPost #-}
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
  
  [ -- Test for empty input to parseQuery
    testCase "Empty input" $
      Lib2.parseQuery "" @?= Left "Invalid command. Must start with 'Remove', 'New order', 'Add pizza', or 'list'"

    -- Test for invalid command
  , testCase "Invalid command" $
      Lib2.parseQuery "Invalid command" @?= Left "Invalid command. Must start with 'Remove', 'New order', 'Add pizza', or 'list'"

    -- Test for a simple new order
  , testCase "Simple new order" $
    Lib2.parseQuery "New order\nJohn\nOrder\nPizza:\nmedium\nthin\npepperoni\n1\nDelivery\nCash\nConfirm" @?=
    Right (Lib2.NewOrder [("John", Lib2.SimpleOrder 
      (Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1)
      (Lib2.OrderDetails Lib2.Delivery Lib2.Cash))])
    
    -- Test for removing an order
  , testCase "Remove order" $
      let initialState = Lib2.State [("John", Lib2.SimpleOrder 
            (Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1)
            (Lib2.OrderDetails Lib2.Delivery Lib2.CreditCard))]
      in Lib2.stateTransition initialState 
           (Lib2.RemoveOrder "John" (Lib2.SimpleOrder 
             (Lib2.Pizza Lib2.Small Lib2.Thin [] 1)
             (Lib2.OrderDetails Lib2.Pickup Lib2.Cash)))
         @?= Right (Just "Order removed for John", Lib2.State [])
    
    -- Test for adding pizza to a non-existent order
  , testCase "Add pizza to non-existent order" $
      let emptyState = Lib2.emptyState
      in Lib2.stateTransition emptyState 
           (Lib2.AddPizzaToOrder "John" 
             (Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1))
         @?= Left "No existing order found for John"

    -- Test for listing orders for an existing person
  , testCase "List orders for existing person" $
    let state = Lib2.State [("John", Lib2.SimpleOrder 
          (Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1)
          (Lib2.OrderDetails Lib2.Delivery Lib2.Cash))]
    in Lib2.stateTransition state (Lib2.ListOrders "John") @?=
       Right (Just "Order for John:\n  Medium Thin crust pizza with Pepperoni, quantity: 1\n\n  Order type: Delivery\n  Payment method: Cash", state)
    
    -- Test for listing orders for a non-existent person
  , testCase "List orders for non-existent person" $
      let state = Lib2.State [("John", Lib2.SimpleOrder 
            (Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1)
            (Lib2.OrderDetails Lib2.Delivery Lib2.Cash))]
      in Lib2.stateTransition state (Lib2.ListOrders "Alice") @?=
         Right (Just "No orders found for Alice", state)

    -- Test for creating a simple bundle order
  , testCase "Simple bundle order" $
    Lib2.parseQuery "New order\nJohn\nBundle\nPizza:\nmedium\nthin\npepperoni\n1\nPizza:\nlarge\nstuffed\nbacon\n2\nDelivery\nCredit\nConfirm" @?=
    Right (Lib2.NewOrder [("John", Lib2.OrderBundle 
      [ Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1
      , Lib2.Pizza Lib2.Large Lib2.Stuffed [Lib2.Bacon] 2]
      [] 
      (Lib2.OrderDetails Lib2.Delivery Lib2.CreditCard))])


    -- Test for adding pizza to an existing bundle
  , testCase "Add pizza to existing bundle" $
      let initialState = Lib2.State [("John", Lib2.OrderBundle 
              [ Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1 ] 
              [] 
              (Lib2.OrderDetails Lib2.Delivery Lib2.Cash))]
      in Lib2.stateTransition initialState 
           (Lib2.AddPizzaToOrder "John" 
             (Lib2.Pizza Lib2.Large Lib2.Thick [Lib2.Bacon] 1))
         @?= Right (Just "Added pizza to order for John", 
                    Lib2.State [("John", Lib2.OrderBundle 
                      [ Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1
                      , Lib2.Pizza Lib2.Large Lib2.Thick [Lib2.Bacon] 1] 
                      [] 
                      (Lib2.OrderDetails Lib2.Delivery Lib2.Cash))])
  ]



-- Arbitrary instances for property testing
instance Arbitrary Lib2.Size where
    arbitrary = elements [Lib2.Small, Lib2.Medium, Lib2.Large]

instance Arbitrary Lib2.Crust where
    arbitrary = elements [Lib2.Thin, Lib2.Thick, Lib2.Stuffed]

instance Arbitrary Lib2.Topping where
    arbitrary = elements [Lib2.Pepperoni, Lib2.Mushrooms, Lib2.Onions, 
                        Lib2.Sausage, Lib2.Bacon, Lib2.ExtraCheese]

instance Arbitrary Lib2.OrderType where
    arbitrary = elements [Lib2.Delivery, Lib2.Pickup]

instance Arbitrary Lib2.PaymentMethod where
    arbitrary = elements [Lib2.CreditCard, Lib2.Cash, Lib2.MobilePayment]

instance Arbitrary Lib2.Pizza where
    arbitrary = Lib2.Pizza 
        <$> arbitrary 
        <*> arbitrary 
        <*> listOf1 arbitrary 
        <*> choose (1, 5)  -- Reasonable quantity range

instance Arbitrary Lib2.OrderDetails where
    arbitrary = Lib2.OrderDetails <$> arbitrary <*> arbitrary

instance Arbitrary Lib2.Order where
    arbitrary = oneof [
        Lib2.SimpleOrder <$> arbitrary <*> arbitrary,
        Lib2.OrderBundle 
            <$> listOf1 arbitrary           -- pizzas
            <*> listOf arbitrary            -- suborders
            <*> arbitrary                   -- details
        ]

instance Arbitrary Lib2.Query where
    arbitrary = oneof [
        Lib2.NewOrder <$> listOf1 ((,) 
            <$> elements ["John", "Alice", "Bob"] 
            <*> arbitrary),
        Lib2.RemoveOrder 
            <$> elements ["John", "Alice", "Bob"] 
            <*> arbitrary,
        Lib2.AddPizzaToOrder 
            <$> elements ["John", "Alice", "Bob"] 
            <*> arbitrary,
        Lib2.ListOrders 
            <$> elements ["John", "Alice", "Bob"]
        ]


propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ 
  -- Test that state transitions preserve orders when adding pizza
    QC.testProperty "stateTransition preserves existing orders when adding pizza" $
      \name pizza ->
        let initialState = Lib2.State [("John", Lib2.SimpleOrder 
              (Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1)
              (Lib2.OrderDetails Lib2.Delivery Lib2.Cash))]
            query = Lib2.AddPizzaToOrder name pizza
            result = Lib2.stateTransition initialState query
        in case result of
             Right (_, finalState) -> 
               case lookup "John" (Lib2.orders finalState) of
                 Just _ -> True  -- Original order still exists
                 Nothing -> False
             _ -> True
  ]