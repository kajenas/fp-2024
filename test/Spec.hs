{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

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

propertyTests :: TestTree
propertyTests = testGroup "some meaningful name"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]