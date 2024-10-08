{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lib1Tests, lib2Tests]

lib1Tests :: TestTree
lib1Tests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
    null Lib1.completions @?= False
  ]

lib2Tests :: TestTree
lib2Tests = testGroup "Lib2 tests"
  [ testCase "Empty input" $
    Lib2.parseQuery "" @?= Left "Invalid command. Must start with 'Remove', 'New order', or 'Add pizza'"
    
  , testCase "Invalid command" $
    Lib2.parseQuery "Invalid command" @?= Left "Invalid command. Must start with 'Remove', 'New order', or 'Add pizza'"
    
  , testCase "Simple new order" $
    Lib2.parseQuery "New order\nJohn\nPizza:\nmedium\nthin\npepperoni\n1\nDelivery\nCredit Card\nConfirm" @?=
    Right (Lib2.NewOrder [("John", Lib2.SimpleOrder 
      (Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1)
      (Lib2.OrderDetails Lib2.Delivery Lib2.CreditCard))])
    
  , testCase "Remove order" $
    let initialState = Lib2.State [("John", Lib2.SimpleOrder 
          (Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1)
          (Lib2.OrderDetails Lib2.Delivery Lib2.CreditCard))]
    in Lib2.stateTransition initialState 
         (Lib2.RemoveOrder "John" (Lib2.SimpleOrder 
           (Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1)
           (Lib2.OrderDetails Lib2.Delivery Lib2.CreditCard)))
       @?= Right (Just "Order removed for John", Lib2.State [])
    
  , testCase "Add pizza to non-existent order" $
    let emptyState = Lib2.emptyState
    in Lib2.stateTransition emptyState 
         (Lib2.AddPizzaToOrder "John" 
           (Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Pepperoni] 1))
       @?= Left "No existing order found for John"
  ]