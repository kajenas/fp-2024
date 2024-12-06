{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main (main) where

import Control.Concurrent (Chan, forkIO, killThread, newChan)
import Control.Lens
import Control.Monad.Free (Free (..), liftF)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.String.Conversions
import GHC.Conc (TVar, newTVar, newTVarIO, readTVar, readTVarIO)
import qualified Lib2
import qualified Lib3

import Network.Wreq


data PizzaOrderAlgebra a
  = NewOrder [(String, Lib2.Order)] a
  | RemoveOrder String Lib2.Order a
  | AddPizzaToOrder String Lib2.Pizza a
  | ListOrders String (String -> a)
  | Save a
  | Load a
  deriving (Functor)

type PizzaOrderProgram = Free PizzaOrderAlgebra

-- Convenience Functions
newOrderToStore :: [(String, Lib2.Order)] -> PizzaOrderProgram ()
newOrderToStore orders = liftF $ NewOrder orders ()

removeOrderFromStore :: String -> Lib2.Order -> PizzaOrderProgram ()
removeOrderFromStore person order = liftF $ RemoveOrder person order ()

addPizzaToExistingOrder :: String -> Lib2.Pizza -> PizzaOrderProgram ()
addPizzaToExistingOrder person pizza = liftF $ AddPizzaToOrder person pizza ()

listCustomerOrders :: String -> PizzaOrderProgram String
listCustomerOrders person = liftF $ ListOrders person id

save :: PizzaOrderProgram ()
save = liftF $ Save ()

load :: PizzaOrderProgram ()
load = liftF $ Load ()

-- HTTP Interpreter
interpretorSingleRequest :: PizzaOrderProgram a -> IO a
interpretorSingleRequest (Pure a) = return a
interpretorSingleRequest (Free step) = do
  next <- runStep step
  interpretorSingleRequest next
  where
    runStep :: PizzaOrderAlgebra a -> IO a
    runStep (NewOrder orders next) = sendSingleStatement (Lib2.NewOrder orders) >> return next
    runStep (RemoveOrder person order next) = sendSingleStatement (Lib2.RemoveOrder person order) >> return next
    runStep (AddPizzaToOrder person pizza next) = sendSingleStatement (Lib2.AddPizzaToOrder person pizza) >> return next
    runStep (ListOrders person next) = do
      str <- sendSingleStatement (Lib2.ListOrders person)
      return $ next str
    runStep (Save next) = postAsString "save" >> return next
    runStep (Load next) = postAsString "load" >> return next

interpretWithBatching' :: PizzaOrderProgram a -> [Lib2.Query] -> IO a
interpretWithBatching' (Pure a) batch = dumpBatch batch >> return a
interpretWithBatching' (Free step) batch = do
  case step of
    NewOrder orders next -> interpretWithBatching' next $ batch ++ [Lib2.NewOrder orders]
    RemoveOrder person order next -> interpretWithBatching' next $ batch ++ [Lib2.RemoveOrder person order]
    AddPizzaToOrder person pizza next -> interpretWithBatching' next $ batch ++ [Lib2.AddPizzaToOrder person pizza]
    ListOrders person next -> do
      _ <- dumpBatch batch
      str <- sendSingleStatement (Lib2.ListOrders person)
      interpretWithBatching' (next str) []
    Save next -> dumpBatch batch >> postAsString "save" >> interpretWithBatching' next []
    Load next -> dumpBatch batch >> postAsString "load" >> interpretWithBatching' next []

interpretWithBatching :: PizzaOrderProgram a -> IO a
interpretWithBatching prog = interpretWithBatching' prog []

dumpBatch :: [Lib2.Query] -> IO (Maybe String)
dumpBatch [] = return Nothing
dumpBatch [single] = Just <$> sendSingleStatement single
dumpBatch batch = Just <$> sendAsBatch batch

sendAsBatch :: [Lib2.Query] -> IO String
sendAsBatch = postAsString . Lib3.renderStatements . Lib3.Batch

sendSingleStatement :: Lib2.Query -> IO String
sendSingleStatement = postAsString . Lib3.renderStatements . Lib3.Single

postAsString :: String -> IO String
postAsString s = do
  let rawRequest = cs s :: ByteString
  putStrLn $ "Sending request:\n" ++ cs rawRequest
  resp <- post "http://localhost:3000" rawRequest
  return $ cs $ resp ^. responseBody

-- Test Interpreter
testInterpretator :: PizzaOrderProgram a -> IO a
testInterpretator p = do
  state <- newTVarIO Lib2.emptyState
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  initialState <- readTVarIO state
  putStrLn $ "Initial state:\n" ++ show initialState ++ "\n"
  _ <- forkIO $ Lib3.storageOpLoop chan
  testInterpretator' state chan p
  where
    testInterpretator' :: TVar Lib2.ProgramState -> Chan Lib3.StorageOp -> PizzaOrderProgram a -> IO a
    testInterpretator' _ _ (Pure a) = return a
    testInterpretator' state chan (Free step) = do
      next <- runStep state chan step
      putStrLn "State after:"
      newState <- readTVarIO state
      putStrLn $ show newState ++ "\n"
      testInterpretator' state chan next
    
runStep :: TVar Lib2.ProgramState -> Chan Lib3.StorageOp -> PizzaOrderAlgebra a -> IO a
runStep state chan (NewOrder orders next) = do
  transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.NewOrder orders) chan
  return next
runStep state chan (RemoveOrder person order next) = do
  transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.RemoveOrder person order) chan
  return next
runStep state chan (AddPizzaToOrder person pizza next) = do
  transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.AddPizzaToOrder person pizza) chan
  return next
runStep state chan (ListOrders person next) = do
  str <- transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.ListOrders person) chan
  return $ next str
runStep state chan (Save next) = do
  transitionAndPrint state Lib3.SaveCommand chan
  return next
runStep state chan (Load next) = do
  transitionAndPrint state Lib3.LoadCommand chan
  return next


transitionAndPrint :: TVar Lib2.ProgramState -> Lib3.Command -> Chan Lib3.StorageOp -> IO String
transitionAndPrint state cmd chan = do
  putStrLn $ "Command:\n" ++ show cmd
  res <- Lib3.stateTransition state cmd chan
  let str = either id (fromMaybe "Success" . fst) res 
  putStrLn "Result:"
  putStrLn str
  return str



-- Example Program
program :: PizzaOrderProgram (String, String)
program = do
  let pizza1 = Lib2.Pizza Lib2.Large Lib2.Thick [Lib2.Pepperoni] 3
      pizza2 = Lib2.Pizza Lib2.Medium Lib2.Thin [Lib2.Sausage] 4
      details1 = Lib2.OrderDetails Lib2.Delivery Lib2.CreditCard
      details2 = Lib2.OrderDetails Lib2.Pickup Lib2.Cash
      order1 = Lib2.SimpleOrder pizza1 details1
      order2 = Lib2.SimpleOrder pizza2 details2
  

  newOrderToStore [("Alice", order1), ("Bob", order2)]
  addPizzaToExistingOrder "Alice" pizza2
  save
  removeOrderFromStore "Bob" order2
  removeOrderFromStore "Alice" order1
  load
  b <- listCustomerOrders "Bob"
  a <- listCustomerOrders "Alice"
  return (b, a)

main :: IO ()
main = do
  -- usages:
  --str <- interpretorSingleRequest program
  --str <- interpretWithBatching program
  str <- testInterpretator program
  print str