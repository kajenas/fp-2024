{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib3
    ( stateTransition,
      StorageOp (..),
      Statements (..),
      Command (..),
      storageOpLoop,
      parseCommand,
      parseStatements,
      marshallState,
      renderStatements,
      parseBatch,
    ) where

import Control.Concurrent (Chan, readChan, writeChan)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import qualified Lib2
import Data.List (intercalate, isPrefixOf, isInfixOf)
import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar
import Data.Char (isSpace)
import Control.Monad (void)


data StorageOp
  = Save String (Chan ())
  | Load (Chan String)

-- | Sequential file access loop
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
    op <- readChan chan
    case op of
        Save content responseChan -> do
            writeFile "state.txt" content
            writeChan responseChan ()
            storageOpLoop chan
        Load responseChan -> do
            content <- readFile "state.txt"
            writeChan responseChan content
            storageOpLoop chan

data Statements = Batch [Lib2.Query] |
                 Single Lib2.Query
                 deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parse batch of statements between BEGIN and END
parseBatch :: String -> Either String [Lib2.Query]
parseBatch input = case words (trim input) of
    "BEGIN":rest -> case break (=="END") rest of
        (statements, ["END"]) -> parseQueries $ unwords statements
        _ -> Left "Missing END keyword or malformed batch"
    _ -> Left "Batch must start with BEGIN"
  where
    parseQueries :: String -> Either String [Lib2.Query]
    parseQueries str =
        let queries = filter (not . null) $ map trim $ splitOn ';' str
        in mapM Lib2.parseQuery queries

    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

    splitOn :: Char -> String -> [String]
    splitOn c s = case break (==c) s of
        (x, []) -> [x]
        (x, _:xs) -> x : splitOn c xs

-- | Parse user commands including load/save operations
parseCommand :: String -> Either String (Command, String)
parseCommand input = case words input of
    "load":rest -> Right (LoadCommand, unwords rest)
    "save":rest -> Right (SaveCommand, unwords rest)
    "BEGIN":_ -> case parseBatch input of
        Right queries -> Right (StatementCommand (Batch queries), "")
        Left err -> Left err
    _ -> case parseStatements input of
        Right (stmts, remaining) -> Right (StatementCommand stmts, remaining)
        Left err -> Left err


-- | Parse individual statements or batch of statements
parseStatements :: String -> Either String (Statements, String)
parseStatements input
  | "BEGIN" `isPrefixOf` input && "END" `isInfixOf` input =
      let lines' = lines input
          content = unlines $ tail $ init lines' -- Remove BEGIN and END
      in case map Lib2.parseQuery (filter (not . null) $ lines content) of
           queries | all isRight queries ->
               Right (Batch (map fromRight queries), "")
           _ -> Left "Failed to parse some queries in batch"
  | otherwise =
      case Lib2.parseQuery (trim input) of
        Right query -> Right (Single query, "")
        Left err -> Left err
  where
    isRight (Right _) = True
    isRight _ = False
    fromRight (Right x) = x
    fromRight _ = error "Impossible case - filtered by isRight"
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse


-- | Convert state to statements for persistence
marshallState :: Lib2.ProgramState -> Statements
marshallState (Lib2.ProgramState orders) =
    Batch [Lib2.NewOrder orders]

-- Render a list of statements (queries) as a string
renderStatements :: Statements -> String
renderStatements (Batch queries) =
    "BEGIN\n" ++ unlines (map formatQuery queries) ++ "END"
renderStatements (Single query) = formatQuery query



-- Update formatQuery to add 'Confirm' at the end for RemoveOrder
formatQuery :: Lib2.Query -> String
formatQuery query = case query of
    Lib2.NewOrder orders ->
        unwords $ "New order" : concatMap formatOrderPair orders ++ ["Confirm"]
    Lib2.RemoveOrder name order ->
        unwords ["Remove", name, formatOrder order, "Confirm"]
    Lib2.AddPizzaToOrder name pizza ->
        unwords ["Add pizza", name, formatPizza pizza, "Confirm"]
    Lib2.ListOrders name ->
        unwords ["list", name, "Confirm"]
  where
    formatOrderPair (name, order) =
        [unwords [name, formatOrder order]]

-- Ensure formatOrder handles nested structures more robustly
formatOrder :: Lib2.Order -> String
formatOrder (Lib2.SimpleOrder pizza details) =
    unwords ["Order", formatPizza pizza, formatOrderDetails details]
formatOrder (Lib2.OrderBundle pizzas subOrders details) =
    unwords $
        ["Bundle"] ++
        map formatPizza pizzas ++
        concatMap formatSubOrder subOrders ++
        [formatOrderDetails details]
  where
    formatSubOrder (Lib2.SimpleOrder pizza details) =
        [unwords ["Order", formatPizza pizza, formatOrderDetails details]]
    formatSubOrder (Lib2.OrderBundle subPizzas nestedOrders subDetails) =
        unwords ["Bundle"] :
        map formatPizza subPizzas ++
        concatMap formatSubOrder nestedOrders ++
        [formatOrderDetails subDetails]

-- | Helper function to format pizzas
formatPizza :: Lib2.Pizza -> String
formatPizza pizza = unwords [
    "Pizza:",
    formatSize (Lib2.size pizza),
    formatCrust (Lib2.crust pizza),
    unwords (map formatTopping (Lib2.toppings pizza)),
    show (Lib2.quantity pizza)
    ]

-- | Helper functions for formatting various components
formatSize :: Lib2.Size -> String
formatSize Lib2.Small = "Small"
formatSize Lib2.Medium = "Medium"
formatSize Lib2.Large = "Large"

formatCrust :: Lib2.Crust -> String
formatCrust Lib2.Thin = "Thin"
formatCrust Lib2.Thick = "Thick"
formatCrust Lib2.Stuffed = "Stuffed"

formatTopping :: Lib2.Topping -> String
formatTopping Lib2.Pepperoni = "Pepperoni"
formatTopping Lib2.Mushrooms = "Mushrooms"
formatTopping Lib2.Onions = "Onions"
formatTopping Lib2.Sausage = "Sausage"
formatTopping Lib2.Bacon = "Bacon"

formatOrderDetails :: Lib2.OrderDetails -> String
formatOrderDetails details = unwords [
    formatOrderType (Lib2.orderType details),
    formatPaymentMethod (Lib2.paymentMethod details)
    ]

formatOrderType :: Lib2.OrderType -> String
formatOrderType Lib2.Delivery = "Delivery"
formatOrderType Lib2.Pickup = "Pickup"

formatPaymentMethod :: Lib2.PaymentMethod -> String
formatPaymentMethod Lib2.CreditCard = "CreditCard"
formatPaymentMethod Lib2.Cash = "Cash"
formatPaymentMethod Lib2.MobilePayment = "Mobile"

-- | State transition with file operations
stateTransition :: TVar Lib2.ProgramState -> Command -> Chan StorageOp -> IO (Either String (Maybe String, String))
stateTransition stateVar command ioChan = case command of
    LoadCommand -> do
        responseChan <- newChan
        writeChan ioChan (Load responseChan)
        loadedContent <- readChan responseChan
        case parseStatements loadedContent of
            Right (statements, _) -> atomically $ do
                case applyStatements Lib2.emptyState statements of
                    Right newState -> do
                        writeTVar stateVar newState
                        return $ Right (Just "State loaded successfully", show newState)
                    Left err -> return $ Left $ "Error loading state: " ++ err
            Left err -> return $ Left $ "Error parsing saved state: " ++ err

    SaveCommand -> do
        currentState <- readTVarIO stateVar
        let serializedState = renderStatements (marshallState currentState)
        responseChan <- newChan
        writeChan ioChan (Save serializedState responseChan)
        _ <- readChan responseChan
        return $ Right (Just "State saved successfully", show currentState)

    StatementCommand statements -> atomically $ do
        currentState <- readTVar stateVar
        case applyStatements currentState statements of
            Right newState -> do
                writeTVar stateVar newState
                return $ Right (Nothing, show newState)
            Left err -> return $ Left err


-- | Helper function to apply statements to state
applyStatements :: Lib2.ProgramState -> Statements -> Either String Lib2.ProgramState
applyStatements state (Single query) =
    case Lib2.stateTransition state query of
        Right (_, newState) -> Right newState
        Left err -> Left err
applyStatements state (Batch queries) =
    foldl (\stateResult query ->
        stateResult >>= \s -> case Lib2.stateTransition s query of
            Right (_, newState) -> Right newState
            Left err -> Left err
    ) (Right state) queries