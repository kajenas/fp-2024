{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib3
    ( stateTransition,
      StorageOp (..),
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
import Data.List (intercalate)
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
        in sequence $ map Lib2.parseQuery queries

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
parseStatements input = 
    let lines' = lines input
        -- Handle empty input
        parsedLines = if null lines' 
                     then [Lib2.parseQuery ""]
                     else map Lib2.parseQuery lines'
    in if length lines' == 1
       then case head parsedLines of
            Right query -> Right (Single query, "")
            Left err -> Left err
       else if all isRightQuery parsedLines
            then Right (Batch (map fromRight parsedLines), "")
            else Left "Failed to parse some queries"
  where
    isRightQuery (Right _) = True
    isRightQuery _ = False
    fromRight (Right x) = x
    fromRight _ = error "Impossible case - filtered by isRightQuery"

-- | Convert state to statements for persistence
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State orders) = 
    Batch [Lib2.NewOrder orders]

-- | Convert statements to string format for storage
renderStatements :: Statements -> String
renderStatements (Batch queries) = 
    intercalate "\n" (map formatQuery queries)
renderStatements (Single query) = formatQuery query

-- | Helper function to format queries consistently
formatQuery :: Lib2.Query -> String
formatQuery query = case query of
    Lib2.NewOrder orders -> 
        unwords $ "New order" : concatMap formatOrderPair orders ++ ["Confirm"]
    Lib2.RemoveOrder name _ ->
        unwords ["Remove", name, "Confirm"]
    Lib2.AddPizzaToOrder name pizza ->
        unwords ["Add pizza", name, formatPizza pizza, "Confirm"]
    Lib2.ListOrders name ->
        unwords ["list", name, "Confirm"]
  where
    formatOrderPair (name, order) = 
        [unwords [name, formatOrder order]]
        
-- | Helper function to format orders
formatOrder :: Lib2.Order -> String
formatOrder (Lib2.SimpleOrder pizza details) =
    unwords ["Order", formatPizza pizza, formatOrderDetails details]
formatOrder (Lib2.OrderBundle pizzas subOrders details) =
    unwords $ ["Bundle"] ++ map formatPizza pizzas ++ 
              concatMap (return . formatOrder) subOrders ++ 
              [formatOrderDetails details]

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
formatSize Lib2.Small = "small"
formatSize Lib2.Medium = "medium"
formatSize Lib2.Large = "large"

formatCrust :: Lib2.Crust -> String
formatCrust Lib2.Thin = "thin"
formatCrust Lib2.Thick = "thick"
formatCrust Lib2.Stuffed = "stuffed"

formatTopping :: Lib2.Topping -> String
formatTopping Lib2.Pepperoni = "pepperoni"
formatTopping Lib2.Mushrooms = "mushrooms"
formatTopping Lib2.Onions = "onions"
formatTopping Lib2.Sausage = "sausage"
formatTopping Lib2.Bacon = "bacon"
formatTopping Lib2.ExtraCheese = "extra_cheese"

formatOrderDetails :: Lib2.OrderDetails -> String
formatOrderDetails details = unwords [
    formatOrderType (Lib2.orderType details),
    formatPaymentMethod (Lib2.paymentMethod details)
    ]

formatOrderType :: Lib2.OrderType -> String
formatOrderType Lib2.Delivery = "Delivery"
formatOrderType Lib2.Pickup = "Pickup"

formatPaymentMethod :: Lib2.PaymentMethod -> String
formatPaymentMethod Lib2.CreditCard = "Credit"
formatPaymentMethod Lib2.Cash = "Cash"
formatPaymentMethod Lib2.MobilePayment = "Mobile"

-- | State transition with file operations
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String, String))
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
applyStatements :: Lib2.State -> Statements -> Either String Lib2.State
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