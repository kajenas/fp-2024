{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib2
    ( Query(..)
    , Order(..)
    , Pizza(..)
    , Size(..)
    , Crust(..)
    , Topping(..)
    , OrderDetails(..)
    , OrderType(..)
    , PaymentMethod(..)
    , parseQuery
    , ProgramState(..)
    , emptyState
    , stateTransition
    , Parser
    ) where

import Data.Char (isAlpha, isDigit)
import Data.List
import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)

data Query = RemoveOrder String Order
           | NewOrder [(String, Order)]
           | AddPizzaToOrder String Pizza
           | ListOrders String
           deriving (Eq, Show)

data Order
  = SimpleOrder Pizza OrderDetails
  | OrderBundle [Pizza] [Order] OrderDetails
  deriving (Eq, Show)

data Pizza = Pizza {
  size :: Size,
  crust :: Crust,
  toppings :: [Topping],
  quantity :: Int
} deriving (Eq, Show)

data Size = Small | Medium | Large deriving (Eq, Show)
data Crust = Thin | Thick | Stuffed deriving (Eq, Show)
data Topping = Pepperoni | Mushrooms | Onions | Sausage | Bacon | ExtraCheese
    deriving (Eq, Show)


data OrderDetails = OrderDetails {
    orderType :: OrderType,
    paymentMethod :: PaymentMethod
} deriving (Eq, Show)

data OrderType = Delivery | Pickup deriving (Eq, Show)
data PaymentMethod = CreditCard | Cash | MobilePayment deriving (Eq, Show)


type Parser a = ExceptT [String] (State [String]) a

parseQuery :: String -> Either String Query
parseQuery input = case runState (runExceptT parseQuery') (words input) of
    (Left errs, _) -> Left (unwords errs)
    (Right query, _) -> Right query

parseQuery' :: Parser Query
parseQuery' = do
    input <- lift get
    case input of
        ("list":rest) -> do
            lift $ put rest
            parseListOrders
        ("New":"order":rest) -> do
            lift $ put rest
            parseNewOrder
        ("Remove":rest) -> do
            lift $ put rest
            parseRemoveOrder
        ("Add":"pizza":rest) -> do
            lift $ put rest
            parseAddPizzaToOrder
        _ -> throwE ["Error: Invalid command. Start with 'Remove', 'New order', 'Add pizza', or 'list'."]

parseListOrders :: Parser Query
parseListOrders = do
    input <- lift get
    case input of
        (name:rest) -> do
            lift $ put rest
            return $ ListOrders name
        _ -> throwE ["Error: 'list' requires a person name."]

parseNewOrder :: Parser Query
parseNewOrder = NewOrder <$> parseMultiplePersonOrders

parseRemoveOrder :: Parser Query
parseRemoveOrder = do
    input <- lift get
    case input of
        (name:rest) -> do
            lift $ put rest
            RemoveOrder name <$> parseOrder
        _ -> throwE ["Error: 'Remove' requires a person name."]

parseAddPizzaToOrder :: Parser Query
parseAddPizzaToOrder = do
    input <- lift get
    case input of
        (name:rest) -> do
            lift $ put rest
            pizza <- parsePizza
            remaining <- lift get
            if remaining == ["Confirm"]
                then return $ AddPizzaToOrder name pizza
                else throwE ["'Add pizza' must end with 'Confirm'. Found: " ++ unwords remaining]
        _ -> throwE ["Error: 'Add pizza' requires a person name."]

parseMultiplePersonOrders :: Parser [(String, Order)]
parseMultiplePersonOrders = parseMultiplePersonOrders' []

parseMultiplePersonOrders' :: [(String, Order)] -> Parser [(String, Order)]
parseMultiplePersonOrders' accum = do
    input <- lift get
    if null input
        then if null accum
            then throwE ["Error: No orders found."]
            else return (reverse accum)
        else case input of
            ("Confirm":rest) -> do
                lift $ put rest
                if null accum
                    then throwE ["Error: No orders before 'Confirm'."]
                    else return (reverse accum)
            _ -> do
                (person, rest) <- parsePerson
                lift $ put rest
                order <- parseOrder
                remaining <- lift get
                lift $ put remaining
                parseMultiplePersonOrders' ((person, order) : accum)

parseOrder :: Parser Order
parseOrder = do
    input <- lift get
    case input of
        ("Order":rest) -> do
            lift $ put rest
            pizza <- parsePizza
            SimpleOrder pizza <$> parseOrderDetails
        ("Bundle":rest) -> do
            lift $ put rest
            parseOrderBundle
        _ -> throwE ["Error: Expected 'Order' or 'Bundle'."]

parsePerson :: Parser (String, [String])
parsePerson = do
    input <- lift get
    let (person, rest) = break (== ' ') (unwords input)
    if null person
        then throwE ["Error: Expected person name."]
        else return (person, words rest)

parsePizza :: Parser Pizza
parsePizza = do
    input <- lift get
    case input of
        ("Pizza:":rest) -> do
            lift $ put rest
            parsePizzaDetails
        _ -> throwE ["Error: Expected 'Pizza:' at the beginning."]

parsePizzaDetails :: Parser Pizza
parsePizzaDetails = do
    input <- lift get
    let (sizeStr, rest1) = break (== ' ') (unwords input)
        (crustStr, rest2) = break (== ' ') (dropWhile (== ' ') rest1)
        (toppingsStr, rest3) = break (== ' ') (dropWhile (== ' ') rest2)
        (quantityStr, rest4) = break (== ' ') (dropWhile (== ' ') rest3)
    case (parseSize sizeStr, parseCrust crustStr, parseToppings toppingsStr, parseQuantity quantityStr) of
        (Right size, Right crust, Right toppings, Right quantity) -> do
            lift $ put (words $ dropWhile (== ' ') rest4)
            return $ Pizza size crust toppings quantity
        _ -> throwE ["Error: Invalid pizza format."]



parseSize :: String -> Either String Size
parseSize "Small" = Right Small
parseSize "Medium" = Right Medium
parseSize "Large" = Right Large
parseSize _ = Left "Error: Invalid size."

parseCrust :: String -> Either String Crust
parseCrust "Thin" = Right Thin
parseCrust "Thick" = Right Thick
parseCrust "Stuffed" = Right Stuffed
parseCrust _ = Left "Error: Invalid crust."

parseToppings :: String -> Either String [Topping]
parseToppings input = mapM parseTopping (words input)

parseTopping :: String -> Either String Topping
parseTopping "Pepperoni" = Right Pepperoni
parseTopping "Mushrooms" = Right Mushrooms
parseTopping "Onions" = Right Onions
parseTopping "Sausage" = Right Sausage
parseTopping "Bacon" = Right Bacon
parseTopping _ = Left "Error: Invalid topping."

parseOrderDetails :: Parser OrderDetails
parseOrderDetails = do
    input <- lift get
    let (orderTypeStr, rest1) = break (== ' ') (unwords input)
        (paymentMethodStr, rest2) = break (== ' ') (dropWhile (== ' ') rest1)
    case (parseOrderType orderTypeStr, parsePaymentMethod paymentMethodStr) of
        (Right orderType, Right paymentMethod) -> do
            lift $ put (words $ dropWhile (== ' ') rest2)
            return $ OrderDetails orderType paymentMethod
        _ -> throwE ["Error: Invalid order details format."]

parseOrderType :: String -> Either String OrderType
parseOrderType "Delivery" = Right Delivery
parseOrderType "Pickup" = Right Pickup
parseOrderType _ = Left "Error: Invalid order type."

-- | Parse a quantity as an integer from the input string.
parseQuantity :: String -> Either String Int
parseQuantity str
    | all isDigit str = Right (read str)
    | otherwise = Left "Error: Invalid quantity."


parsePaymentMethod :: String -> Either String PaymentMethod
parsePaymentMethod "CreditCard" = Right CreditCard
parsePaymentMethod "Cash" = Right Cash
parsePaymentMethod "MobilePayment" = Right MobilePayment
parsePaymentMethod _ = Left "Error: Invalid payment method."


parseOrderBundle :: Parser Order
parseOrderBundle = do
    pizzas <- parseMultiplePizzas
    subOrders <- parseMultipleOrders
    OrderBundle pizzas subOrders <$> parseOrderDetails

parseMultiplePizzas :: Parser [Pizza]
parseMultiplePizzas = parseMultiplePizzas' []

parseMultiplePizzas' :: [Pizza] -> Parser [Pizza]
parseMultiplePizzas' accum = do
    input <- lift get
    case input of
        ("Pizza:":_) -> do
            pizza <- parsePizza
            parseMultiplePizzas' (pizza : accum)
        _ -> return (reverse accum) -- Stop when there are no more "Pizza:" tokens


parseMultipleOrders :: Parser [Order]
parseMultipleOrders = parseMultipleOrders' []

parseMultipleOrders' :: [Order] -> Parser [Order]
parseMultipleOrders' accum = do
    input <- lift get
    if null input
        then return (reverse accum)
        else case input of
            ("Order":rest) -> do
                lift $ put rest
                order <- parseOrder
                remaining <- lift get
                lift $ put remaining
                parseMultipleOrders' (order : accum)
            _ -> return (reverse accum)

-- New utility: Combine error messages for bundles
combineErrors :: [Either String a] -> Either String [a]
combineErrors results =
    case partitionEithers results of
        ([], rights) -> Right rights
        (errors, _)  -> Left $ intercalate "; " errors
  where
    partitionEithers = foldr (\e (ls, rs) -> either (\l -> (l:ls, rs)) (\r -> (ls, r:rs)) e) ([], [])

data ProgramState = ProgramState {
    orders :: [(String, Order)]
} deriving (Eq, Show)

emptyState :: ProgramState
emptyState = ProgramState { orders = [] }

stateTransition :: ProgramState -> Query -> Either String (Maybe String, ProgramState)
stateTransition state query = case query of
    ListOrders person -> handleListOrders state person
    RemoveOrder person _ -> handleRemoveOrder state person
    NewOrder newOrders -> handleNewOrder state newOrders
    AddPizzaToOrder person pizza -> handleAddPizzaToOrder state person pizza

handleListOrders :: ProgramState -> String -> Either String (Maybe String, ProgramState)
handleListOrders state person =
    case lookup person (orders state) of
        Nothing -> Right (Just $ "No orders found for " ++ person, state)
        Just order -> Right (Just $ formatOrder person order, state)

handleRemoveOrder :: ProgramState -> String -> Either String (Maybe String, ProgramState)
handleRemoveOrder state person =
    case lookup person (orders state) of
        Nothing -> Left $ "No order found for " ++ person
        Just _ -> Right (Just $ "Order removed for " ++ person,
                         state { orders = filter ((/= person) . fst) (orders state) })

handleNewOrder :: ProgramState -> [(String, Order)] -> Either String (Maybe String, ProgramState)
handleNewOrder state newOrders =
    let conflictingCustomers = filter (\(person, _) -> any ((== person) . fst) (orders state)) newOrders
    in if not (null conflictingCustomers)
        then Left $ "Orders already exist for: " ++ show (map fst conflictingCustomers)
        else Right (Just $ "Added orders for: " ++ show (map fst newOrders),
                   state { orders = orders state ++ newOrders })

handleAddPizzaToOrder :: ProgramState -> String -> Pizza -> Either String (Maybe String, ProgramState)
handleAddPizzaToOrder state person newPizza =
    case lookup person (orders state) of
        Nothing -> Left $ "No existing order found for " ++ person
        Just existingOrder ->
            case addPizzaToOrder existingOrder newPizza of
                Left err -> Left err
                Right updatedOrder ->
                    Right (Just $ "Added pizza to order for " ++ person,
                          state { orders = updateOrders person updatedOrder (orders state) })

addPizzaToOrder :: Order -> Pizza -> Either String Order
addPizzaToOrder (SimpleOrder existingPizza details) newPizza =
    Right $ OrderBundle [existingPizza, newPizza] [] details
addPizzaToOrder (OrderBundle pizzas subOrders details) newPizza =
    Right $ OrderBundle (pizzas ++ [newPizza]) subOrders details

updateOrders :: String -> Order -> [(String, Order)] -> [(String, Order)]
updateOrders person newOrder = map (\(p, o) -> if p == person then (p, newOrder) else (p, o))

formatOrder :: String -> Order -> String
formatOrder person (SimpleOrder pizza details) =
    "Order for " ++ person ++ ":\n" ++ formatPizza pizza ++ "\n" ++ formatDetails details
formatOrder person (OrderBundle pizzas subOrders details) =
    "Bundle order for " ++ person ++ ":\n" ++
    concatMap formatPizza pizzas ++ "\n" ++
    "Sub-orders:\n" ++ concatMap (formatOrder person) subOrders ++ "\n" ++
    formatDetails details

formatDetails :: OrderDetails -> String
formatDetails (OrderDetails orderType paymentMethod) =
    "  Order type: " ++ show orderType ++ "\n" ++
    "  Payment method: " ++ show paymentMethod


formatPizza :: Pizza -> String
formatPizza (Pizza size crust toppings quantity) =
    "  " ++ show size ++ " " ++ show crust ++ " crust pizza with " ++
    formatToppings toppings ++ ", quantity: " ++ show quantity ++ "\n"

formatToppings :: [Topping] -> String
formatToppings [] = "no toppings"
formatToppings toppings = unwords (map show toppings)

arePizzasCompatible :: Pizza -> Pizza -> Bool
arePizzasCompatible p1 p2 = size p1 == size p2 && crust p1 == crust p2
