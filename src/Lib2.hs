{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
    , State(..)
    , emptyState
    , stateTransition
    , Parser
    ) where

import Data.Char (isAlpha, isDigit)
import Data.List

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

type Parser a = [String] -> Either String (a, [String])

parseQuery :: String -> Either String Query
parseQuery input = case words input of
    "list":rest -> parseListOrders rest
    "New":"order":rest -> parseNewOrder rest
    ["Remove"] -> Left "Error: 'Remove' requires a person name."
    "Remove":rest -> parseRemoveOrder rest
    "Add":"pizza":rest -> parseAddPizzaToOrder rest
    _ -> Left "Error: Invalid command. Start with 'Remove', 'New order', 'Add pizza', or 'list'."

-- Improved `parseNewOrder` with explicit parsing for multiple orders
parseNewOrder :: [String] -> Either String Query
parseNewOrder [] = Left "Error: 'New order' requires a person name."
parseNewOrder input = case parseMultiplePersonOrders input [] of
    Right (orders, ["Confirm"]) -> Right $ NewOrder orders
    Right (_, remaining) -> Left $ "Error: New order must end with 'Confirm'. Remaining input: " ++ unwords remaining
    Left err -> Left err

-- Improved `parseRemoveOrder`
parseRemoveOrder :: [String] -> Either String Query
parseRemoveOrder [] = Left "Error: 'Remove' requires a person name."
parseRemoveOrder input = case parsePerson input of
    Right (person, ["Confirm"]) -> Right $ RemoveOrder person dummyOrder
    Right (_, rest) -> Left $ "'Remove' must end with 'Confirm'. Found: " ++ unwords rest
    Left err -> Left err
  where
    dummyOrder = SimpleOrder (Pizza Small Thin [] 1) (OrderDetails Pickup Cash)

-- Improved `parseAddPizzaToOrder`
parseAddPizzaToOrder :: [String] -> Either String Query
parseAddPizzaToOrder input = case parsePerson input of
    Right (person, rest) -> case parsePizza rest of
        Right (pizza, ["Confirm"]) -> Right $ AddPizzaToOrder person pizza
        Right (_, remaining) -> Left $ "'Add pizza' must end with 'Confirm'. Found: " ++ unwords remaining
        Left err -> Left err
    Left err -> Left err

-- Improved `parseMultiplePersonOrders`
parseMultiplePersonOrders :: [String] -> [(String, Order)] -> Either String ([(String, Order)], [String])
parseMultiplePersonOrders [] accum = 
    if null accum then Left "Error: No orders found." else Right (reverse accum, [])
parseMultiplePersonOrders ("Confirm":rest) accum = 
    if null accum then Left "Error: No orders before 'Confirm'." else Right (reverse accum, ["Confirm"])
parseMultiplePersonOrders input accum = case parsePerson input of
    Right (person, rest) -> case parseOrder rest of
        Right (order, remaining) -> parseMultiplePersonOrders remaining ((person, order) : accum)
        Left err -> Left $ "Error parsing order for " ++ person ++ ": " ++ err
    Left err -> Left err

-- Improved `parseOrder`
parseOrder :: Parser Order
parseOrder ("Order":rest) = do
    (pizza, afterPizza) <- parsePizza rest
    (details, finalRest) <- parseOrderDetails afterPizza
    Right (SimpleOrder pizza details, finalRest)
parseOrder ("Bundle":rest) = parseOrderBundle rest
parseOrder _ = Left "Error: Expected 'Order' or 'Bundle'."

-- Improved `parseOrderBundle`
parseOrderBundle :: Parser Order
parseOrderBundle input = do
    (pizzas, afterPizzas) <- parseMultiplePizzas input
    (subOrders, afterSubOrders) <- parseMultipleOrders afterPizzas
    (details, finalRest) <- parseOrderDetails afterSubOrders
    Right (OrderBundle pizzas subOrders details, finalRest)

-- Other helper parsers (parsePizza, parseSize, etc.) remain unchanged

-- New utility: Combine error messages for bundles
combineErrors :: [Either String a] -> Either String [a]
combineErrors results = 
    case partitionEithers results of
        ([], rights) -> Right rights
        (errors, _)  -> Left $ intercalate "; " errors
  where
    partitionEithers = foldr (\e (ls, rs) -> either (\l -> (l:ls, rs)) (\r -> (ls, r:rs)) e) ([], [])

parseListOrders :: [String] -> Either String Query
parseListOrders (name:"Confirm":_) = Right $ ListOrders name
parseListOrders _ = Left "List command must be 'list <name> Confirm'"


parsePerson :: Parser String
parsePerson [] = Left "Expected person name"
parsePerson (name:rest)
    | all isAlpha name = Right (name, rest)
    | otherwise = Left "Person name must contain only letters"

parsePizza :: Parser Pizza
parsePizza ("Pizza:":rest) = 
    andThen parseSize
            (andThen parseCrust
                     (andThen parseToppings parseQuantity
                              (\toppings quantity -> (toppings, quantity)))
                     (\crust (toppings, quantity) -> 
                         Pizza Small crust toppings quantity))
            (\size pizza -> pizza { size = size })
            rest
parsePizza _ = Left "Expected 'Pizza:'"

parseMultiplePizzas :: Parser [Pizza]
parseMultiplePizzas input = case parsePizza input of
    Right (pizza, rest) -> case parseMultiplePizzas rest of
        Right (morePizzas, finalRest) -> Right (pizza : morePizzas, finalRest)
        Left _ -> Right ([pizza], rest)  -- End of pizzas
    Left _ -> Right ([], input)  -- No pizzas found

parseMultipleOrders :: Parser [Order]
parseMultipleOrders ("Order":rest) = case parseSimpleOrder rest of
    Right (order, remainingLines) -> case parseMultipleOrders remainingLines of
        Right (moreOrders, finalRest) -> Right (order : moreOrders, finalRest)
        Left _ -> Right ([order], remainingLines)
    Left err -> Left err
parseMultipleOrders input = Right ([], input)  -- No more orders

parseSimpleOrder :: Parser Order
parseSimpleOrder input = 
    case parsePizza input of
        Right (pizza, remainingLines) -> 
            Right (SimpleOrder pizza (OrderDetails Pickup Cash), remainingLines)
        Left err -> Left err

parseList :: Parser a -> Parser [a]
parseList parser input = case parser input of
    Right (item, rest) -> case parseList parser rest of
        Right (items, finalRest) -> Right (item:items, finalRest)
        Left _ -> Right ([item], rest)
    Left _ -> Right ([], input)

andThen :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
andThen pa pb f input = 
    case pa input of
        Left err -> Left err
        Right (a, rest1) -> 
            case pb rest1 of
                Left err -> Left err
                Right (b, rest2) -> Right (f a b, rest2)

parseSize :: Parser Size
parseSize [] = Left "Expected size"
parseSize (sizeStr:rest) = case sizeStr of
    "small" -> Right (Small, rest)
    "medium" -> Right (Medium, rest)
    "large" -> Right (Large, rest)
    _ -> Left "Invalid size"

parseCrust :: Parser Crust
parseCrust [] = Left "Expected crust"
parseCrust (crustStr:rest) = case crustStr of
    "thin" -> Right (Thin, rest)
    "thick" -> Right (Thick, rest)
    "stuffed" -> Right (Stuffed, rest)
    _ -> Left "Invalid crust"

parseToppings :: Parser [Topping]
parseToppings = parseList parseTopping

parseTopping :: Parser Topping
parseTopping [] = Left "Expected topping"
parseTopping (topping:rest) = case topping of
    "pepperoni" -> Right (Pepperoni, rest)
    "mushrooms" -> Right (Mushrooms, rest)
    "onions" -> Right (Onions, rest)
    "sausage" -> Right (Sausage, rest)
    "bacon" -> Right (Bacon, rest)
    "extra_cheese" -> Right (ExtraCheese, rest)
    _ -> Left "Invalid topping"

parseQuantity :: Parser Int
parseQuantity [] = Left "Expected quantity"
parseQuantity (q:rest) = case reads q of
    [(n, "")] | n > 0 -> Right (n, rest)
    _ -> Left "Invalid quantity"

parseOrderDetails :: Parser OrderDetails
parseOrderDetails = 
    andThen parseOrderType parsePaymentMethod OrderDetails

parseOrderType :: Parser OrderType
parseOrderType [] = Left "Expected order type"
parseOrderType ("Delivery":rest) = Right (Delivery, rest)
parseOrderType ("Pickup":rest) = Right (Pickup, rest)
parseOrderType _ = Left "Invalid order type"

parsePaymentMethod :: Parser PaymentMethod
parsePaymentMethod [] = Left "Expected payment method"
parsePaymentMethod ("Credit":rest) = Right (CreditCard, rest)
parsePaymentMethod ("Cash":rest) = Right (Cash, rest)
parsePaymentMethod ("Mobile":rest) = Right (MobilePayment, rest)
parsePaymentMethod _ = Left "Invalid payment method"

data State = State {
    orders :: [(String, Order)]
} deriving (Eq, Show)

emptyState :: State
emptyState = State { orders = [] }

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
    ListOrders person -> handleListOrders state person
    RemoveOrder person _ -> handleRemoveOrder state person
    NewOrder newOrders -> handleNewOrder state newOrders
    AddPizzaToOrder person pizza -> handleAddPizzaToOrder state person pizza

handleListOrders :: State -> String -> Either String (Maybe String, State)
handleListOrders state person =
    case lookup person (orders state) of
        Nothing -> Right (Just $ "No orders found for " ++ person, state)
        Just order -> Right (Just $ formatOrder person order, state)

handleRemoveOrder :: State -> String -> Either String (Maybe String, State)
handleRemoveOrder state person =
    case lookup person (orders state) of
        Nothing -> Left $ "No order found for " ++ person
        Just _ -> Right (Just $ "Order removed for " ++ person, 
                         state { orders = filter ((/= person) . fst) (orders state) })

handleNewOrder :: State -> [(String, Order)] -> Either String (Maybe String, State)
handleNewOrder state newOrders = 
    let conflictingCustomers = filter (\(person, _) -> any ((== person) . fst) (orders state)) newOrders
    in if not (null conflictingCustomers)
        then Left $ "Orders already exist for: " ++ show (map fst conflictingCustomers)
        else Right (Just $ "Added orders for: " ++ show (map fst newOrders),
                   state { orders = orders state ++ newOrders })

handleAddPizzaToOrder :: State -> String -> Pizza -> Either String (Maybe String, State)
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