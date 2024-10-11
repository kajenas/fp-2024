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
    ) where

import Data.Char (isAlpha, isDigit)

-- | An entity which represents user input.
data Query = RemoveOrder String Order
            | NewOrder [(String, Order)]
            | AddPizzaToOrder String Pizza
            | ListOrders String
            deriving (Eq, Show)

data Order = SimpleOrder Pizza OrderDetails
           | OrderBundle Pizza Pizza Pizza OrderDetails
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



parseQuery :: String -> Either String Query
parseQuery input = case words input of
    "list":rest -> case parseListOrders ("list":rest) of
        Right (query, []) -> Right query
        Left err -> Left $ "Invalid list command: " ++ err
    "New":"order":rest -> parseNewOrder rest
    ["Remove"] -> Left "Expected person name after 'Remove'"
    "Remove":rest -> parseRemoveOrder rest
    "Add":"pizza":rest -> parseAddPizzaToOrder rest
    _ -> Left "Invalid command. Must start with 'Remove', 'New order', or 'Add pizza'"

parseListOrders :: Parser Query
parseListOrders ("list":name:rest) = 
    case parsePerson [name] of
        Right (person, []) -> 
            case rest of
                ["Confirm"] -> Right (ListOrders person, [])
                _ -> Left "List command must end with 'Confirm'"
        Left err -> Left err
parseListOrders _ = Left "Expected 'list' followed by a person's name"



-- <new_order> ::= "New order\n" <multiple_person_orders> <confirmation>
parseNewOrder :: [String] -> Either String Query
parseNewOrder [] = Left "Expected person name after 'New order'"
parseNewOrder lines =
    case parseMultiplePersonOrders lines [] of
        Right (orders, "Confirm":_) -> Right $ NewOrder orders
        Right (_, remaining) -> Left $ "Order must end with 'Confirm', but found: " ++ show remaining
        Left err -> Left err
  
-- <add_pizza_to_order> ::= "Add pizza\n" <person> <pizza> <confirmation>
parseAddPizzaToOrder :: [String] -> Either String Query
parseAddPizzaToOrder lines =
  case parsePerson lines of
    Right (person, remainingLines1) ->
      case parsePizza remainingLines1 of
        Right (pizza, ["Confirm"]) -> Right $ AddPizzaToOrder person pizza
        _ -> Left "Add pizza must end with 'Confirm'"
    Left err -> Left err
  
-- <multiple_person_orders> :: = <person_order> | <person_order> <multiple_person_orders>
parseMultiplePersonOrders :: [String] -> [(String, Order)] -> Either String ([(String, Order)], [String])
parseMultiplePersonOrders [] accum = 
    if null accum
    then Left "Expected at least one order"
    else Right (reverse accum, [])
parseMultiplePersonOrders ("Confirm":rest) accum =
    if null accum
    then Left "Expected at least one order before 'Confirm'"
    else Right (reverse accum, ["Confirm"])
parseMultiplePersonOrders lines accum =
  case parsePerson lines of
    Right (person, remainingLines1) ->
      case parseOrder remainingLines1 of
        Right (order, remainingLines2) ->
          parseMultiplePersonOrders remainingLines2 ((person, order) : accum)
        Left err -> Left err
    Left err -> Left err

-- <remove_order> ::= <person> "Confirm"
parseRemoveOrder :: [String] -> Either String Query
parseRemoveOrder lines =
  case parsePerson lines of
    Right (person, ["Confirm"]) -> Right $ RemoveOrder person (SimpleOrder (Pizza Small Thin [] 1) (OrderDetails Pickup Cash))  -- Placeholder order, not used anymore
    _ -> Left "Remove order must include a person and end with 'Confirm'"

 -- <person> ::= ([a-z] | [A-Z])+
parsePerson :: Parser String
parsePerson [] = Left "Expected person name"
parsePerson (name:rest)
    | all isAlpha name = Right (name, rest)
    | otherwise = Left "Person name must contain only letters"

-- <pizza> ::= "Pizza:\n" <size> <crust> <toppings> <quantity>
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


-- <order> ::= <simple_order> | <order_bundle>
parseOrder :: Parser Order
parseOrder input = bundleOrder `or2` simpleOrder $ input
  where
    bundleOrder ("Order":"bundle":rest) = 
        andThen parsePizza
                (andThen parsePizza
                         (andThen parsePizza parseOrderDetails
                                  (\pizza3 details -> (pizza3, details)))
                         (\pizza2 (pizza3, details) -> (pizza2, pizza3, details)))
                (\pizza1 (pizza2, pizza3, details) -> 
                    OrderBundle pizza1 pizza2 pizza3 details)
                rest
    bundleOrder _ = Left "Not a bundle order"
    
    simpleOrder = andThen parsePizza parseOrderDetails SimpleOrder
    
type Parser a = [String] -> Either String (a, [String])

-- Combinators:
and2 :: Parser a -> Parser b -> Parser (a, b)
and2 pa pb = \input -> 
    case pa input of
        Left err -> Left err
        Right (resultA, restA) -> 
            case pb restA of
                Left err -> Left err
                Right (resultB, restB) -> Right ((resultA, resultB), restB)


or2 :: Parser a -> Parser a -> Parser a
or2 pa pb = \input ->
    case pa input of
        Right result -> Right result
        Left _ -> pb input
        
-- and2 helper:
andThen :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
andThen pa pb f = \input -> 
    case pa input of
        Left err -> Left err
        Right (a, rest1) -> 
            case pb rest1 of
                Left err -> Left err
                Right (b, rest2) -> Right (f a b, rest2)

parseSize :: Parser Size
parseSize [] = Left "Expected size"
parseSize (size:rest) = case size of
    "small" -> Right (Small, rest)
    "medium" -> Right (Medium, rest)
    "large" -> Right (Large, rest)
    _ -> Left "Invalid size"

parseCrust :: Parser Crust
parseCrust [] = Left "Expected crust"
parseCrust (crust:rest) = case crust of
    "thin" -> Right (Thin, rest)
    "thick" -> Right (Thick, rest)
    "stuffed" -> Right (Stuffed, rest)
    _ -> Left "Invalid crust"

parseQuantity :: Parser Int
parseQuantity [] = Left "Expected quantity"
parseQuantity (q:rest) = case reads q of
    [(n, "")] | n > 0 -> Right (n, rest)
    _ -> Left "Invalid quantity"

-- <toppings> ::= <topping_list>
parseToppings :: Parser [Topping]
parseToppings = parseToppingsHelper []
  where
    parseToppingsHelper :: [Topping] -> Parser [Topping]
    parseToppingsHelper accum [] = Left "Expected toppings"
    parseToppingsHelper accum (topping:rest) = case topping of
        "pepperoni" -> parseToppingsHelper (Pepperoni:accum) rest
        "mushrooms" -> parseToppingsHelper (Mushrooms:accum) rest
        "onions" -> parseToppingsHelper (Onions:accum) rest
        "sausage" -> parseToppingsHelper (Sausage:accum) rest
        "bacon" -> parseToppingsHelper (Bacon:accum) rest
        "extra cheese" -> parseToppingsHelper (ExtraCheese:accum) rest
        _ | all isDigit topping -> Right (reverse accum, topping:rest)
          | otherwise -> Left "Invalid topping"



-- <order_details> ::= <order_type> <payment_method>
parseOrderDetails :: Parser OrderDetails
parseOrderDetails = andThen parseOrderType parsePaymentMethod OrderDetails


-- <order_type> ::=  ("Delivery" | "Pickup")
parseOrderType :: [String] -> Either String (OrderType, [String])
parseOrderType (ot:rest) = case ot of
  "Delivery" -> Right (Delivery, rest)
  "Pickup" -> Right (Pickup, rest)
  _ -> Left "Invalid order type"
parseOrderType [] = Left "Expected order type"

-- <payment_method> ::=  ("Credit Card" | "Cash" | "Mobile Payment")
parsePaymentMethod :: [String] -> Either String (PaymentMethod, [String])
parsePaymentMethod (pm:rest) = case pm of
  "Credit Card" -> Right (CreditCard, rest)
  "Cash" -> Right (Cash, rest)
  "Mobile Payment" -> Right (MobilePayment, rest)
  _ -> Left "Invalid payment method"
parsePaymentMethod [] = Left "Expected payment method"


data State = State {
    orders :: [(String, Order)]
} deriving (Eq, Show)

emptyState :: State
emptyState = State { orders = [] }

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
    ListOrders person -> handleListOrders state person
    RemoveOrder person order -> handleRemoveOrder state person order
    NewOrder newOrders -> handleNewOrder state newOrders
    AddPizzaToOrder person pizza -> handleAddPizzaToOrder state person pizza

handleListOrders :: State -> String -> Either String (Maybe String, State)
handleListOrders state@(State currentOrders) person =
    case filter ((== person) . fst) currentOrders of
        [] -> Right (Just $ "No orders found for " ++ person, state)
        personOrders -> Right (Just $ formatOrders person personOrders, state)

-- Helper function to format orders for a person
formatOrders :: String -> [(String, Order)] -> String
formatOrders person orders = 
    "Orders for " ++ person ++ ":\n" ++ 
    unlines (map (("  " ++) . formatOrder . snd) orders)

-- Helper function to format a single order
formatOrder :: Order -> String
formatOrder (SimpleOrder pizza details) = 
    "Simple Order: " ++ formatPizza pizza ++ " - " ++ formatDetails details
formatOrder (OrderBundle pizza1 pizza2 pizza3 details) =
    "Bundle Order:\n    " ++ formatPizza pizza1 ++ 
    "\n    " ++ formatPizza pizza2 ++ 
    "\n    " ++ formatPizza pizza3 ++ 
    "\n    " ++ formatDetails details

-- Helper function to format a pizza
formatPizza :: Pizza -> String
formatPizza (Pizza size crust toppings quantity) =
    show quantity ++ "x " ++ show size ++ " " ++ show crust ++ 
    " pizza with " ++ formatToppings toppings

-- Helper function to format toppings
formatToppings :: [Topping] -> String
formatToppings [] = "no toppings"
formatToppings toppings = unwords (map show toppings)

-- Helper function to format order details
formatDetails :: OrderDetails -> String
formatDetails (OrderDetails orderType paymentMethod) =
    show orderType ++ ", paying by " ++ show paymentMethod

handleAddPizzaToOrder :: State -> String -> Pizza -> Either String (Maybe String, State)
handleAddPizzaToOrder state@(State currentOrders) person newPizza =
    case lookup person currentOrders of
        Nothing -> Left $ "No existing order found for " ++ person
        Just existingOrder -> 
            case addPizzaToOrder existingOrder newPizza of
                Left err -> Left err
                Right updatedOrder -> 
                    Right (
                        Just $ "Added pizza to order for " ++ person,
                        State { orders = updateOrders person updatedOrder currentOrders }
                    )


handleNewOrder :: State -> [(String, Order)] -> Either String (Maybe String, State)
handleNewOrder (State currentOrders) newOrders = 
    let conflictingCustomers = filter (\(person, _) -> any ((== person) . fst) currentOrders) newOrders
    in if not (null conflictingCustomers)
        then Left $ "Orders already exist for: " ++ show (map fst conflictingCustomers)
        else Right (
            Just $ "Added orders for: " ++ show (map fst newOrders),
            State { orders = currentOrders ++ newOrders }
        )

handleRemoveOrder :: State -> String -> Order -> Either String (Maybe String, State)
handleRemoveOrder (State currentOrders) person _ =
    case lookup person currentOrders of
        Nothing -> Left $ "No order found for " ++ person
        Just _ -> 
            Right (Just $ "Order removed for " ++ person, State (filter ((/= person) . fst) currentOrders))

addPizzaToOrder :: Order -> Pizza -> Either String Order
addPizzaToOrder (SimpleOrder existingPizza details) newPizza =
    if arePizzasCompatible existingPizza newPizza
        then Right $ OrderBundle existingPizza newPizza newPizza details
        else Left "Cannot add incompatible pizza to simple order"
addPizzaToOrder (OrderBundle _ _ _ _) _ =
    Left "Cannot add more pizzas to an order bundle"

arePizzasCompatible :: Pizza -> Pizza -> Bool
arePizzasCompatible p1 p2 = size p1 == size p2 && crust p1 == crust p2

updateOrders :: String -> Order -> [(String, Order)] -> [(String, Order)]
updateOrders person newOrder = map (\(p, o) -> if p == person then (p, newOrder) else (p, o))