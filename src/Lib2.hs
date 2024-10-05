{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}



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


-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = RemoveOrder String Order
            | NewOrder [(String, Order)]
            | AddPizzaToOrder String Pizza

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


-- | The instances are needed basically for tests


parseQuery :: String -> Either String Query
parseQuery input = case lines input of
  ("Remove":rest) -> parseRemoveOrder rest
  ("New order":rest) -> parseNewOrder rest
  ("Add pizza":rest) -> parseAddPizzaToOrder rest
  _ -> Left "Invalid command. Must start with 'Remove', 'New order', or 'Add pizza'"


-- <remove_order> ::= "Remove\n" <person_order> <confirmation>
parseRemoveOrder :: [String] -> Either String Query
parseRemoveOrder lines = do
  (person, remainingLines1) <- parsePerson lines
  (order, remainingLines2) <- parseOrder remainingLines1
  case remainingLines2 of
    ["Confirm"] -> Right $ RemoveOrder person order
    _ -> Left "Remove order must end with 'Confirm'"

-- <new_order> ::= "New order\n" <multiple_person_orders> <confirmation>
parseNewOrder :: [String] -> Either String Query
parseNewOrder lines = do
  (orders, remainingLines) <- parseMultiplePersonOrders lines []
  case remainingLines of
    ["Confirm"] -> Right $ NewOrder orders
    _ -> Left "New order must end with 'Confirm'"

-- <add_pizza_to_order> ::= "Add pizza\n" <person> <pizza> <confirmation>
parseAddPizzaToOrder :: [String] -> Either String Query
parseAddPizzaToOrder lines = do
  (person, remainingLines1) <- parsePerson lines
  (pizza, remainingLines2) <- parsePizza remainingLines1
  case remainingLines2 of
    ["Confirm"] -> Right $ AddPizzaToOrder person pizza
    _ -> Left "Add pizza must end with 'Confirm'"

-- <multiple_person_orders> ::= <person_order> | <person_order> <multiple_person_orders>
parseMultiplePersonOrders :: [String] -> [(String, Order)] -> Either String ([(String, Order)], [String])
parseMultiplePersonOrders [] accum = Right (reverse accum, [])
parseMultiplePersonOrders lines accum = do
  (person, remainingLines1) <- parsePerson lines
  (order, remainingLines2) <- parseOrder remainingLines1
  if null remainingLines2 || head remainingLines2 == "Confirm"
    then Right (reverse ((person, order) : accum), remainingLines2)
    else parseMultiplePersonOrders remainingLines2 ((person, order) : accum)

-- <person> ::= ([a-z]|[A-Z])+ "\n"
parsePerson :: [String] -> Either String (String, [String])
parsePerson [] = Left "Expected person name"
parsePerson (name:rest)
  | all isAlpha name = Right (name, rest)
  | otherwise = Left "Person name must contain only letters"

-- <pizza> ::= "Pizza:\n" <size> <crust> <toppings> <quantity>
parsePizza :: [String] -> Either String (Pizza, [String])
parsePizza ("Pizza:":rest) = do
  (size, rest1) <- parseSize rest
  (crust, rest2) <- parseCrust rest1
  (toppings, rest3) <- parseToppings rest2 []
  (quantity, rest4) <- parseQuantity rest3
  Right (Pizza size crust toppings quantity, rest4)
parsePizza _ = Left "Pizza must start with 'Pizza:'"

-- <order> ::= <simple_order> | <order_bundle>
parseOrder :: [String] -> Either String (Order, [String])
parseOrder ("Order bundle":rest) = do
  (pizza1, rest1) <- parsePizza rest
  (pizza2, rest2) <- parsePizza rest1
  (pizza3, rest3) <- parsePizza rest2
  (details, rest4) <- parseOrderDetails rest3
  Right (OrderBundle pizza1 pizza2 pizza3 details, rest4)
parseOrder lines = do
  (pizza, rest1) <- parsePizza lines
  (details, rest2) <- parseOrderDetails rest1
  Right (SimpleOrder pizza details, rest2)

-- <size> ::= "small" | "medium" | "large"
parseSize :: [String] -> Either String (Size, [String])
parseSize (size:rest) = case size of
  "small" -> Right (Small, rest)
  "medium" -> Right (Medium, rest)
  "large" -> Right (Large, rest)
  _ -> Left "Invalid size"
parseSize [] = Left "Expected size"

-- <crust> ::= "thin" | "thick" | "stuffed"
parseCrust :: [String] -> Either String (Crust, [String])
parseCrust (crust:rest) = case crust of
  "thin" -> Right (Thin, rest)
  "thick" -> Right (Thick, rest)
  "stuffed" -> Right (Stuffed, rest)
  _ -> Left "Invalid crust"
parseCrust [] = Left "Expected crust"

-- <toppings> ::= <topping_list>
parseToppings :: [String] -> [Topping] -> Either String ([Topping], [String])
parseToppings [] accum = Left "Expected toppings"
parseToppings (topping:rest) accum = case topping of
  "pepperoni" -> addTopping Pepperoni
  "mushrooms" -> addTopping Mushrooms
  "onions" -> addTopping Onions
  "sausage" -> addTopping Sausage
  "bacon" -> addTopping Bacon
  "extra cheese" -> addTopping ExtraCheese
  _ | all isDigit topping -> Right (reverse accum, topping:rest)
    | otherwise -> Left "Invalid topping"
  where
    addTopping t = parseToppings rest (t:accum)

-- <quantity> ::= <integer> "\n"
parseQuantity :: [String] -> Either String (Int, [String])
parseQuantity (q:rest) = case reads q of
  [(n, "")] | n > 0 -> Right (n, rest)
  _ -> Left "Invalid quantity"
parseQuantity [] = Left "Expected quantity"

-- <order_details> ::= <order_type> <payment_method>
parseOrderDetails :: [String] -> Either String (OrderDetails, [String])
parseOrderDetails lines = do
  (orderType, rest1) <- parseOrderType lines
  (paymentMethod, rest2) <- parsePaymentMethod rest1
  Right (OrderDetails orderType paymentMethod, rest2)

-- <order_type> ::= "Delivery" | "Pickup"
parseOrderType :: [String] -> Either String (OrderType, [String])
parseOrderType (ot:rest) = case ot of
  "Delivery" -> Right (Delivery, rest)
  "Pickup" -> Right (Pickup, rest)
  _ -> Left "Invalid order type"
parseOrderType [] = Left "Expected order type"

-- <payment_method> ::= "Credit Card" | "Cash" | "Mobile Payment"
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

instance Eq Query where
    (RemoveOrder p1 o1) == (RemoveOrder p2 o2) = p1 == p2 && o1 == o2
    (NewOrder orders1) == (NewOrder orders2) = orders1 == orders2
    (AddPizzaToOrder p1 pizza1) == (AddPizzaToOrder p2 pizza2) = p1 == p2 && pizza1 == pizza2
    _ == _ = False

instance Show Query where
    show (RemoveOrder person order) = 
        "RemoveOrder " ++ show person ++ " " ++ show order
    show (NewOrder orders) = 
        "NewOrder " ++ show orders
    show (AddPizzaToOrder person pizza) = 
        "AddPizzaToOrder " ++ show person ++ " " ++ show pizza

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
    RemoveOrder person order -> handleRemoveOrder state person order
    NewOrder orders -> handleNewOrder state orders
    AddPizzaToOrder person pizza -> handleAddPizzaToOrder state person pizza

handleRemoveOrder :: State -> String -> Order -> Either String (Maybe String, State)
handleRemoveOrder state@(State currentOrders) person order =
    case lookup person currentOrders of
        Nothing -> Left $ "No order found for " ++ person
        Just existingOrder ->
            if existingOrder == order
                then Right (
                    Just $ "Order removed for " ++ person,
                    State { orders = filter ((/= person) . fst) currentOrders }
                )
                else Left $ "Order mismatch for " ++ person

handleNewOrder :: State -> [(String, Order)] -> Either String (Maybe String, State)
handleNewOrder (State currentOrders) newOrders = 
    let conflictingCustomers = filter (\(person, _) -> any ((== person) . fst) currentOrders) newOrders
    in if not (null conflictingCustomers)
        then Left $ "Orders already exist for: " ++ show (map fst conflictingCustomers)
        else Right (
            Just $ "Added orders for: " ++ show (map fst newOrders),
            State { orders = currentOrders ++ newOrders }
        )

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