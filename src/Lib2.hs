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

-- | An entity which represents user input.
data Query = RemoveOrder String Order
            | NewOrder [(String, Order)]
            | AddPizzaToOrder String Pizza
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


-- <remove_order> ::= <person> "Confirm"
parseRemoveOrder :: [String] -> Either String Query
parseRemoveOrder lines =
  case parsePerson lines of
    Right (person, ["Confirm"]) -> Right $ RemoveOrder person (SimpleOrder (Pizza Small Thin [] 1) (OrderDetails Pickup Cash))  -- Placeholder order, not used anymore
    _ -> Left "Remove order must include a person and end with 'Confirm'"

parseQuery :: String -> Either String Query
parseQuery input = case words input of
    "New":"order":rest -> parseNewOrder rest
    ["Remove"] -> Left "Expected person name after 'Remove'"
    "Remove":rest -> parseRemoveOrder rest
    "Add":"pizza":rest -> parseAddPizzaToOrder rest
    _ -> Left "Invalid command. Must start with 'Remove', 'New order', or 'Add pizza'"

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

 -- <person> ::= ([a-z] | [A-Z])+
parsePerson :: [String] -> Either String (String, [String])
parsePerson [] = Left "Expected person name"
parsePerson (name:rest)
  | all isAlpha name = Right (name, rest)
  | otherwise = Left "Person name must contain only letters"


-- <pizza> ::= "Pizza:\n" <size> <crust> <toppings> <quantity>
parsePizza :: [String] -> Either String (Pizza, [String])
parsePizza ("Pizza:":rest) =  -- Ensure "Pizza:" is correctly followed by the details
  case parseSize rest of
    Right (size, rest1) ->
      case parseCrust rest1 of
        Right (crust, rest2) ->
          case parseToppings rest2 [] of
            Right (toppings, rest3) ->
              case parseQuantity rest3 of
                Right (quantity, rest4) -> Right (Pizza size crust toppings quantity, rest4)
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err
parsePizza (x:_) = Left $ "Expected 'Pizza:', but got: " ++ x
parsePizza [] = Left "Expected 'Pizza:', but got empty input"


-- <order> ::= <simple_order> | <order_bundle>
parseOrder :: [String] -> Either String (Order, [String])
parseOrder ("Order":"bundle":rest) =  -- Correctly handle the "Order bundle" keyword
  case parsePizza rest of
    Right (pizza1, rest1) ->
      case parsePizza rest1 of
        Right (pizza2, rest2) ->
          case parsePizza rest2 of
            Right (pizza3, rest3) ->  -- Expect three pizzas
              case parseOrderDetails rest3 of
                Right (details, rest4) -> Right (OrderBundle pizza1 pizza2 pizza3 details, rest4)
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err
parseOrder lines =
  case parsePizza lines of
    Right (pizza, rest1) ->
      case parseOrderDetails rest1 of
        Right (details, rest2) -> Right (SimpleOrder pizza details, rest2)
        Left err -> Left err
    Left err -> Left err

-- <size> ::=  ("small" | "medium" | "large")
parseSize :: [String] -> Either String (Size, [String])
parseSize (size:rest) = case size of
  "small" -> Right (Small, rest)
  "medium" -> Right (Medium, rest)
  "large" -> Right (Large, rest)
  _ -> Left "Invalid size"
parseSize [] = Left "Expected size"
 
 -- <crust> ::=  ("thin" | "thick" | "stuffed")
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

-- <quantity> ::=  ([0-9])+
parseQuantity :: [String] -> Either String (Int, [String])
parseQuantity (q:rest) = case reads q of
  [(n, "")] | n > 0 -> Right (n, rest)
  _ -> Left "Invalid quantity"
parseQuantity [] = Left "Expected quantity"

-- <order_details> ::= <order_type> <payment_method>
parseOrderDetails :: [String] -> Either String (OrderDetails, [String])
parseOrderDetails lines =
  case parseOrderType lines of
    Right (orderType, rest1) ->
      case parsePaymentMethod rest1 of
        Right (paymentMethod, rest2) -> Right (OrderDetails orderType paymentMethod, rest2)
        Left err -> Left err
    Left err -> Left err

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
    RemoveOrder person order -> handleRemoveOrder state person order
    NewOrder newOrders -> handleNewOrder state newOrders
    AddPizzaToOrder person pizza -> handleAddPizzaToOrder state person pizza

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