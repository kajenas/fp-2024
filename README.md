# Pizza ordering system

## Overview

This is a functional programming project designed to mimic a pizza ordering system

## Main BNF structure

```markdown


<order> ::= <pizza> | <pizza> <more_pizzas>

<pizza> ::= "Pizza" <size> <crust> <toppings> <quantity>

<more_pizzas> ::= "Add another pizza?" ("yes" <pizza> | "no")

<size> ::= "small" | "medium" | "large"

<crust> ::= "thin" | "thick" | "stuffed"

<toppings> ::= "Toppings:" <topping_list>

<topping_list> ::= <topping> | <topping> <more_toppings>

<topping> ::= "pepperoni" | "mushrooms" | "onions" | "sausage" | "bacon" | "extra cheese" | "black olives" | "green peppers" | "pineapple"

<more_toppings> ::= "," <topping_list> | "done"

<quantity> ::= "Quantity:" <integer>

<integer> ::= "1" | "2" | "3" | "4" | "5" | ...

<order_type> ::= "Delivery" | "Pickup"

<payment_method> ::= "Credit Card" | "Cash" | "Mobile Payment"

<confirmation> ::= "Confirm order?" ("yes" | "no")


```

### Commands

* `add_pizza_to_order` - adds a pizza to order

Example:
```
ADD
ORDER ID: 1234
SIZE: Large
CRUST: Thin
QUANTITY: 2
TOPPINGS: Pepperoni, Extra Cheese

```
* `update_pizza_quantity` - updates the quantity of a specific pizza

Example:
```
UPDATE
ORDER ID: 1234
PIZZA SIZE: Medium
CRUST: Thick
NEW QUANTITY: 3

```
* `remove_pizza_from_order` - removes a specific pizza from order
    
    Example:
    ```
    REMOVE
    ORDER ID: 1234
    PIZZA SIZE: Large
    CRUST: Stuffed

    ```
* `change_payment_method` - changes the payment method of a specific order
* `cancel_order` - cancels specific order