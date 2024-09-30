# Pizza ordering system

## Overview

This is a functional programming project designed to mimic a pizza ordering system

## Main BNF structure

```markdown


<order> ::= <simple_order> | <nested_order>

<simple_order> ::= <pizza_order> <order_details>

<nested_order> ::= "Sub-order:" <order> <order>

<pizza_order> ::= <pizza> | <pizza> <pizza_order>

<pizza> ::= "Pizza" <size> <crust> <toppings> <quantity>

<size> ::= "small" | "medium" | "large"

<crust> ::= "thin" | "thick" | "stuffed"

<toppings> ::= "Toppings:" <topping_list>

<topping_list> ::= <topping> | <topping> "," <topping_list>

<topping> ::= "pepperoni" | "mushrooms" | "onions" | "sausage" | "bacon" | "extra cheese" | "black olives" | "green peppers" | "pineapple"

<quantity> ::= "Quantity:" <integer>

<integer> ::= [1-9][0-9]*

<order_details> ::= <order_type> <payment_method> <confirmation>

<order_type> ::= "Delivery" | "Pickup"

<payment_method> ::= "Credit Card" | "Cash" | "Mobile Payment"

<confirmation> ::= "Confirm order?" ("yes" | "no")

```

### Commands

* `add_pizza_to_order` - adds a pizza to order

Example:
```
ADD
SIZE: Large
CRUST: Thin
QUANTITY: 2
TOPPINGS: Pepperoni, Extra Cheese

```
* `update_pizza_quantity` - updates the quantity of a specific pizza

Example:
```
UPDATE
PIZZA SIZE: Medium
CRUST: Thick
NEW QUANTITY: 3

```
* `remove_pizza_from_order` - removes a specific pizza from order
    
    Example:
    ```
    REMOVE
    PIZZA SIZE: Large
    CRUST: Stuffed

    ```
* `change_payment_method` - changes the payment method of a specific order
* `cancel_order` - cancels specific order