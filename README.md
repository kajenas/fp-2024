# Pizza ordering system

## Overview

This is a functional programming project designed to mimic a pizza ordering system

## Main BNF structure

```markdown

<remove_order> ::= "Remove\n" <order>
<new_order> ::= "New order\n" <order>
<add_pizza_to_order> ::= "Add pizza\n" <pizza> <order>

<order> ::= <simple_order> | <complex_order>

<simple_order> ::= <pizza_order> <order_details>

<complex_order> ::= "Sub-order:\n" <order> <order>

<pizza_order> ::= <pizza> | <pizza> <pizza_order>

<pizza> ::= "Pizza:\n" <size> <crust> <toppings> <quantity>

<size> ::=  ("small" | "medium" | "large") "\n"

<crust> ::=  ("thin" | "thick" | "stuffed") "\n"

<toppings> ::= <topping_list>

<topping_list> ::= <topping> | <topping> "," <topping_list>

<topping> ::= "pepperoni" | "mushrooms" | "onions" | "sausage" | "bacon" | "extra cheese" | "black olives" | "green peppers" | "pineapple" "\n"

<quantity> ::= <integer>

<integer> ::= [1-9]+

<order_details> ::= <order_type> <payment_method> <confirmation>

<order_type> ::= ("Delivery" | "Pickup") "\n"

<payment_method> ::= ("Credit Card" | "Cash" | "Mobile Payment") "\n"

<confirmation> ::= ("Confirm" | "Cancel")

```

### Commands

* `new_order` - makes an order

Example:
```
Pizza
Large
Thin
2
Pepperoni, Extra Cheese
Delivery
Cash
Confirm

```
* `remove_order` - removes an order
    
    Example:
    ```
    Remove
    Pizza
    large
    thick
    4
    mushrooms
    delivery
    Credit Card
    Confirm

    ```
* `add_pizza_to_order` - adds pizza to order