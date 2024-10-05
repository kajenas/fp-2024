# Pizza ordering system

## Overview

This is a functional programming project designed to mimic a pizza ordering system

## Main BNF structure

```markdown

<Remove_order> ::= "Remove\n" <person_order> <confirmation>
<New_order> ::= "New order\n" <multiple_person_orders> <confirmation>
<multiple_person_orders> ::= <person_order> | <person_order> <multiple_person_orders>
<person_order> ::= <person> <order>
<Add_pizza_to_order> ::= "Add pizza\n" <person> <pizza> <confirmation>


<person> ::= ([a-z] | [A-Z])+ "\n"
<order> ::= <simple_order> | <order_bundle>
<simple_order> ::= <pizza> <order_details>
<order_bundle> ::= "Order bundle\n" (<pizza> <pizza> <pizza>) <order_details>

<pizza> ::= "Pizza:\n" <size> <crust> <toppings> <quantity>
<size> ::=  ("small" | "medium" | "large") "\n"
<crust> ::=  ("thin" | "thick" | "stuffed") "\n"
<toppings> ::= <topping_list>
<topping_list> ::= <topping> | <topping> <topping_list>
<topping> ::= ("pepperoni" | "mushrooms" | "onions" | "sausage" | "bacon" | "extra cheese") "\n"

<quantity> ::= <integer> "\n"
<integer> ::= [1-9]+
<order_details> ::= <order_type> <payment_method>
<order_type> ::= ("Delivery" | "Pickup") "\n"
<payment_method> ::= ("Credit Card" | "Cash" | "Mobile Payment") "\n"

<confirmation> ::= ("Confirm") "\n"
```

### Commands

* `new_order` - makes an order

Example:
```
New order
Kajus
Pizza:
large
stuffed
pepperoni
7
Pickup
Credit Card
Jonas
Order bundle
Pizza:
large
thick
pineapple
pepperoni
11
Pizza:
large
thin
black olives
bacon
3
Pizza:
small
stuffed
extra cheese
pineapple
4
Delivery
Credit Card
Tomas
Pizza:
medium
thick
pineapple
7
Delivery
Mobile Payment
Confirm


```
* `remove_order` - removes an order
    
```
Remove
Kajus
Pizza:
large
stuffed
pepperoni
7
Pickup
Credit Card
Confirm
```

* `add_pizza_to_order` - adds pizza to order
```
Add pizza
Kajus
Pizza:
small thin mushrooms
2
Pickup
Credit Card
Confirm
```