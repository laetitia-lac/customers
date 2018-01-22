# INTERCOM : Home Test

## Task

There is some customer records in a text file (customers.json) with one customer per line, JSON-encoded.
We want to invite any customer within 100km of our Dublin office for some food and drinks on us.
Write a program that will read the full list of customers and output the names and user ids of matching customers (within 100km), sorted by User ID (ascending).
The code must include automated tests.

## Provided Resources

Module `CustomersParsing` provides a datatype `Customer` to represent a Customer.
There is also the functions `getJSON` and `convertJSONtoCustomers`, that parses the JSON file data.

Module `Distance` provides a way (function `haversine`) to compute the distance between two points specified by latitude and longitude coordinates in degrees.

Module `ListGuests` provides a way to determine which customers are invited thanks to the function `customersInvited`.

## Running the executable

It may be necessary to install a few libraries. ex : `stack install aeson` or `stack setup`
See https://docs.haskellstack.org/en/stable/README/ for more details.

Build the exectuable:  `stack build` 

Run the executable: `stack exec customers`

## Running Tests

Run the tests : `stack test`

## Data

The provided `gistfile1.txt` has been modified in `gistfile1_modified.txt`, such as the customers are represented in an array.
Indeed, `gistfile1.txt` had multiple roots, and it made the parsing difficult.