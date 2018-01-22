module Main where
import System.Directory

import CustomersParsing
import Distance
import ListGuests

version = "1.0.0"
main
 = do putStrLn ("\n\tInvitations of customers : v"++version++"\n")
      let allCustomers = convertJSONtoCustomers (getJSON jsonFile)
      putStrLn (displayGuests (customersInvited allCustomers []) "\n\tWe are going to invite the following customers :\n" )
