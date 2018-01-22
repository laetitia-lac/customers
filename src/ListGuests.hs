module ListGuests where
import Data.List (sortBy)
import Data.Ord (comparing)
import CustomersParsing
import Distance

intercomOffice :: (Double, Double)
intercomOffice = (53.339428, -6.257664)

-- define if a specific Customer is invited or not
inviteCustomer :: Customer -> Bool
inviteCustomer c = let dist = distance intercomOffice (lat c, lon c)
                   in dist <= 1e5 --100km=100 000m

-- from a list of customers and a list of current guests, obtain the final array of guests
customersInvited :: [Customer] -> [Customer] -> [Customer]
customersInvited (c:allCustomers) guests = let invitation = inviteCustomer c
                                           in case invitation of
                                               True -> customersInvited allCustomers (c:guests)
                                               False -> customersInvited allCustomers guests
customersInvited [] guests = sortBy (comparing customer_id) guests  

-- functions to display correctly the information
displayCustomer :: Customer -> String
displayCustomer c = "\t- " ++ (nam c) ++ " with id = " ++ show (customer_id c) ++ " and coordinates (latitude:" ++ show (lat c) ++ ", longitude:" ++ show (lon c) ++ ")\n"

displayGuests :: [Customer] -> String -> String
displayGuests (c:guests) response = let response' = response ++ (displayCustomer c)
                                    in displayGuests guests response'
displayGuests [] response = response
--