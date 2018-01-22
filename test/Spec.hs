{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Char
import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import CustomersParsing
import Distance
import ListGuests

-- examples of customers expressed in CustomerJSON
margaret_JSON = CustomerJSON "52.940000" 3 "Margaret Mrs" "-6.601450"
null_JSON = CustomerJSON "0.0" 0 "" "0.0"

-- examples of customers expressed in Customer
ian = Customer 53.339428 2 "Ian McIop" (-6.257664) --is in the intercom office
margaret = Customer 52.940000 3 "Margaret Mrs" (-6.601450) --is 50km away from the office
christina = Customer 54.138284 12 "Christina Aze" (-6.954994) --is 100km away from the office
alice = Customer 54.62415 1 "Alice Qsdfg" (-6.954994) --is 150km away from the intercom office
marc = Customer 54.62425 5 "Marc Tyu" (-6.954794) --is 150km away from the intercom office
null_customer = Customer 0.0 0 "" 0.0

{- Parsing - Tests -}
--CHECKED
-- from a filepath, get the content of the JSON data file under the ByteString format --

one_customer_byte_string = "\"{\\\"latitude\\\": \\\"54.138284\\\", \\\"user_id\\\": 12, \\\"name\\\": \\\"Christina Aze\\\", \\\"longitude\\\": \\\"-6.954994\\\"}\""
multiple_customers_byte_string = "\"[{\\\"latitude\\\": \\\"54.138284\\\", \\\"user_id\\\": 12, \\\"name\\\": \\\"Christina Aze\\\", \\\"longitude\\\": \\\"-6.954994\\\"},\\n{\\\"latitude\\\": \\\"54.62415\\\", \\\"user_id\\\": 1, \\\"name\\\": \\\"Alice Qsdfg\\\", \\\"longitude\\\": \\\"-6.954994\\\"},\\n{\\\"latitude\\\": \\\"53.339428\\\", \\\"user_id\\\": 2, \\\"name\\\": \\\"Ian McIop\\\", \\\"longitude\\\": \\\"-6.257664\\\"}]\""
empty_file_byte_string = "\"\""
empty_array_byte_string = "\"[]\""

test_get_one_customer  = show (getJSON "data/test/one_customer.txt") @?= one_customer_byte_string
test_get_multiple_customers  = show (getJSON "data/test/multiple_customers.txt") @?= multiple_customers_byte_string
test_get_empty_file  = show (getJSON "data/test/empty_file.txt") @?= empty_file_byte_string
test_get_empty_array  = show (getJSON "data/test/empty_array.txt") @?= empty_array_byte_string

test_get_content_from_file_path
 = testGroup "\nGet content of a JSON data file from a filepath"
      [ testCase "Get one customer" $ test_get_one_customer,
        testCase "Get multiple customers" $ test_get_multiple_customers, 
        testCase "Get an empty file" $ test_get_empty_file,
        testCase "Get an empty array" $ test_get_empty_array
      ]

-- convert Customer under the JSON Data format to Customer --
--CHECKED
test_convert_to_customer_JSON_to_customer_margaret  = convertCustomer margaret_JSON @?= margaret
test_convert_to_customer_JSON_to_customer_null = convertCustomer null_JSON @?= null_customer

test_convert_to_customer_JSON_to_customer
 = testGroup "\nConvert from Customer JSON format to Customer"
      [ testCase "Convert a lambda customer" $ test_convert_to_customer_JSON_to_customer_margaret,
       testCase "Convert an empty customer" $ test_convert_to_customer_JSON_to_customer_null
      ]

-- from a ByteString, obtain an array of Customer -- 
--CHECKED
test_convert_content_file_to_one_customer  = convertJSONtoCustomers (getJSON "data/test/one_customer.txt") @?= [christina]
test_convert_content_file_to_multiple_customers  = convertJSONtoCustomers (getJSON "data/test/multiple_customers.txt") @?= [christina, alice, ian]
test_convert_content_file_to_empty_file  = convertJSONtoCustomers (getJSON "data/test/empty_file.txt") @?= []
test_convert_content_file_to_empty_array  = convertJSONtoCustomers (getJSON "data/test/empty_array.txt") @?= []

test_convert_content_file_to_customer
 = testGroup "\nConvert the content of the file to an array of Customer"
      [ testCase "Convert one customer" $ test_convert_content_file_to_one_customer,
        testCase "Convert multiple customers" $ test_convert_content_file_to_multiple_customers, 
        testCase "Convert an empty file" $ test_convert_content_file_to_empty_file,
        testCase "Convert an empty array" $ test_convert_content_file_to_empty_array
      ]

{- Compute distance - Tests -}

-- check the validity of the subfunction of haversine --
--CHECKED
test_compute_subHaversine_function_theta_zero  = subHaversine 0.0 @?= 0.0
test_compute_subHaversine_function_theta_pi  = subHaversine(pi) @?= 1.0

test_compute_subHaversine
 = testGroup "\nCheck the results for the subfunction of haversine function"
      [ testCase "Theta = 0" $ test_compute_subHaversine_function_theta_zero,
        testCase "Theta = pi" $ test_compute_subHaversine_function_theta_pi
      ]

-- check the validity of the function of haversine --
--UNCHECKED
test_compute_haversine_function_same_point  = haversine (0.56, 0.78) (0.56, 0.78) @?= 0.0
test_compute_haversine_function_distance_from_book_of_kells  = haversine (0.9309703,-0.1094174) (0.9309486,-0.1092168) @?= 1.2172107499994261e-4
test_compute_haversine_function_distance_from_galway  = haversine (0.9299811,-0.1591528) (0.9309486,-0.1092168) @?= 2.984851003618343e-2
test_compute_haversine_function_distance_from_new_york  = haversine (0.7103083,-1.2960793) (0.9309486,-0.1092168) @?= 0.8056539275652534

test_compute_haversine
 = testGroup "\nCheck the results for the haversine function : distance between two points -- expressed in radians"
      [ testCase "Distance between the same point" $ test_compute_haversine_function_same_point,
        testCase "Distance between Book of Kells and Intercom" $ test_compute_haversine_function_distance_from_book_of_kells,
        testCase "Distance between Galway and Intercom" $ test_compute_haversine_function_distance_from_galway, 
        testCase "Distance between New York and Intercom" $ test_compute_haversine_function_distance_from_new_york
      ]

-- convert point expressed in degrees to a point expressed in radians --
--CHECKED
test_convert_degrees_to_radius_0  = convertPoint (0, 0) @?= (0,0)
test_convert_degrees_to_radius_180  = convertPoint (180, 180) @?= (pi,pi)
test_convert_degrees_to_radius_360  = convertPoint (360, 360) @?= (2*pi,2*pi)
test_convert_degrees_to_radius_90_and_45  = convertPoint (90, 45) @?= (pi/2,pi/4)

test_convert_degrees_to_radius
 = testGroup "\nConvert a position - from degrees to radians"
      [ testCase "Convert a point expressed as (0°, 0°)" $ test_convert_degrees_to_radius_0,
        testCase "Convert a point expressed as (180°, 180°)" $ test_convert_degrees_to_radius_180,
        testCase "Convert a point expressed as (360°, 360°)" $ test_convert_degrees_to_radius_360,
        testCase "Convert a point expressed as (90°, 45°)" $ test_convert_degrees_to_radius_90_and_45
      ]

-- check the distance (in meters) between two points (expressed in degrees) --
--CHECKED
test_compute_distance_same_point  = distance intercomOffice intercomOffice @?= 0.0
test_compute_distance_from_book_of_kells  = distance (53.3406731,-6.2691553) intercomOffice @?= 775.3743624032215
test_compute_distance_from_galway  = distance intercomOffice (53.2839936,-9.1187865) @?= 190164.88173844173
test_compute_distance_from_new_york  = distance intercomOffice (40.6976701,-74.2598751) @?= 5132820.882336593

test_compute_distance_expressed_in_meters
 = testGroup "\nCheck the distance (in meters) between two points (expressed in degrees)"
      [ testCase "A null distance (same point)" $ test_compute_distance_same_point,
        testCase "Distance between Book of Kells and Intercom" $ test_compute_distance_from_book_of_kells, 
        testCase "Distance between Galway and Intercom" $ test_compute_distance_from_galway,
        testCase "Distance between New York and Intercom" $ test_compute_distance_from_new_york
      ]

{- Customers at 100km from the office - sort by id -}

-- check if customer is invited (less than 100 km from the office)--
--CHECKED
test_invite_this_customer_in_the_office = inviteCustomer ian @?= True
test_invite_this_customer_at_50_km_from_the_office = inviteCustomer margaret @?= True
test_invite_this_customer_at_100_km_from_the_office = inviteCustomer christina @?= True
test_invite_this_customer_at_150_km_from_the_office = inviteCustomer alice @?= False

test_invite_this_customer
 = testGroup "\nCheck if a customer is invited (less than 100km from the office)"
      [ testCase "A customer who is in the office" $ test_invite_this_customer_in_the_office,
        testCase "A customer who is 50km away from the office" $ test_invite_this_customer_at_50_km_from_the_office,
        testCase "A customer who is 100km away from the office" $ test_invite_this_customer_at_100_km_from_the_office,
        testCase "A customer who is 150km away from the office" $ test_invite_this_customer_at_150_km_from_the_office
      ]

--checks the list of guests sorted by id --
--CHECKED
test_list_of_invited_customers_with_one_customer_in_initial_list_one_guest = customersInvited [margaret] [] @?= [margaret]
test_list_of_invited_customers_with_one_customer_in_initial_list_none_guest = customersInvited [alice] [] @?= []
test_list_of_invited_customers_with_multiple_customers_in_initial_list_all_guests = customersInvited [ian,margaret] [] @?= [ian,margaret]
test_list_of_invited_customers_with_multiple_customers_in_initial_list_none_guest = customersInvited [alice,marc] [] @?= []
test_list_of_invited_customers_with_multiple_customers_in_initial_list_a_few_guests = customersInvited [alice,margaret] [] @?= [margaret]
test_list_of_invited_customers_with_multiple_customers_in_initial_list_no_sorted_all_guests = customersInvited [margaret,ian] [] @?= [ian,margaret]
test_list_of_invited_customers_with_multiple_customers_in_initial_list_no_sorted_none_guest = customersInvited [marc,alice] [] @?= []
test_list_of_invited_customers_with_multiple_customers_in_initial_list_no_sorted_a_few_guests = customersInvited [marc,ian] [] @?= [ian]

test_list_of_invited_customers
 = testGroup "\nCheck the list of invited customers"
      [ testCase "Initial list : one customer - is a guest" $ test_list_of_invited_customers_with_one_customer_in_initial_list_one_guest,
        testCase "Initial list : one customer - is not a guest" $ test_list_of_invited_customers_with_one_customer_in_initial_list_none_guest,
        testCase "Initial list : sorted list - multiple customers - all guests" $ test_list_of_invited_customers_with_multiple_customers_in_initial_list_all_guests,
        testCase "Initial list : sorted list - multiple customers - none guest"  $ test_list_of_invited_customers_with_multiple_customers_in_initial_list_none_guest,
        testCase "Initial list : sorted list - multiple customers - a few guests"  $ test_list_of_invited_customers_with_multiple_customers_in_initial_list_a_few_guests,
        testCase "Initial list : unsorted list - multiple customers - all guests"  $ test_list_of_invited_customers_with_multiple_customers_in_initial_list_no_sorted_all_guests,
        testCase "Initial list : unsorted list - multiple customers - none guest"  $ test_list_of_invited_customers_with_multiple_customers_in_initial_list_no_sorted_none_guest, 
        testCase "Initial list : unsorted list - multiple customers - a few guests"  $ test_list_of_invited_customers_with_multiple_customers_in_initial_list_no_sorted_a_few_guests
      ]

{- Test Main ---------------------- -}

main = defaultMain tests

tests :: [TF.Test]
tests
  = [ test_get_content_from_file_path,
      test_convert_to_customer_JSON_to_customer, 
      test_convert_content_file_to_customer, 
      test_compute_subHaversine, 
      test_compute_haversine, 
      test_convert_degrees_to_radius, 
      test_compute_distance_expressed_in_meters,
      test_invite_this_customer,
      test_list_of_invited_customers
    ]
