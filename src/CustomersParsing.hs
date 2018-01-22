{-# LANGUAGE DeriveGeneric #-}

module CustomersParsing where
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import System.IO.Unsafe

-- data used in JSON file
data CustomerJSON = CustomerJSON  { latitude :: String
                                  , user_id :: Int
                                  , name :: String
                                  , longitude :: String
                                  } deriving (Show,Generic)

-- data used to perform easily computations
data Customer = Customer  { lat :: Double
                          , customer_id :: Int
                          , nam :: String
                          , lon :: Double
                          } deriving (Ord, Eq, Show)

convertCustomer :: CustomerJSON -> Customer
convertCustomer c = Customer (read (latitude c)) (user_id c) (name c) (read (longitude c))

-- allow us to define CustomerJSON as the data in the JSON file
instance FromJSON CustomerJSON
instance ToJSON CustomerJSON

-- JSON File that we are going to analyse
jsonFile :: FilePath
jsonFile = "data/gistfile1_modified.txt"

-- get the content of the JSON file
getJSON :: FilePath -> B.ByteString
getJSON file = unsafePerformIO $ (B.readFile file)

-- from the content of a JSON file, obtain an array of Customer
convertJSONtoCustomers :: B.ByteString -> [Customer]
convertJSONtoCustomers dataJSON = let result = (eitherDecode dataJSON) :: (Either String [CustomerJSON])
                                  in case result of
                                       Left err -> let singleResult = (eitherDecode dataJSON) :: (Either String CustomerJSON)
                                                   in case singleResult of
                                                         Left err -> [] -- impossible to decrypt the JSON data as an array of customers or an unique customer
                                                         Right customer -> [convertCustomer customer] -- case where the JSON data contains only one customer 
                                       Right customers -> map convertCustomer customers -- case where the JSON data is an array of customers