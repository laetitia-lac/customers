module Distance where

sqr n = n * n

-- subfunction for the haversine function
subHaversine :: Double -> Double
subHaversine theta = sqr (sin (theta / 2))

-- function to compute the distance (in radians) between points (latitude, longitude in radians)
-- 1st point -- 2nd point (radians) -- great circle distance (radians)
haversine ::  (Double, Double) ->  (Double, Double) ->  Double            
haversine (phi1,lambda1) (phi2,lambda2)
  = 2 * asin (sqrt hbody)
  where hbody = subHaversine (abs (phi2-phi1)) + cos phi1 * cos phi2 * subHaversine (abs (lambda2-lambda1))

--earth radius (in metres)
earthRadius :: Double
earthRadius = 6371e3

degreesToRadians :: Double -> Double
degreesToRadians d = pi * d / 180

-- convert a point expressed by latitude and longitude from degrees to radians
convertPoint :: (Double, Double) -> (Double, Double)
convertPoint (lat,lon) = (degreesToRadians lat, degreesToRadians lon)

-- compute the distance (in meters) between points (latitude, longitude in degrees)
-- 1st point -- 2nd point expressed in latitude, longitude (degrees) -- great circle distance (m)
distance :: (Double, Double) -> (Double, Double) -> Double
distance pt1 pt2
 = earthRadius * haversine (convertPoint pt1) (convertPoint pt2)
