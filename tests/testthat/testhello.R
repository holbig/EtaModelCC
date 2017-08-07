#load_all("RCPTEC")
#test("RCPTEC") function

library(testthat)
test_check("RCPTEC")

hello()
getTemperatura(lon = -75.05, lat = -35.05)
