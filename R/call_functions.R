# This is an example of calling the functions of RCPTEC package
#
# EM FASE DE TESTES

# Tests for getWeatherData() function
getWeatherData(lon = -50.88, lat = -28.51, iTime = '2017110400', fTime = '2017110423')
getWeatherData(lon = -50.88, lat = -28.51, iTime = '2017110400')
getWeatherData(-50.88,-28.51, iTime = '2017110400', fTime = '2017110423')

getWeatherData(lon = -50.88, lat = -28.51, weatherData = 'OCIS', iTime = '2017110400', fTime = '2017110423')
getWeatherData(lon = -50.88, lat = -28.51, 'OCIS', iTime = '2017110400')

getWeatherData(-50.88,-28.51, 'OCIS')

getWeatherData(-50.88,-28.51)


# Test for info.RCPTEC.weather() function
info.RCPTEC.weather()

# Tests for getClimateData() function
getClimateData('1', 'YEARLY','TP2M', '-12', '-49', iYear = 2006, fYear = 2010)
getClimateData('1', 'YEARLY','TP2M', '-12', '-49', 1, 2006, 12, 2010)
getClimateData('1', 'MONTHLY','TP2M', '-12', '-49', 1, 2006, 12, 2006)
getClimateData('1', 'DAILY','TP2M', '-12', '-49', 1, 2006, 01, 2006)
getClimateData('1', 'HOURLY','TP2M', '-12', '-49', 1, 2006, 01, 2006)

# Test for info.RCPTEC.climate() function
info.RCPTEC.climate()

# Tests for plots functions
plot.RCPTEC.var(-50.88,-28.51, 'UR2M', "2017110400", "2017110623")
plot.RCPTEC.var(-50.88,-28.51, 'PREC', "2017110400", "2017110700")

plot.RCPTEC.meteogram(-50.88,-28.51)
plot.RCPTEC.THP(-50.88,-28.51)
