# This is an example of calling the functions of RCPTEC package
#
# EM FASE DE TESTES

loadRda("OCIS")

getWeatherData(lon = -50.88, lat = -28.51, iTime = '2017080700', fTime = '2017080710')
getWeatherData(lon = -50.88, lat = -28.51, iTime = '2017080700')
teste_weather <- getWeatherData(lon = -50.88, lat = -28.51,
                                iTime = '2017110400', fTime = '2017110423',
                                weatherData = 'OCIS')

#getWeatherData(-50.88,-28.51, 'OCIS')
#getWeatherData(-50.88,-28.51, 'PREC')
#getWeatherData(-50.88,-28.51, 'UR2M')
#getWeatherData(-50.88,-28.51, 'V10M')

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
