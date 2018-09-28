library(EtaModelCC)

# Test for getInfoClimate() function
getInfoClimate()

# Tests for getClimateData() function

# getClimateData(modelID, modelFrequency, modelVar, lat, lon, iMonth = 1, iYear, fMonth = 1, fYear)
getClimateData('1', 'YEARLY', 'TP2M', '-28.35', '-52.34', iYear = 2006, fYear = 2010)
getClimateData('1', 'YEARLY', 'TP2M', '-28.35', '-52.35', 2006, 2010)
getClimateData('1', 'MONTHLY','TP2M', '-28.35', '-52.35', 2006, 2008)
getClimateData('1', 'DAILY',  'TP2M', '-28.35', '-52.35', 2006, 2006)
getClimateData('1', 'HOURLY', 'TP2M', '-28.35', '-52.35', 2006, 2006)

getClimateData('1', 'HOURLY', 'TP2M', '-28.2612', '-52.4083', 2018, 2018)


getClimateData('4', 'YEARLY', 'TP2M', '-23.5', '-46.6', 2018, 2048)
getClimateData('5', 'DAILY', 'TP2M', '-23.5', '-46.6', 2018, 2018)

getClimateDataBR('1', 'YEARLY', 'TP2M', iYear = 2006, fYear = 2006)

