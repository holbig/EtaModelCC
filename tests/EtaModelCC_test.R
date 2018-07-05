library(EtaModelCC)

# Test for getInfoClimate() function
getInfoClimate()

# Tests for getClimateData() function
getClimateData('1', 'YEARLY', 'TP2M', '-28.35', '-52.35', iYear = 2006, fYear = 2010)
getClimateData('1', 'YEARLY', 'TP2M', '-28.35', '-52.35', 1, 2006, 12, 2010)
getClimateData('1', 'MONTHLY','TP2M', '-28.35', '-52.35', 1, 2006, 12, 2006)
getClimateData('1', 'DAILY',  'TP2M', '-28.35', '-52.35', 1, 2006, 01, 2006)
getClimateData('1', 'HOURLY', 'TP2M', '-28.35', '-52.35', 1, 2006, 01, 2006)
