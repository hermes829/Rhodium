import os
import csv
import sys
import time
import geopy
from geopy import geocoders
from getpass import getuser

if getuser()=="janus829": path='/Users/janus829/Dropbox/Research/Rhodium/Data/CityPopData/Combined'

os.chdir(path)

cities=[]
with open('Capitals.csv', 'rU') as f:
    reader = csv.reader(f)
    next(reader, None)
    for row in reader:
    	cityCntry=row[1] + ', ' + row[0]
        cities.append(cityCntry)

g=geocoders.GoogleV3()
latlong=[]
for cty in cities:
	ll=g.geocode(cty, exactly_one=False)[0][1]
	latlong.append(ll)
	time.sleep(3)

# Writing
with open('CapitalsLatLong.csv', 'wb') as f:
	writer = csv.DictWriter(f, fieldnames=("City", "Country", "Lat", "Long"))
	writer.writeheader()
	for i in range(1, len(cities)):
		city=cities[i].split(',')[0].strip()
		cntry=cities[i].split(',')[1].strip()
		lat=latlong[i][0]
		lon=latlong[i][1]
		writer.writerow({"City":city, "Country":cntry, "Lat":lat, "Long":lon})