import os
import csv
import sys
import time
import geopy
from geopy import geocoders
from scipy import stats
from math import floor
from getpass import getuser

####################
# Set wd
if getuser()=="janus829": path='/Users/janus829/Dropbox/Research/Rhodium/Data/CityPopData/Combined'

os.chdir(path)
####################

####################
# Load Data
print "Loading data...\n"

cities=[]
with open('CityPopData.csv', 'rU') as f:
    reader = csv.reader(f)
    next(reader, None)
    for row in reader:
    	cityCntry=row[3] + ', ' + row[1]
        cities.append(cityCntry)

print "		...Data Loaded\n"
####################

####################
# Remove Duplicate Values (No need to look up dupl city-cntry combos)
cities=list(set(cities))
####################

####################
# Look up lat and long
print "Looking up locations...\n"

g=geocoders.GoogleV3()
latlong=[]
counter=0
pVals=stats.scoreatpercentile(range(1,len(cities)), range(10,100,10))
pVals=[int(floor(i)) for i in pVals]

for cty in cities:
	try:
		ll=g.geocode(cty, exactly_one=False)[0][1]
	except TypeError:
		ll=("Not Found","Not Found")
		print cty + " Not Found"
	except geopy.exc.GeocoderQueryError: 
		ll=("Weird String","Weird String")
		print cty + " Weird String"
	except geopy.exc.GeocoderServiceError:
		print "Need time to rest"
		time.sleep(30)
		try:
			ll=g.geocode(cty, exactly_one=False)[0][1]
		except geopy.exc.GeocoderServiceError:
			print "Still resting"
			time.sleep(60)
			ll=g.geocode(cty, exactly_one=False)[0][1]
	counter+=1
	latlong.append(ll)
	if counter in pVals: print "{0:.0%}".format(float(counter)/len(cities)) + " Complete"

print "		...Locations looked up\n"
####################

####################
# Writing
print "Writing to file...\n"

with open('CityLatLong.csv', 'wb') as f:
	writer = csv.DictWriter(f, 
		fieldnames=("Country", "City","Lat", "Long"))
	writer.writeheader()
	for i in range(1, len(cities)):
		city=cities[i].split(',')[0].strip()
		cntry=cities[i].split(',')[1].strip()
		lat=latlong[i][0]
		lon=latlong[i][1]
		writer.writerow({"Country":cntry,"City":city,"Lat":lat,"Long":lon})

print "		...Finished writing data\n"
####################

####################
print "COMPLETE\n"
####################