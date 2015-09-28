#!/usr/bin/python
from PyABike import *

cab = PyABike()
lis=''
for i in range(0,30):
   #lis+= str(cab.listCompletedTrips(i*20,20, '','','Number', 'Password'))
   lis+= str(cab.listCompletedTrips(i*20,20, '','','0179141...', 'Your login Password'))
   print i

f= open('out.txt', 'w')
f.write(lis)
f.close()


##gleim 50.1315150,8.6939040
#print PyABike.buildGeoPos(cab, 8.4040, 49.0095)

#PyABike.buildGeoPos(cab, 8.6935357,50.1313682)

#bikes = PyABike.listFreeBikes(cab, 100, 1000,8.4040, 49.0095)

#print bikes
#print "other stuff"
#PyABike.l

#freeBikes =  PyABike.listFreeBikes(cab, 2, 100)
#returnLocations = PyABike.listReturnLocations(cab, '5708', 2, 100)
#bikeInfo =  PyABike.getBikeInfo(cab, '5708')

#trips = PyABike.listCompletedTrips()


#print freeBikes
#print returnLocations
#print bikeInfo