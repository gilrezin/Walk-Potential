# Walk-Potential
A tool used to identify corridors in US cities with high walking potential, measuring both topography and seperation from car traffic.

This project uses data from OpenStreetMap, the Bureau of Transportation Statistics, and the United States Geological Survey to give a "walking potential score" to every street in a given geographic location.

# What is Walking Potential? #
This project came about when I wanted to find out the best places for my college town to invest in walking and biking infrastructure. 
My criteria for walking potential consists of two factors:
- Topography (steeper streets tire out walkers more than topographically flat streets)
- Road noise pollution (this is a aggregate factor, encompassing all the negatives for walkers associated with proximity to vehicular traffic, including noise but also air pollution, safety, etc.)

In theory, this data would display the optimal places for pedestrians to be, regardless of current infrastructure. From here, a city could construct a network of pedestrian-priority streets, in which the infrastructure prioritizes the movement of people walking instead of motor traffic.

## Methodology & Limits ##
This project aggregates the road noise score and the road grade into a single score for every road and path in a geographic area. 

To obtain the road grade, the start and end points are obtained, of which the elevation is obtained for both of them. From here, a slope formula is used to calculate the average road grade. The primary flaw with this method is that any changes in road grade between those points are not accounted for. For example, if a road were to start and end at the same elevation, yet the midpoint was 100 feet higher, the grade of the road would still be 0%.

To apply across a range of cities, data on road noise traffic was supplied via the Road Noise Map from the Bureau of Transportation Statistics, which generalizes average road noise from traffic volumes and road speed limits. As a result, *it does not account for factors such as sound barriers or tunnels.* This can result in lowered scores for trails and streets adjacent to busy roads, regardless of the sound barriers betweeen them.

Long-term plans for this project include updates to the methodology to account for these limitations.

# How to Run #
To use this project, download the walkingpotential.R file listed above.
While the OpenStreetMap and Geological Survey data are both accessed via API, the Bureau of Transportation Data requires that you [download the full road noise dataset.](https://www.bts.gov/bts-net-storage/CONUS_road_noise_2020.zip)

Once downloaded, you will need to modify the .R file to point towards the state's .tif file that you wish to use. 
*EX: getting data from Los Angeles, California would require pointing towards the **CA_road_noise.tif** file.*

You will also need to modify the bbox to whichever city you want to collect data from. The file default is for "Pullman, Washington, USA".

Once running, the script takes anywhere from 10-30 minutes to complete, depending on how large of a geographic area you requested.
