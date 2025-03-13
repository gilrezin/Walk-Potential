### Identify roads of high potential for walking amenities
### Score will be determined using two factors: Noise pollution & topography
### Noise pollution accounts for other factors such as traffic volumes and speed

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# for our bounding box, get a road's points and obtain its nodes
library(raster)
library(sf)
library(sp)
library(elevatr)
library(dplyr)
library(osmdata)
library(geosphere)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(ggspatial) 
library(rosm)
library(prettymapr)

# replace with a path to your own state's .tif file
# WA State - if we are examining any other state, this needs to be changed
noise_data <- raster("C:\\Users\\gilre\\Documents\\CptS 475\\Walk Potential\\final_project\\CONUS_road_noise_2020\\State_rasters\\WA_road_noise_2020.tif")

bbox <- getbb("Pullman, Washington, USA")
roads <- opq(bbox = bbox) %>% add_osm_feature(key = "highway") %>% osmdata_sf()
#nodes <- roads$osm_points
ways <- roads$osm_lines

# remove private roads & parking lots from the dataset
ways <- subset(ways, highway != "service")

#print(ways[1,]$geometry)
ways$grade <- NA
ways$noise <- NA
ways$grade_factor <- NA
ways$noise_factor <- NA
ways$fit <- NA

minNoise <- 40
maxNoise <- 90
minGrade <- 0
maxGrade <- 10

grade_weight <- 0.2
noise_weight <- 0.8

# for every road, find the change in elevation along its nodes via the rate of change (elevation between nodes 1 and 2 / distance between nodes 1 and 2)
coords_list <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(coords_list) <- c("x", "y", "L1") # if this doesn't run, the elevation code won't either

# prepare our coordinate list for processing in single query form
for (i in 1:nrow(ways))
{
  coords <- st_coordinates(ways[i,]$geometry)[c(1, nrow(st_coordinates(ways[i,]$geometry))), ]
  df_linestring <- as.data.frame(coords)
  df_linestring[1,3] <- i
  df_linestring[2,3] <- i
  coords_list <- rbind(df_linestring, coords_list)
}

colnames(coords_list) <- c("x", "y", "L1")
# process our query
elevation_list <- get_elev_point(coords_list, prj = 4326, src = "epqs")

# individually obtain any missing elevation data
for (i in 1:nrow(elevation_list))
{
 repeat
  {
    if (any(is.na(elevation_list[i,]$elevation)))
    {
      elevation_list[i,] <- get_elev_point(elevation_list[i,], prj = 4326, src = "epqs", overwrite=TRUE)
    }
    else
    {
      break
    }
  }
}


# then average these rates of change to obtain the steepness of the road
for (i in 1:nrow(ways))
{
  print(i)
  # obtain our elevation info from elevation_list and place into a data frame
  
  line <- elevation_list[elevation_list$L1 == i,]
  
  # get the grade of the road from the first and last node distances & change in elevation
  distance = distHaversine(as.vector(line$geometry[[1]]), as.vector(line$geometry[[length(line$geometry)]]))
  elevation_change = ((line$elevation[length(line$geometry)]-line$elevation[1]) / distance) * 100
  
  ways[i,]$grade = abs(elevation_change)
  
  
  # obtain the average transportation noise pollution along the road
  start_point <- spTransform(SpatialPoints(cbind(line$geometry[[1]][1], line$geometry[[1]][2]), proj4string = CRS("+proj=longlat +datum=NAD83")), crs(noise_data))
  end_point <- spTransform(SpatialPoints(cbind(line$geometry[[2]][1], line$geometry[[2]][2]), proj4string = CRS("+proj=longlat +datum=NAD83")), crs(noise_data))
  
  start_noise_value = extract(noise_data, start_point)
  end_noise_value = extract(noise_data, end_point)
  
  # default value if quieter than 45db
  if (is.na(start_noise_value))
  {
    start_noise_value <- 40
  }
  
  if (is.na(end_noise_value))
  {
    end_noise_value <- 40
  }
  
  ways[i,]$noise = mean(start_noise_value, end_noise_value)
  
  
  # create a relative scale (0-1) for these factors
  
  ways[i,]$grade_factor <- 1 - ((ways[i,]$grade - minGrade) / (maxGrade - minGrade))
  ways[i,]$noise_factor <- 1 - ((ways[i,]$noise - minNoise) / (maxNoise - minNoise))
  
  ways[i,]$fit <- grade_weight * ways[i,]$grade_factor + noise_weight * ways[i,]$noise_factor
}

# form a decision tree
fit_tree <- rpart(fit ~ grade + noise, data = ways, method = "anova")
rpart.plot(fit_tree)


# plot our map denoting differing colors for increasing score
ways$fit_category <- cut(ways$fit, 
                         breaks = c(0.8, 0.85, 0.9, 0.95, 1), 
                         labels = c("Low", "Moderate", "Good", "High"), 
                         include.lowest = TRUE)
ggplot(data = ways) + 
  annotation_map_tile(zoom = 15, type = "cartolight") +
  geom_sf(aes(color = fit_category, size = 1.5)) + 
  scale_color_manual(values = c("Low" = "orange1", "Moderate" = "yellow2", 
                                "Good" = "green2", "High" = "green4"))


### Recalculate weights if desired
for (i in 1:nrow(ways))
{
  ways[i,]$grade_factor <- 1 - ((ways[i,]$grade - minGrade) / (maxGrade - minGrade))
  ways[i,]$noise_factor <- 1 - ((ways[i,]$noise - minNoise) / (maxNoise - minNoise))
  
  ways[i,]$fit <- grade_weight * ways[i,]$grade_factor + noise_weight * ways[i,]$noise_factor
}
