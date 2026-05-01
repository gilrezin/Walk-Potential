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
  #start_point <- spTransform(SpatialPoints(cbind(line$geometry[[1]][1], line$geometry[[1]][2]), proj4string = CRS("+proj=longlat +datum=NAD83")), crs(noise_data))
  #end_point <- spTransform(SpatialPoints(cbind(line$geometry[[2]][1], line$geometry[[2]][2]), proj4string = CRS("+proj=longlat +datum=NAD83")), crs(noise_data))
  
  #start_noise_value = extract(noise_data, start_point)
  #end_noise_value = extract(noise_data, end_point)
  
  total_noise_value = 0
  for (j in 1:nrow(ways[i,]$geometry[[1]]))
  {
    point <- spTransform(SpatialPoints(cbind(ways[i,]$geometry[[1]][j, 1], ways[i,]$geometry[[1]][j, 2]), proj4string = CRS("+proj=longlat +datum=NAD83")), crs(noise_data))
    
    if (is.na(extract(noise_data, point)))
    {
      total_noise_value = total_noise_value + 40
    }
    else
    {
      total_noise_value = total_noise_value + extract(noise_data, point) 
    }
  }
  average_noise_value = total_noise_value / nrow(ways[i,]$geometry[[1]])
  
  #if (is.na(average_noise_value))
  #{
  #  start_noise_value <- 40
  #}
  
  # default value if quieter than 45db
  #if (is.na(start_noise_value))
  #{
  #  start_noise_value <- 40
  #}
  
  #if (is.na(end_noise_value))
  #{
  #  end_noise_value <- 40
  #}
  
  #ways[i,]$noise = mean(start_noise_value, end_noise_value)
  ways[i,]$noise = average_noise_value
  
  
  # create a relative scale (0-1) for these factors
  
  ways[i,]$grade_factor <- 1 - ((ways[i,]$grade - minGrade) / (maxGrade - minGrade))
  ways[i,]$noise_factor <- 1 - ((ways[i,]$noise - minNoise) / (maxNoise - minNoise))
  
  ways[i,]$fit <- grade_weight * ways[i,]$grade_factor + noise_weight * ways[i,]$noise_factor
}

# form a decision tree
fit_tree <- rpart(fit ~ grade + noise, data = ways, method = "anova")
rpart.plot(fit_tree)


# plot our map denoting differing colors for increasing score (default=cartolight)
ways$fit_category <- cut(ways$fit, 
                         breaks = c(-Inf, 0.8, 0.85, 0.9, 0.95, 1), 
                         labels = c("Very Low", "Low", "Moderate", "Good", "High"), 
                         include.lowest = TRUE)
ggplot(data = ways) + 
  annotation_map_tile(zoom = 15, type = "osm") +
  geom_sf(aes(color = fit_category), size = 1.5) + 
  scale_color_manual(values = c("Very Low" = "red", "Low" = "orange1", "Moderate" = "yellow2", 
                                "Good" = "green2", "High" = "green4"))


### Recalculate weights if desired
#for (i in 1:nrow(ways))
#{
#  ways[i,]$grade_factor <- 1 - ((ways[i,]$grade - minGrade) / (maxGrade - minGrade))
#  ways[i,]$noise_factor <- 1 - ((ways[i,]$noise - minNoise) / (maxNoise - minNoise))
#  
#  ways[i,]$fit <- grade_weight * ways[i,]$grade_factor + noise_weight * ways[i,]$noise_factor
#}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### =============== WEIGHTED NAVIGATIONAL SECTION ===============
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(igraph)
library(leaflet)

ways <- readRDS("C:\\Users\\gilre\\Documents\\CptS 475\\Walk Potential\\pullman.rds")

ways <- ways %>%
  filter(!name %in% c("Northeast Airport Drive", "Kitzmiller Road", "Old Moscow Road", "Southeast Johnson Road", "Brayton Road",
                      "Orville Boyd Road", "Pullman-Albion Road", "Northwest Albion Drive", "Northwest Cottonwood Drive",
                      "Whelan Road", "Wexler Road", "Osprey Lane", "Eagle Lane", "Warren Road", "Kestrel Lane", "J Bar S Road",
                      "Chukkar Road", "Pullman Airport Road", "State Route 270", "Bill Chipman Palouse Trail", "Johnson Road", 
                      "Country Club Road", "State Route 195", "Armstrong Road", "Sunshine Road", "Northeast Airport Drive"))

# extract list-column of coordinates for each feature
coords_list <- st_geometry(ways)

edge_list <- lapply(coords_list, function(line) {
  coords <- st_coordinates(line)
  # create pairwise edges
  data.frame(
    x1 = coords[-nrow(coords), "X"],
    y1 = coords[-nrow(coords), "Y"],
    x2 = coords[-1, "X"],
    y2 = coords[-1, "Y"]
  )
})

edge_df <- do.call(rbind, edge_list)

# Create a unique ID for each coordinate
nodes <- edge_df %>%
  distinct(x1, y1) %>%
  mutate(node_id = row_number()) %>%
  relocate(node_id)

# helper to match coords → node id
match_node <- function(x, y) {
  nodes %>% filter(x1 == x, y1 == y) %>% pull(node_id)
}

edge_df$from <- mapply(match_node, edge_df$x1, edge_df$y1)
edge_df$to   <- mapply(match_node, edge_df$x2, edge_df$y2)
edge_df$weight_fit <- 1 - ways[sub("\\..*", "", as.numeric(rownames(edge_df))),]$fit
#edge_df$weight <- edge_df$weight ^ 0.7
edge_df$geo_dist <- sqrt((edge_df$x2 - edge_df$x1)^2 + (edge_df$y2 - edge_df$y1)^2)
edge_df$weight <- edge_df$geo_dist * edge_df$weight_fit

edge_df_filtered <- edge_df[edge_df$to != 0,]
edge_df_filtered <- edge_df_filtered[!is.na(edge_df_filtered$x1),]

city_graph <- graph_from_data_frame(
  d = edge_df_filtered[, c("from", "to", "weight")],
  vertices = nodes,
  directed = FALSE
)

# visually test for a valid path
path <- shortest_paths(city_graph, from = 10, to = 92, weights = E(city_graph)$weight)
path$vpath[[1]]

path_vertices <- path$vpath[[1]] |> as_ids()
route_coordinates <- nodes[nodes$node_id %in% path_vertices, c("x1", "y1")]
nodes_sf <- st_as_sf(nodes, coords = c("x1", "y1"), crs = 4326)
route_nodes <- nodes_sf[nodes_sf$node_id %in% path_vertices, ]
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = route_nodes,
    radius = 4,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.8
  )
# - END

ways$times_used <- 0

for (i in 1:1000) {
  from_node <- sample(1:gsize(city_graph), 1)
  to_node <- sample(1:gsize(city_graph), 1)
  cat(i,". ",from_node, " -> ", to_node, "\t\r", sep="")
  path <- tryCatch({
    shortest_paths(city_graph, from = from_node, to = to_node, weights = E(city_graph)$weight)
  }, error = function(e) {
    return(NULL)
  })
  if (is.null(path) || length(path$vpath[[1]]) == 0) next
  
  for (node_used in path$vpath[[1]]) {
    ids_to_increment <- row.names(edge_df_filtered)[edge_df_filtered$to == node_used]
    for (ids in ids_to_increment) {
      ids_subscript <- sub("\\..*", "", ids)
      ways[ids_subscript,]$times_used <- ways[ids_subscript,]$times_used + 1
    }
  }
}

ways$times_used_log <- apply(X = ways, MARGIN = 1, FUN = function(ways) {if (ways$times_used != 0) { return(log(ways$times_used)) } else return(0)})



# fairly certain this breaks ways
# ways <- ways %>%
#   filter(!highway %in% "trunk")
  
# Create color palette from grey to red based on times_used
pal <- colorNumeric(
  palette = c("grey", "red1", "red2", "red3", "black"),
  domain = ways$times_used
)

# Create leaflet map
leaflet(data = ways) %>%
  addTiles() %>%  # Add OpenStreetMap tiles
  addPolylines(
    color = ~pal(times_used),
    weight = 1.5,
    opacity = 1
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~times_used,
    title = "Times Used",
    opacity = 1
  )

# Create a greyscale color palette for logarithmic scale
pal <- colorNumeric(
  palette = c("white", "black"),
  domain = ways$times_used_log
)

# Create leaflet map
leaflet(data = ways) %>%
  addTiles() %>%  # Add OpenStreetMap tiles
  addPolylines(
    color = ~pal(times_used_log),
    weight = 1.5,
    opacity = 1
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~times_used_log,
    title = "Times Used Logarithmic",
    opacity = 1
  )

saveRDS(object = ways, file = "pullman_walkcorridor_final.rds")
