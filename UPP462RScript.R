## ----Goal of this project-----------------------------------------
# The goal of this project is to demonstrate how to use R to acquire, manipulate, and map spatial data.
# This exercise will run through an entire series of steps from using the census API key to pull census 
# data, as well as mapping data from outside sources, analyzing it, and presenting it.

# ***ADD STEP TO ADD WORKING DIRECTORY***

# First up will be installing packages, packages in R are ecosystems or platforms that allow for users to access
# tools, features, data philosophies and much more. We will use several packages to manipulate, map, and handle data.
# The tigris R package simplifies the process for R users of obtaining and using Census geographic datasets. 
# Functions in tigris download a requested Census geographic dataset from the US Census Bureau website, then load
# the dataset into R as a spatial object.

## ----install-packages---------------------------------------------
install.packages(c("tidycensus", "tidyverse", "plotly"))
install.packages(c("sf", "tigris", "tidyverse", "tmap", "tidycensus"))
install.packages("writexl")
install.packages("scales")
install.packages("sp")

library(sf)
library(sp)
library(tigris)
library(tidycensus)
library(tidyverse)
library(tmap)
library(dplyr)
library(writexl)
library(ggplot2)
library(scales)
library(crssuggest)

# The census API key allows for direct queries to Census dataset

# ***ADD STEPS TO OBTAIN CENSUS API KEY***

## ----api-key------------------------------------------------------
census_api_key("121819beb19ed0d9780bf2ac3fa331fc01b95bfc", install = TRUE)

# Import data from the Chicago Open Data Portal
# To do this, download the file from the portal:
# (https://data.cityofchicago.org/Education/Chicago-Public-Schools-School-Locations-SY1415/3fhj-xtn5) 
# as a CSV and make sure that it is saved in a folder that can be accessed in RStudio. 
# Once it is in a folder in the "Files" work space, locate the file, right click on it, and select import.

# Now let's prepare our data so that it is ready to map.
# First, we will save the point locations for CPS schools in Chicago to a 
# dataframe with a simpler name. Add the name "cps_schools" to the dataframe
# and run your code.
cps_schools <- CPS_School_Locations_SY1415_20241101

# Use the head() function to look at the first few rows and get familiar with our data. 
head(cps_schools)

# We can see that the X and Y coordinates of each school are in two separate columns. This is 
# what we will need to map the points. Take a look at the points plotted out by using the plot() function.
plot(cps_schools$X,cps_schools$Y)

# For the rest of this exercise we will be using data in an sf object to take advantage of the sf functionality

cps_schools_sf <- st_as_sf(cps_schools, coords = c("X", "Y"), crs = 3435)

plot(cps_schools_sf$geometry)

# Retrieving Chicago Census tracts 
# Load census tracts for Chicago
chi_tracts <- tracts(state = "IL", county = "Cook", cb = TRUE)

# Load Chicago place boundary
chicago_place <- places(state = "IL", cb = TRUE) %>%
  filter(NAME == "Chicago")

# Use st_intersection to filter tracts within Chicago
chi_tracts_chicago <- st_intersection(chi_tracts, chicago_place)

# Plot the resulting Chicago census tracts
plot(chi_tracts_chicago$geometry) 

view(chi_tracts_chicago)

## --Let's plot the two layers so that they are projected properly-----------

# The Coordinate Reference System (CRS) contains the projection and datum of our tract file,
# allowing us to match that CRS in our cps_schools point layer. The output of st_crs() will include:
# The EPSG code (if available), which is a numeric identifier for the CRS (e.g., EPSG:4326 for WGS84).
# The PROJ string or WKT (Well-Known Text) representation, which contains detailed information about the projection and datum.

st_crs(chi_tracts_chicago)

# The DATUM is NAD 1983 and the ESPG is 4269, which is also a reference to NAD 1983.
# However, we want to add a projected coordinate system, this means we will need to 
# transform our data so that they have a new CRS that aligns with what we are used to.
# The next line of code changes the CRS of each layer to the specified EPSG code (3435 for 
# Illinois State Plane East).

cps_schools <- st_transform(cps_schools, crs = 3435)
chi_tracts_chicago <- st_transform(chi_tracts_chicago, crs = 3435)

# Let's look at our data so far. In ggplot2, geom_sf() is designed to work specifically 
# with sf (simple feature) objects, which are a standardized format for spatial data in R. 
# Converting your points layer to an sf object is essential because geom_sf() expects this 
# format to interpret spatial coordinates properly and align the layers with spatial accuracy.

# Create the ggplot
ggplot() + 
  geom_sf(data = chi_tracts_chicago) +            # Plot the census shapefile
  geom_sf(data = cps_schools_sf, color = "red") +  # Overlay points, colored red
  theme_minimal()

# Perform the spatial join
# chi_tract_schools <- st_join(cps_schools, chi_tracts_chicago)

# view(chi_tract_schools)

#_________________________________________________________________________

# Perform the spatial join to associate each school point with its tract

schools_in_tracts <- st_join(cps_schools, chi_tracts_chicago, left = TRUE)

# Count the number of schools per tract

school_counts <- schools_in_tracts %>%
  group_by(GEOID) %>%            # Use GEOID directly from schools_in_tracts
  summarise(school_count = n())   # Count points in each tract

view(school_counts)

# Join the counts back to the original chi_tracts_chicago layer


chi_tracts_with_counts <- chi_tracts_chicago %>%
  left_join(st_drop_geometry(school_counts), by = "GEOID")   # Drop geometry in school_counts to avoid conflicts

view(chi_tracts_with_counts)

#___________________________________________________________________________

ggplot(data = chi_tracts_with_counts) +
  geom_sf(aes(fill = school_count)) +        # Use school_count for fill color
  scale_fill_viridis_c(option = "plasma",    # Color scale for better visualization
                       na.value = "grey90",  # Color for tracts with no schools
                       name = "School Count") +
  labs(title = "Number of Schools per Tract in Chicago",
       caption = "Data Source: CPS and Tracts") +
  theme_minimal()             

ggplot(data = chi_tracts_with_counts) +
  geom_sf(aes(fill = school_count)) +       # Use school_count for fill color
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "School Count") +
  labs(title = "Number of Schools per Tract in Chicago") +
  theme_minimal()    
