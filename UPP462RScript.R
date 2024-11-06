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

# We need to start by making sure that our data is ready to be read. the sf package is where 
# we derive many of the functions for geospatial analysis. Because of this, we have to make sure 
# any features we are using for our mapping is an sf object. Run the code in the next line containing 
# the "st_as_sf" function to change the cps_schools layer to an sf object.
cps_schools <- st_as_sf(cps_schools, coords = c("X", "Y"), crs = 4326)

# You may have noticed the "crs =" at the ned of the last line. CRS, or Coordinate Reference System, 
# contains the projection and datum of a layer to map. When we map multiple layers, they need the same CRS.
# The output of st_crs() in the below code will include: The EPSG code (if available), which is a numeric 
# identifier for the CRS (e.g., EPSG:4326 for WGS84), as well as the datum, among other information.

st_crs(chi_tracts_chicago)

# The DATUM is NAD 1983 and the ESPG is 4269, which is also a reference to NAD 1983.
# However, we want to add a projected coordinate system, this means we will need to 
# transform our data so that there is a new CRS that is appropriate for Chicago.
# The next line of code changes the CRS of each layer to the specified EPSG code Enter 
# "3435" after "crs =" to add the projection Illinois State Plane East to the two layers).

cps_schools <- st_transform(cps_schools, crs = 3435)
chi_tracts_chicago <- st_transform(chi_tracts_chicago, crs = 3435)

# Let's look at our data so far. 

# In the code below, ggplot is used from the ggplot2 package. Geom_sf is used specifically 
# for visualizing spatial data by looking to the layers' "geometry" field.
ggplot() + 
  geom_sf(data = chi_tracts_chicago) +            # Plot the census shapefile
  geom_sf(data = cps_schools, color = "red") +  # Overlay points, colored red
  theme_minimal()

#________Spatial Join_________________________________________________________________

# The purpose of this analysis is to identify census tracts with higher numbers of CPS schools and
# visualize them. To accomplish this we want to associate the census tracts with any point 
# representing CPS schools that intersect with them.In ArcGIS pro, this would be done with a spatial join.
# We can perform the same operation in R.

# To start off, we want to use st_join() to associate each school point with the tract that intersects it. 
# Below, we are creating a new dataframe that. A left join is when all features from the 
# first argument are retained, even if they do not have a spatial relationship with the features 
# in the second argument.Save this operation to a new datafram called "schools_in_tracts"

schools_in_tracts <- st_join(cps_schools, chi_tracts_chicago, left = TRUE)

# Count the number of schools per tract
# This following code groups our new dataframe, schools_in_tracts, by the GEOID column, which uniquely 
# identifies each census tract. The code summarise(school_count = n()) then counts the number of schools 
# within each tract and stores this count in a new column called school_count. The resulting school_counts object 
# is a data frame with two columns: GEOID and school_count, representing the number of schools in each tract.

school_counts <- schools_in_tracts %>%
  group_by(GEOID) %>%            # Use GEOID directly from schools_in_tracts
  summarise(school_count = n())   # Count points in each tract

# Run the below code 
view(school_counts)
# What is the range of the number of schools that you see?

# Next, the goal is to append the school_count information back to the original
# chi_tracts_chicago layer, so each tract polygon now has a count of schools. Sf 
# objects can only have one active geometry field at a time, so
# st_drop_geometry(school_counts) removes the geometry from school_counts, leaving 
# only the attribute data (GEOID and school_count) to prevent conflicts when joining.
# left_join() then merges this attribute data back into chi_tracts_chicago by matching 
# rows based on the GEOID column. The result, chi_tracts_with_counts, reflects the 
# original tract layer  with the school_count field added.

chi_tracts_with_counts <- chi_tracts_chicago %>%
  left_join(st_drop_geometry(school_counts), by = "GEOID")  

# Take a look at the new dataframe with the school count field added.
view(chi_tracts_with_counts)

#____Create Map for Count of Schools by Tract______________________________________________________________

# Now that we have a field for schools counts by tract, we can turn it into a choropleth 
# map to demonstrate which tracts have the most schools. To do this, we will go back to ggplot.
# There are a couple more features that we will make use of to create a complete map. 
# the "aes" function dictates appearance, "scale_fill_gradient" specifies that appearance, 
# and "labs" allows for labeling. In the following code chunk, give the "low" argument a value 
# of "lightblue", and give the "high" argument a value of "darkblue". This will give our tracts 
# a gradient of colors based on the school_count value.

ggplot(data = chi_tracts_with_counts) +
  geom_sf(aes(fill = school_count)) +       # Use school_count for fill color
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "School Count") +
  labs(title = "Number of Schools per Tract in Chicago") +
  theme_minimal()    
