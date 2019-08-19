# Atlan Geospacial data in R free course - https://atlan.com/courses/introduction-to-gis-r/lesson2-manipulating-geospatial-data/

# Setting Directory right ----
getwd() 
setwd("/Users/Alexandre Mergulh?o/Documents/R/GIS course - Geospacial in R")
# needs / instead of \ and to begin w an /

# 1st LESSON ----

# installing packages ----
#everything is installed to C:/Users/Alexandre Mergulh?o/Documents/R/win-library/3.6

# first we need Rtools (download from url and installed by .exe manually) and devtools
install.packages("devtools")
library(devtools)
find_rtools() # TRUE = everything is installed properly ;)

install.packages("tidyverse")
# install.packages("Rtools") # error:
# its not a package but a seperate software! installed via web .exe and installed in C:\

install.packages("sf")
# Chapter 2 de https://geocompr.robinlovelace.net/spatial-class.html tem tudo sobre sf
# its the successor of sp package. geospatial and attribute data can be stored together in a
# spatial dataframe , where the object's geometry occupies a special list-column. In
# addition to being faster, this lets you manipulate an sf object via magrittr pipes like
# an ordinary dataframe
install.packages("raster")
install.packages("spData")
devtools::install_github("Nowosad/spDataLarge")
  install.packages('spDataLarge', repos='https://nowosad.github.io/drat/',
                 type='source')

install.packages("GADMTools") # has all packages needed for geospacial analysis ;)

library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data


# importing map ----
# All sf functions begin with st_* to help users identify them.
library(sf)
my_sf <- st_read("viz_india-master/india_states_2014/india_states.shp") # imports dataset
# works bcs directory set above and used / instead of \ !

            # importing GADM India MAP -> ERROR
            library(GADMTools)
            india_wrapper <- gadm.loadCountries("IND", level = 1, basefile = "./") 
            # check your directory for a file "IND_adm1.rds" after running this command
            # download spatial data directly from the GADM website, but using the GADMTools
              # package helps ensure your workflow is reproducible. Specifying level = 1 returns state-level boundaries
            india_wrapper <- st_read("gadm36_IND_1_sf.rds") # also doesnt work
            # ERROR : this is still not done -> downloaded manually

# Inspecting Objects ----
my_spdf <- as(my_sf, "Spatial") # to work first run this with sf:: bfr as(...) and then take it out and run again
# converts the sf object to a SpatialPolygonsDataFrame, an S4 class defined by the sp package
class(my_spdf)

str(my_spdf, max.level = 2) # like summurize, gives a basic output of an object
# We can extract any of these 5 slots using the @ symbol like we'd normally do with the $ symbol.
library(tidyverse)
glimpse(my_spdf@data)
# data and aspects of the object’s geometry are held in separate slots. 
# This is not compatible with the tidyverse-style workflow

# While we could continue working with this format, 
# let’s convert it back to an sf object with the st_as_sf() function and inspect the difference.
ind_sf <- sf::st_as_sf(my_spdf)
class(ind_sf) # now its a data.frame as bfr in my_sf

head(ind_sf, 3) # just first 3 rows of each collumn. 
# there are 9 variables but only 8 attributes/fields
# multipolygon represents areas, could be points, lines or their “multi-” counterparts
# bbox gives the object’s bounding box dimensions.
# epsg and proj4string describe the coordinate reference system (CRS). Note that this is a geographic CRS (measured in longitude and latitude) as opposed to a projected CRS
# geometry attribute/collumn is the state's geometry in a list
glimpse(ind_sf)

# Manipulating sf Objects ----
# since ind_sf is a normal data.frame using sf package => we can use normal manipulation commands like dplyr
uts <- c("Delhi", "Andaman & Nicobar Islands", "Puducherry", 
         "Lakshadweep", "Dadra & Nagar Haveli", "Daman & Diu",
         "Chandigarh")
# select these values of union territories

ind_sf <- ind_sf %>% 
  select(name, abbr) %>%
  mutate(
    type = ifelse(name %in% uts, "Union Territory", "State")
  ) %>% 
  rename(abb = abbr, state_ut = name)
# This doesn't explicitly select the geometry column, but the geometry in sf objects is sticky. 
# It remains in the object unless explicitly dropped with ind_sf %>% st_set_geometry(NULL).



# Preparing Attribute Data 
# data import = googlesheets package, web scraping = rvest, and wrangling = dplyr

# refer to prepare_data.R script in Github to prepare attributes_df (copy pasted below):

# primeiro bring required packages:
install.packages("googlesheets") 
install.packages("rvest")
install.packages("rmapshaper")
  library(googlesheets)
  library(rvest)
  library(rmapshaper)

# github for attributes_df ----
# paste data from Wikipedia into a google sheet
# population data: https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population
# gdp data: https://en.wikipedia.org/wiki/List_of_Indian_states_and_union_territories_by_GDP
   
india_sheets <- gs_title('india_states') 
# directed to google website for authorization for this "app" which gave me a key that it entered in console
# had to manually create a googlesheet named india_states 
# and then the worksheets manually too
# finally manually download the wiki data in to them
pop <- india_sheets %>% gs_read(ws = 'population') # this is just to extract from the manually created sheet
gdp <- india_sheets %>% gs_read(ws = 'gdp') # both need var names in 1st row


# wrangle population data
# create a new object tidy_pop which o tratamento da pop data.frame para tidy limpinha e pronta ;)
tidy_pop <- pop %>%
  select(-1) %>% 
  slice(2:37) %>% 
  rename_at(vars(names(.)), ~ c("state_ut", "pop_2011", 
                                "decadal_growth", "rural_pop", 
                                "urban_pop", "area_km2", "density_km2", 
                                "sex_ratio")) %>%
  mutate(
    pop_2011 = sub("\n.*", "", pop_2011) %>% 
      str_replace_all(",", "") %>% 
      as.numeric(),
    decadal_growth = as.numeric(decadal_growth),
    rural_pop = sub("\n.*", "", rural_pop) %>% 
      str_replace_all(",", "") %>% 
      as.numeric(),
    urban_pop = sub("\n.*", "", urban_pop) %>% 
      str_replace_all(",", "") %>% 
      as.numeric(),
    area_km2 = sub(" .*", "", area_km2) %>% 
      str_replace_all(",", "") %>%
      as.numeric(),
    density_km2 = sub("/.*", "", density_km2) %>% 
      str_replace_all(",", "") %>%
      as.numeric(),
    state_ut = replace(state_ut, state_ut == 'Manipurβ', 'Manipur')
  ) %>% 
  arrange(state_ut)

# wrangle gdp data
# com gdp não funcionou pq a tabela wiki mudou...
tidy_gdp <- gdp %>% 
  select(-2) %>% 
  slice(3:nrow(.)) %>% 
  rename_at(vars(names(.)), ~ c("state_ut", "nominal_gdp_inr", 
                                "nominal_gdp_usd", "data_year", 
                                "comparable_economy")) %>% 
  separate(nominal_gdp_usd, into = c('usd_value', 'usd_unit'), sep = " ") %>%
  separate(nominal_gdp_inr, into = c('inr_value', 'inr_unit'), sep = " ") %>%
  mutate(
    usd_value = sub('.*\\$', '', usd_value) %>% 
      as.numeric(),
    inr_value = sub('.*\\₹', '', inr_value) %>% 
      str_replace(",", "") %>% 
      as.numeric(),
    nominal_gdp_usd = ifelse(usd_unit == "billion", usd_value * 1e9,
                             usd_value * 1e6),
    nominal_gdp_inr = ifelse(inr_unit == "lakh", inr_value * 1e12,
                             inr_value * 1e7),
    data_year = sub("( |\\[).*", "", data_year)
  ) %>%
  select(-c(2:5)) %>% 
  arrange(state_ut)

# wrangle region data
region_url <- "https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_area"
region_scrape <- read_html(region_url) %>% 
  html_nodes("td") %>% 
  html_text(trim = TRUE)

regions_mat <- matrix(region_scrape[4:length(region_scrape)], 
                      ncol = 7, byrow = TRUE)
# this matrix is the wiki table of region 

tidy_region <- data.frame(regions_mat) %>% 
  select(2, 4) %>%
  slice(1:36) %>% 
  rename_at(vars(names(.)), ~ c("state_ut", "region")) %>% 
  mutate_if(is.factor, as.character) %>% 
  arrange(state_ut)
# vai buscar so a 2a e 4a col dessa matrix, todas as rows, renames and mutates then 

# join attribute data together
attributes_df <- tidy_gdp %>% 
  left_join(tidy_pop) %>% 
  left_join(tidy_region) %>% 
  mutate(
    state_ut = replace(state_ut, state_ut == "Andaman and Nicobar Islands", 
                       "Andaman & Nicobar Islands"),
    state_ut = replace(state_ut, state_ut == "Daman and Diu", 
                       "Daman & Diu"),
    state_ut = replace(state_ut, state_ut == "Dadra and Nagar Haveli", 
                       "Dadra & Nagar Haveli"),
    state_ut = replace(state_ut, state_ut == "Jammu and Kashmir", 
                       "Jammu & Kashmir")
  )

# output attributes file
saveRDS(attributes_df, "attributes.rds")
# end of paste from github

# Since gdp table is diff in wiki, I downloaded the attributes.rds file from github
remove("gdp", "pop", "tidy_pop", "tidy_region", "regions_mat", "region_scrape", "region_url")


# Preparing Attribute Data ----
attributes_df <- readRDS("attributes.rds") # need to be in right wd (working directory)
head(ind_sf)
head(attributes_df)

# merge attributes_df into ind_sf spatial df and mutate 2 new vars
ind_sf <- ind_sf %>% 
  left_join(attributes_df, by = "state_ut") %>% # merge by state_ut
  mutate(
    per_capita_gdp_inr = nominal_gdp_inr / pop_2011,
    per_capita_gdp_usd = nominal_gdp_usd / pop_2011
  ) # create 2 new vars in the end (after geometry var of ind_sf)

head(ind_sf, 3)

# Calculating Area ----
# wiki data already had area collumn, but lets pretend it had not
# careful w the units! convert sq mts to sq kms
library(units)
# mutate area
ind_sf <- ind_sf %>% 
  mutate(my_area = st_area(.)) # st_area fuction (from sf package) calculates the area 
        # requires lwgeom package:
            # install.packages("lwgeom")
        # install.packages("lwgeom", type = "source")
     # also error: went to wd where it was downloaded and pasted in C:\Users\Alexandre Mergulhão\Documents\R\win-library\3.6   
    
# convert units - bcs st_area calculates in m^2
units(ind_sf$my_area) <- with(ud_units, km^2)
# mutate gdp density
ind_sf <- ind_sf %>% 
  mutate(gdp_density_usd_km2 = nominal_gdp_usd / my_area)

# compare classes of both area vars
class(ind_sf$area_km2) # numeric
class(ind_sf$my_area) # units
# values are similar but not equal

# Simplifying Geometry ----
# For simple maps, there's no need to have the fine level of detail that comes with the GADM data or many other sources of geospatial data. 
# Simplification can vastly reduce memory requirements while sacrificing very little in terms of visual output. Fortunately, 
# there's an easy process to reduce the number of vertices in a polygon while retaining the same visible shape.

# One option is sf::st_simplify(), but here we'll use the ms_simplify() function from the rmapshaper package. 
# Below we keep only 1% of the object’s vertices while maintaining the same number of shapes.
# strip units class 
ind_sf <- ind_sf %>% # certify both are vectors for ms_simplify() and then the map
  mutate(
    my_area = as.vector(my_area),
    gdp_density_usd_km2 = as.vector(gdp_density_usd_km2)
  )

original_geometry <- st_geometry(ind_sf) # save geometry into variable

library(rmapshaper) # lets use ms_simplify() from rmapshaper to simplify to 1% of vertices of polygons keeping the shapes
simp_sf <- ms_simplify(ind_sf, keep = 0.01, keep_shapes = TRUE)
simple_geometry <- st_geometry(simp_sf) # save the simplified geometry of map

par(mfrow = c(1,2)) # Set or Query Graphical Parameters, mfrow is the tag/name
# this divides the map into 2 maps:
plot(original_geometry, main = "Original Geometry")
plot(simple_geometry, main = "Simplified Geometry")

# simplification reduced the geometry size from 9.56 MB to just 150 KB and its even better!
install.packages("pryr") # had to delete folder C:\Users\Alexandre Mergulhão\Documents\R\win-library\3.6/00LOCK
library(pryr)
object_size(original_geometry) ## 9.56 MB
object_size(simple_geometry) ## 150 kB

saveRDS(simp_sf, "simp_sf.rds") #command to save a single R object 









# 2nd LESSON ----

# Useful resources for mapping and plotting:
  # Data Visualization: A practical introduction by Kieran Healy:
    my_packages <- c("tidyverse", "broom", "coefplot", "cowplot",
                     "gapminder", "GGally", "ggrepel", "ggridges", "gridExtra",
                     "here", "interplot", "margins", "maps", "mapproj",
                     "mapdata", "MASS", "quantreg", "rlang", "scales",
                     "survey", "srvyr", "viridis", "viridisLite", "devtools")
    
    install.packages(my_packages, repos = "http://cran.rstudio.com")
  # Fundamentals of Data Visualization by Claus O. Wilke
    
    # Just maps: "Making Maps with R" chapter of the previously-mentioned Geocomputation with R
    # Just maps: [Tutorial] Bhaskar V. Karambelkar’s tutorial at useR 2017 on "Geospatial Data Visualization in R"

# Static Maps     ----
# BEST packages for static maps: tmap(), ggplot2(), cartography
# sf()  also has a plot() fuction has we've seen bfr
    simp_sf <- readRDS("simp_sf.rds")
    plot(simp_sf['pop_2011']) # plot onlt population
    
# Thematic Maps (tmap)
# You can pass a spatial dataframe to the tm_shape() function much like you'd pass a dataframe to the ggplot() function.    
# Moreover, because spatial dataframes in the sf package are also dataframes, you can filter out any particular features (like "Andaman & Nicobar Islands" below) and directly proceed with piping the object into a tm_shape() chain.    
install.packages("tmap")
    library(tmap)
# After filtering out union territories, the choropleth below maps India’s GDP density, 
# a measure of economic activity by area. Measured here in units of nominal GDP per square 
# kilometer, GDP density has no clear midpoint, and so it requires a sequential color scale
# as opposed to a diverging or categorical color scale.

# tmap_tricks()

simp_sf %>% 
  filter(!state_ut %in% c("Andaman & Nicobar Islands", "Lakshadweep")) %>% 
  tm_shape() +
  tm_fill(col = "pop_2011", title = "No. People") +
  tm_borders(lwd = 0.5) +
  tm_text("abb", size = 0.5) +
  tm_style("gray") +
  tm_layout(
    main.title = "Population (2011)",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("right", "bottom")
  ) +
  tm_credits("Data:\n2011 Census", position = c("left", "bottom"))

    head(simp_sf)
 # erro: mostra a populacao e nao o gdp per capita...
    
    simp_sf %>% 
      filter(!state_ut %in% c("Andaman & Nicobar Islands", "Lakshadweep")) %>% 
      tm_shape() +
      tm_fill(col = "gdp_density_usd_km2", title = "Avg. USD per person") +
      tm_borders(lwd = 0.5) +
      tm_text("abb", size = 0.5) +
      tm_style("gray") +
      tm_layout(
        main.title = "GDP per capita ",
        main.title.position = c("center"),
        main.title.size = 1,
        legend.position = c("right", "bottom")
      ) +
      tm_credits("Source:\nTua mãe", position = c("left", "bottom"))
    
summary(simp_sf$gdp_density_usd_km2)    
help("summary")
    
# versão de simp_sf mas sem obs com Union Territory
states_Sf <- simp_sf %>%
          filter(!type == "Union Territory")
# grava o mapa em growth e não o executa ja.
growth <- tm_shape(states_Sf) + 
        tm_fill(col = "decadal_growth", title = "Percentage") +
  tm_borders(lwd = 0.5) +
  tm_layout(
    main.title = "Pop growth of states (2001-2011)",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("right", "bottom")
  ) +
  tm_credits("Source:\nTua mãe", position = c("left", "bottom"))

density <- tm_shape(states_Sf) +
  tm_fill(col = "density_km2", title = "No People / Sq Km",
    palette = "YlGnBu") +  
    tm_borders(lwd = 0.5) +
  tm_layout(
    main.title = "Pop Density of states (2011)",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("right", "bottom")
  ) 

# para finalmente plot os 2 graficos gravados nestes dois objectos ao mesmo tempo
  tmap_arrange(growth, density)
 
  
  
# inset maps: "zoom in background"
  # create primary map
ne_sex <- simp_sf %>% 
    filter(region == "Northeastern") %>%
  tm_shape() + # só esta parte da India (Northeastern)
    tm_fill(col = "sex_ratio", title = "Sex Ratio",
            palette = "-Reds") + # -Color para ser o negativo/reverso  
    tm_borders(lwd = 0.5) +
  tm_text('state_ut', size = 0.75) +
    tm_layout(
      main.title = "Sex Ratio in Northeast",
      main.title.position = c("center"),
      main.title.size = 1,
          ) 
  

  # create secondary map - smaller inset map (with total india)
# put it in regions = sum of sub-comp that are the districts
regional_sf <- simp_sf %>%
  group_by(region) %>%
  summarise(pop = sum(pop_2011))

# create object map from this regional sf 
inset <- regional_sf %>%
  filter(!region == "Arabian Sea",
         !region == "Bay of Bengal") %>%
    mutate(northeast = ifelse(region == "Northeast", TRUE, FALSE)) %>%
  tm_shape() +
  tm_fill(col = "northeast", palette = c("gray", "red")) + 
  tm_style("cobalt") +
  tm_legend(show = FALSE)

# finally combine both by trial and error on the parameters on viewport
library(grid)
ne_sex # show primary map (region)
print(inset, vp = viewport(0.28, 0.18, width = 0.2, height = 0.4)) # and then print on top of it, this inset map
  
# could get the region red in inset map...



