# Stanford - coding assessment -------------------------------------------------
# This code creates the tasks from Stanford's RA programming assessment 

# 0. Settings ------------------------------------------------------------------

# This cleans previous objects in the environment
rm(list=ls())
# This makes space for R
gc()

# Libraries
xfun::pkg_attach(c('tidyverse','haven','fixest', 
                   'ggplot2', 'readr', 'sf', 'raster'), install=T)

# Input
mother_data <- unzip(zipfile = "programming-assessment-2022.zip", 
                     files = "programming-assessment-2022/MotherData.dta") %>%
  haven::read_dta(file = ., encoding = 'UTF-8')

location <- unzip(zipfile = "programming-assessment-2022.zip", 
                  files = "programming-assessment-2022/Locations.csv") %>%
  readr::read_csv(file = ., locale = locale(encoding = 'UTF-8'))

# This is the shapefile I use and I downloded from https://datacatalog.worldbank.org/search/dataset/0039368
shapefile <- unzip(zipfile = "ngaadmbndaadm2osgof.zip", 
                  files = "nga_admbnda_adm2_osgof/nga_admbnda_adm2_osgof_20170222.shp") %>%
  sf::read_sf(.)

# The climate data I downloaded from https://www.worldclim.org/data/worldclim21.html
avg_temp_data <- unzip(zipfile = "wc2.1_10m_tavg.zip", 
                  files = "wc2.1_10m_tavg_06.tif") %>% 
  raster::raster(.) %>% 
  as.data.frame(., xy = T) %>% 
  dplyr::rename(avg_temp = wc2.1_10m_tavg_06, lat = x, lon = y) %>% 
  sf::st_as_sf(x = ., coords = c('lat', 'lon'))

# 1. Reshape ------------------------------------------------------------------------------

mother_data_reshaped <- mother_data %>%
  tidyr::pivot_longer(data = ., cols = dplyr::starts_with(match = 'b'), # Here I take only the column names that starts with b - these are the quentions about the births
                      names_to = c('variable', 'b_id'),
                      names_pattern = 'b(.*)_(.*)') %>% # Here I create a variable that contains the questions and identification for the number of the birth
  dplyr::mutate(variable = case_when(variable == 0 ~ 'twins',
                                     variable == 1 ~ 'month',
                                     variable == 2 ~ 'year',
                                     variable == 3 ~ 'century',
                                     variable == 4 ~ 'sex',
                                     variable == 5 ~ 'alive',
                                     variable == 6 ~ 'age_death_reported',
                                     variable == 7 ~ 'age_death_calculated',
                                     variable == 8 ~ 'age',
                                     variable == 9 ~ 'lives_with',
                                     variable == 10 ~ 'birth_date',
                                     variable == 11 ~ 'preceding_birth_interval',
                                     variable == 12 ~ 'succeding_birth_interval',
                                     variable == 13 ~ 'age_at_death',
                                     variable == 15 ~ 'any_other_births',
                                     variable == 16 ~ 'child_household',
                                     T ~ variable)) %>% 
  tidyr::pivot_wider(data = ., names_from = 'variable', values_from = 'value') # Here I turn the question x birth data to only birth data

readr::write_csv(mother_data_reshaped, file = 'MotherData_reshaped_Cruz.csv')

# 2. Birth Analysis  (2nd to 5th task) ------------------------------------------------------------------------------
# I create the binary variable with the calculated age in months at the time the child died
birth_data <- mother_data_reshaped %>% 
  dplyr::mutate(infant_death = case_when(age_death_calculated <= 12 ~ 1, T ~0)) 

# Here I calculate the total of births and then the deaths
birth_data %>% 
  dplyr::filter(is.na(idx) == F) %>% 
  nrow(.)

birth_data %>% 
  dplyr::filter(is.na(idx) == F & infant_death == 1) %>% 
  nrow(.)

# Here I run the regression of wealth score and infant mortality
birth_data %>% 
  fixest::feols(v191 ~ infant_death, vcov = 'hetero') %>% 
  fixest::etable(.)

# Here I run the regression of wealth score and infant mortality with the interaction of the sex variable
birth_data %>% 
  dplyr::mutate(sex = sex -1) %>% 
  fixest::feols(infant_death ~ v191*sex, vcov = 'hetero') %>% 
  fixest::etable(.)

# 3. Village Analysis (6th to 7th task) ------------------------------------------------------------------------------
# I group the data in village codes and calculate the mean, I already joined the location data
village_data <- birth_data %>% 
  dplyr::group_by(v001) %>% 
  dplyr::summarise(across(.cols = c(v191, infant_death), ~mean(.))) %>% 
  dplyr::full_join(location, by = 'v001')

# The plot for Figure 1
ggplot(village_data, aes(x = v191, y = infant_death)) +
  geom_point() + labs(x = 'Wealth Score', y = 'Infant Mortality Rate') +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        title = element_text(size = 12)) + theme_minimal() +
  ggtitle('Figure 1: Scatter plot of village-average wealth score versus 
village-average probability of infant death')

# Here I transform the data at village level to a st object and intersect with the shapefile of Nigeria
village_map <- birth_data %>% 
  dplyr::group_by(v001) %>% 
  dplyr::summarise(across(.cols = c(v191, infant_death), ~mean(.))) %>% 
  dplyr::full_join(location, by = 'v001') %>% 
  sf::st_as_sf(x = ., coords = c('lat', 'lon')) %>% 
  sf::st_intersection(shapefile)

# This is plot for Figure 2
ggplot() +
  geom_sf(data = shapefile) + 
  geom_sf(data = village_map, mapping = aes(color = infant_death)) +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        title = element_text(size = 12)) + theme_minimal() +
  ggtitle('Figure 2: Infant Mortality Rate of villages in Nigeria') +
  labs(color  = 'Infant Mortality Rate')

# 4. Average Temperature Analysis (8th to 9th task) ------------------------------------------------------------------------------
# Here I make the file that was TIF into a st object
avg_temp_data_shapefile  <-  avg_temp_data  %>% 
  sf::st_intersection(shapefile)

# Here I plot the temperature data in Nigeria and the location data for the village
# and I understand that they are not exactly the same
ggplot() +
  geom_sf(data = avg_temp_data_shapefile, mapping = aes(color= avg_temp)) +
  geom_point(village_data, mapping = aes(x = lat, y = lon))

# Here I calculate the difference for each point of the temperature map to the location
# of the village map and I select the points that have the minimum distance to a village
avg_temp_data_village <- avg_temp_data_shapefile %>% 
  cbind(
    village_map[st_nearest_feature(avg_temp_data_shapefile, village_map),]) %>% 
  dplyr::mutate(dist = st_distance(geometry, geometry.1, by_element = T)) %>% 
  dplyr::group_by(v001) %>% 
  dplyr::mutate(min = min(dist)) %>% 
  dplyr::distinct(min,v001, .keep_all = T)


# Here is the plot for Figure 3
ggplot(avg_temp_data_village, aes(x = avg_temp, y = infant_death)) +
  geom_point() + geom_smooth(method=lm) + 
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        title = element_text(size = 12)) + 
  labs(title = 'Figure 3: Relationship between average temperature\nand average infant mortality rate in each village of Nigeria',
       x = 'Average Temperature', y = 'Average Infant Mortality Rate')

# I do a 4th plot to understand the non-linear relationship between the average temperature
# and the average infant mortality
ggplot(avg_temp_data_village, aes(x = avg_temp, y = infant_death)) +
  geom_point() + geom_smooth() +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        title = element_text(size = 12)) + 
  labs(title = 'Figure 4: Loess Regression between average temperature\nand average infant mortality rate in each village of Nigeria',
       x = 'Average Temperature', y = 'Average Infant Mortality Rate')

# That's it! Bye!
