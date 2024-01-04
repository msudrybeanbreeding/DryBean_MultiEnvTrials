### Plot Clips by Leonardo Volpato

#rm(list=ls())
#gc()

#my.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
#setwd(my.path)

library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)
library(usmap)
library(stringr)
library(viridis)
library(RColorBrewer)
library(patchwork)

#setwd("G:/Shared drives/Bean_Lab/Volpato/GxE_Variety_trials_Scott/Manuscript/Review/New figure/Figure1")

# Importing dataset
black_beans_production_in_cwt <- read_csv("utils/data/black beans_production in cwt.csv")
#View(black_beans_production_in_cwt)

navy_beans_production_in_cwt <- read_csv("utils/data/navy beans_production in cwt.csv")
#View(navy_beans_production_in_cwt)

small_red_production_in_cwt <- read_csv("utils/data/small red_production in cwt.csv")
#View(small_red_production_in_cwt)

# Adjusitng data 1
names(black_beans_production_in_cwt)
black_beans_production_in_cwt_sel <- black_beans_production_in_cwt %>% 
  dplyr::select('Year', "Geo Level", 'State', "Data Item", 'Value') %>% 
  mutate(mkt = "BB")

navy_beans_production_in_cwt_sel <- navy_beans_production_in_cwt %>% 
  dplyr::select('Year', "Geo Level", 'State', "Data Item", 'Value')%>% 
  mutate(mkt = "NB")

small_red_production_in_cwt_sel <- small_red_production_in_cwt %>% 
  dplyr::select('Year', "Geo Level", 'State', "Data Item", 'Value')%>% 
  mutate(mkt = "SR")


data_bean_fig <- bind_rows(black_beans_production_in_cwt_sel, navy_beans_production_in_cwt_sel, small_red_production_in_cwt_sel)


data_bean_us <- data_bean_fig %>% 
  dplyr::filter(`Geo Level` == "NATIONAL") 

data_bean_us_clean <- data_bean_us %>%
  mutate(Value = as.numeric(gsub(",", "", Value))) %>% 
  dplyr::filter(!is.na(Value)) %>% 
  dplyr::filter(!Value == 0)

str(data_bean_us_clean)

data_bean_us_ton <- data_bean_us_clean %>%
  mutate(Value_ton = (Value * 45.3592)/1000)

# Assuming data_bean_us_ton is your data
# Ensure the data is sorted by Year for correct plotting
data_bean_us_ton <- data_bean_us_ton %>% arrange(Year)
data_bean_us_ton$Year <- as.factor(data_bean_us_ton$Year)

unique_years <- unique(data_bean_us_ton$Year)
n_unique_years <- as.numeric(as.character(unique_years))
display_years <- n_unique_years[n_unique_years %% 2 == 0]  # Using the mod operator
display_positions <- match(display_years, unique_years)  # Get positions of these years

data_bean_us_ton_bb<- data_bean_us_ton %>% 
  dplyr::filter(mkt == "BB")

# Create a sequence of years from 1919 to 1952
missing_years <- seq(1919, 1952)

# Create a new data frame with the missing years and Value, Value_ton set to 0 for market BB
missing_data <- data.frame(
  Year = factor(missing_years, levels = as.character(seq(1919, max(as.numeric(levels(data_bean_us_ton_bb$Year)))))),
  `Geo Level` = "NATIONAL",
  State = "US TOTAL",
  `Data Item` = "BEANS, DRY EDIBLE, BLACK - PRODUCTION, MEASURED IN CWT",
  Value = 0,
  mkt = "BB",
  Value_ton = 0
)
  # Bind the new data frame with missing years to the original data_bean_us_ton_bb data frame
  data_bean_us_ton_bb <- dplyr::bind_rows(missing_data, data_bean_us_ton_bb)
  
  # Arrange the data by Year
  data_bean_us_ton_bb <- data_bean_us_ton_bb %>% dplyr::arrange(as.numeric(as.character(Year)))
  

#Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd

#data_bean_us_ton_bb$Year <- as.numeric(as.character(data_bean_us_ton_bb$Year))

# Your existing code
bb <- ggplot(data_bean_us_ton_bb, aes(x = as.numeric(as.factor(Year)), y = 0.5, fill = Value_ton)) + 
  geom_tile(data = data_bean_us_ton_bb,
            aes(x = as.numeric(as.factor(Year)), y = 0.5, fill = Value_ton), height = 0.8, width = 1) +
  scale_fill_gradientn(colors = brewer.pal(9, "Greys"),
                       breaks = seq(min(data_bean_us_ton_bb$Value_ton, na.rm = TRUE), 
                                    max(data_bean_us_ton_bb$Value_ton, na.rm = TRUE), 
                                    length.out = 8),  # Adjust this to change the number of breaks
                       labels = scales::label_number(big.mark = ",", accuracy = 1000),  # This will format the labels with commas
                       name = NULL) +  # This line removes the legend title
  scale_x_continuous(breaks = display_positions, labels = display_years) +
  scale_y_continuous(labels = NULL) +
  theme_minimal() +
  theme(line = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(b = -5)),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
       # axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=1, margin = margin(t = -5)),  # Adjust top margin
        legend.position = "right",
        legend.text = element_text(angle = 0, vjust = 0.7, hjust=1, margin = margin(t = -10), size = 8),  # Adjust top margin
        legend.margin = margin(t = -10),  # Adjust top margin of legend box
        legend.box.margin = margin(0,0,0,0)) + ggtitle("BB")


# Print the plot
#print(bb)


data_bean_us_ton_nb<- data_bean_us_ton %>% 
  dplyr::filter(mkt == "NB")


#Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd

#data_bean_us_ton_nb$Year <- as.numeric(as.character(data_bean_us_ton_nb$Year))


# Your existing code
nb <- ggplot(data_bean_us_ton_nb, aes(x = as.numeric(as.factor(Year)), y = 0.5, fill = Value_ton)) + 
  geom_tile(data = data_bean_us_ton_nb,
            aes(x = as.numeric(as.factor(Year)), y = 0.5, fill = Value_ton), height = 0.8, width = 1) +
  scale_fill_gradientn(colors = brewer.pal(9, "Blues"),
                       breaks = seq(min(data_bean_us_ton_nb$Value_ton, na.rm = TRUE), 
                                    max(data_bean_us_ton_nb$Value_ton, na.rm = TRUE), 
                                    length.out = 8),  # Adjust this to change the number of breaks
                       labels = scales::label_number(big.mark = ",", accuracy = 1000),   # This will format the labels with commas
                       name = NULL) +  # This line removes the legend title
  scale_x_continuous(breaks = display_positions, labels = display_years) +
  scale_y_continuous(labels = NULL) +
  theme_minimal() +
  theme(line = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(b = -5)),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
       # axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=1, margin = margin(t = -5)),  # Adjust top margin
        legend.position = "right",
        legend.text = element_text(angle = 0, vjust = 0.7, hjust=1, margin = margin(t = -10), size = 8),  # Adjust top margin
        legend.margin = margin(t = -10),  # Adjust top margin of legend box
        legend.box.margin = margin(0,0,0,0)) +  # This line reduces margin around the legend box
  labs(x = "Year", y = "") + ggtitle("NB")

# Print the plot
#print(nb)

data_bean_us_ton_sr<- data_bean_us_ton %>% 
  dplyr::filter(mkt == "SR")


#Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd

#data_bean_us_ton_sr$Year <- as.numeric(as.character(data_bean_us_ton_sr$Year))
# Filter years for breaks every 5 years


# Your existing code
sr <- ggplot(data_bean_us_ton_sr, aes(x = as.numeric(as.factor(Year)), y = 0.5, fill = Value_ton)) + 
  geom_tile(data = data_bean_us_ton_sr,
            aes(x = as.numeric(as.factor(Year)), y = 0.5, fill = Value_ton), height = 0.8, width = 1) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"),
                       breaks = seq(min(data_bean_us_ton_sr$Value_ton, na.rm = TRUE), 
                                    max(data_bean_us_ton_sr$Value_ton, na.rm = TRUE), 
                                    length.out = 8),  # Adjust this to change the number of breaks
                       labels = scales::label_number(big.mark = ",", accuracy = 1000),  # This will format the labels with commas
                       name = NULL) +  # This line removes the legend title
  scale_x_continuous(breaks = display_positions, labels = display_years) +
  scale_y_continuous(labels = NULL) +
  theme_minimal() +
  theme(line = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(b = -5)),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, margin = margin(t = 5), size = 8),  # Adjust top margin
        legend.position = "right",
        legend.text = element_text(angle = 0, vjust = 0.7, hjust=1, margin = margin(t = -10), size = 8),  # Adjust top margin
        legend.margin = margin(t = -10),  # Adjust top margin of legend box
        legend.box.margin = margin(0,0,0,0)) +  # This line reduces margin around the legend box
  labs(x = "Years", y = "") + ggtitle("SR")



# Print the plot
#print(sr)

# 

combined_plot <- bb + nb + sr + plot_layout(ncol = 1, guides = "keep") +
  labs(caption = "Source: USDA - NASS: https://www.nass.usda.gov")

print(combined_plot)


#######################
########## Figure 2


planted_area_acres <- read_csv("utils/data/planted_area_acres.csv")

# Adjusitng data 1
names(planted_area_acres)

planted_area_acres <- planted_area_acres %>% 
  dplyr::filter(Period == "YEAR" & State == "MICHIGAN" & `Geo Level` == "COUNTY"
  )

data_bean_y_m_c <- planted_area_acres %>% 
  dplyr::select('Year', "Geo Level", 'State', "Data Item", 'County', 'Value') 


data_bean_y_m_c <- data_bean_y_m_c %>%
  mutate(
    Value = as.numeric(gsub(",", "", Value)),  # Remove commas and convert to numeric if necessary
    Value = Value * 0.40468564  # Convert acres to hectares
  )


#data_bean_y_m_c$County<- as.factor(data_bean_y_m_c$County)

data_bean_y_m_c <- data_bean_y_m_c %>%
  dplyr::filter(!str_detect(County, "OTHER \\(COMBINED\\) COUNTIES"))

levels(as.factor(data_bean_y_m_c$`Data Item`))

data_bean_y_m_c <- data_bean_y_m_c %>% 
  dplyr::arrange(`Data Item`) %>% 
  dplyr::slice(1:1465)

latest_data_per_county <- data_bean_y_m_c %>%
  dplyr::arrange(County, desc(Year)) %>%
  dplyr::group_by(County) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()


mi_counties_sf <- st_read("./utils/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
mi_data <- mi_counties_sf %>% 
  dplyr::filter(STATEFP == "26")

# Convert the Value column to numeric
latest_data_per_county <- latest_data_per_county %>%
  mutate(Value = as.numeric(gsub(",", "", Value)))

mi_data <- mi_data %>%
  mutate(NAME = toupper(NAME))

# Convert county names to uppercase
latest_data_per_county <- latest_data_per_county %>%
  mutate(County = toupper(County))

dim(mi_data)
dim(latest_data_per_county)
# Merge the data frames
merged_data <- mi_data %>%
  left_join(latest_data_per_county, by = c("NAME" = "County"))
dim(merged_data)

# Compute centroids of each county for plotting points
county_centroids <- st_centroid(st_geometry(merged_data))

# Extract coordinates of centroids
centroids_data <- cbind(merged_data, st_coordinates(county_centroids))

# Filter the data for the specified counties
selected_counties <- merged_data %>%
  filter(NAME %in% c("HURON", "BAY", "SANILAC", "TUSCOLA"))

selected_centroids <- centroids_data %>%
  filter(NAME %in% c("HURON", "BAY", "SANILAC", "TUSCOLA"))


# Normalize the Year column to [0, 1]
latest_data_per_county <- latest_data_per_county %>%
  mutate(Year_norm = (Year - min(Year)) / (max(Year) - min(Year)),
         Value = as.numeric(gsub(",", "", Value)))


# Determine breaks for the Year and Value legends
year_breaks <- seq(min(merged_data$Year, na.rm = TRUE), max(merged_data$Year, na.rm = TRUE), by = 10)
value_breaks <- c( 500, 1000, 5000, 10000, 20000, 28000)

# Plot the map
light_gray <- rgb(0.9, 0.9, 0.9)

mi_plot<- ggplot() +
  geom_sf(data = merged_data, aes(fill = Year), color = NA) +
  scale_fill_gradientn(colors = brewer.pal(5, "PuBu"),
                       breaks = year_breaks,
                       labels = year_breaks,
                       na.value = light_gray) +
  geom_point(data = centroids_data, aes(x = X, y = Y, size = Value), alpha = 0.6) +
  scale_size_continuous(range = c(1, 8),
                        name = "Area (ha)",
                        breaks = value_breaks,
                        labels = value_breaks,
                        guide = guide_legend(override.aes = list(alpha = 1))) +  # Remove transparency in legend
  geom_sf(data = selected_counties, fill = NA, color = "red", lwd = 1) +  # Outline specified counties in red
  geom_sf_text(data = selected_counties, aes(label = NAME), size = 3.2, check_overlap = F,
               nudge_x = 0.05, nudge_y = 0.15, fontface = "bold") +  # Label specified counties
  theme_minimal() +
  theme(axis.title = element_blank()) +
  labs(caption = "Source: USDA - NASS: https://www.nass.usda.gov")

print(mi_plot)

rm(bb, nb, sr,
   black_beans_production_in_cwt, black_beans_production_in_cwt_sel,
   centroids_data,
   county_centroids,
   data_bean_us,
   data_bean_us_clean,
   data_bean_us_ton,
   data_bean_us_ton_bb,
   data_bean_us_ton_nb,
   data_bean_us_ton_sr,
   data_bean_y_m_c,
   latest_data_per_county,
   merged_data,
   mi_counties_sf,
   mi_data,
   missing_data,
   navy_beans_production_in_cwt,
   navy_beans_production_in_cwt_sel,
   small_red_production_in_cwt,
   small_red_production_in_cwt_sel,
   planted_area_acres,
   selected_centroids,
   selected_counties,
   data_bean_fig
   )











