state <- map_data("state")

michigan <- subset(state, region=="michigan")
counties <- map_data("county")
michigan_county <- subset(counties, region=="michigan")

mi_map <- ggplot(data=michigan, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=michigan_county, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  scale_fill_viridis_c('mm/yr',direction = -1) +
  coord_sf(expand=F) +
  #ggtitle('Michigan Map with Counties') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  cowplot::theme_cowplot()+
  theme(panel.grid.major = element_line(color = gray(.8),
                                        linetype = 'dashed',
                                        size = 0.3),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill=NA,color = 'black'),
        panel.ontop = TRUE,
        axis.text.x = element_text(size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.7)),
        axis.title.x=element_text(size = rel(0.7)),
        axis.title.y=element_text(size = rel(0.7)))
#mi_map

######## Map2
data("us_states", package = "spData")
me <- counties("Michigan", cb = TRUE, progress_bar = FALSE)

#plot(me$geometry)

michigan_county_ba<- subset(me, COUNTYFP == c("017"))
michigan_county_hu<- subset(me, COUNTYFP == c("063"))
michigan_county_tu<- subset(me, COUNTYFP == c("151"))
michigan_county_sa<- subset(me, COUNTYFP == c("157"))

michigan_county_all <- rbind(michigan_county_ba,michigan_county_hu,michigan_county_tu,
                             michigan_county_sa)
#plot(michigan_county_all$geometry)

#michigan_county_all %>%
# as_tibble()

us_states_2163 = st_transform(us_states, crs = 2163)
michigan_county_all_2163 = st_transform(michigan_county_all, crs = 2163)


michigan_county_all_2163_bb = st_as_sfc(st_bbox(michigan_county_all_2163))

ggm1 = ggplot() + 
  geom_sf(data = us_states_2163, fill = "white") + 
  geom_sf(data = michigan_county_all_2163_bb, fill = NA, color = "red", size = 1.4) +
  theme_void()

#ggm1

# Palette
pal <- hcl.colors(4, "cividis", rev = TRUE, alpha = 0.7)

ggm2 = ggplot() + 
  geom_sf(data = michigan_county_all_2163, 
          aes(fill = NAME), color = NA) +
  labs(#title = "Varieties Dry Beans Locations",
    #subtitle = "NUTS2 regions (2016)",
    #caption = "Eurostat, © EuroGeographics for the administrative boundaries",
    fill = "Loc") +
  # Custom palette
  scale_fill_manual(values = pal,
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         label.position = "bottom")) +
  theme_void() +
  theme(legend.position = c(1.1, 0.06),
        legend.direction = "horizontal",
        legend.key.width = unit(10, "mm"))

#ggm2


MI_trials_beans_map = ggdraw() +
  draw_plot(ggm2, width = 0.7, height = 0.7) +
  draw_plot(ggm1, x = 0.05, y = 0.65, width = 0.3, height = 0.3) +
  draw_plot(mi_map, x = 0.53, y = 0.5, width = 0.45, height = 0.45)

rm(ggm1, ggm2, michigan, michigan_county_all, michigan_county_all_2163, michigan_county_all_2163_bb, michigan_county_ba,michigan_county_tu, michigan_county_sa, michigan_county_hu, state, us_states, us_states_2163, me, mi_map, michigan_county, counties)