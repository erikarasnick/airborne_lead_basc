library(tidyverse)
library(sf)

lead0to12 <- readRDS('unadjusted_airpb.rds')

# filter unadjusted lead predictions to 263 complete cases used in models
d_lead <- readRDS("d_lead.Rds")

lead0to12 <- lead0to12 %>%
  filter(id %in% d_lead$id,
         RowNum < 145)

birth_lead <- lead0to12 %>%
  filter(RowNum == 6)

birth_lead_sf <- birth_lead %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  st_transform(3735)

counties <- tigris::counties('ohio') %>%
  filter(NAME %in% c('Hamilton', 'Butler', 'Warren', 'Clermont')) %>%
  rbind(tigris::counties('indiana') %>% filter(NAME %in% c('Franklin', 'Dearborn'))) %>%
  rbind(tigris::counties('kentucky') %>% filter(NAME %in% c('Campbell', 'Kenton', 'Boone'))) %>%
  st_transform(3735)

roads <- tigris::primary_roads() %>%
  st_transform(3735) %>%
  st_join(counties, left = FALSE)

# epa_monitors <-
#   read_csv(file='/Users/RASV5G/OneDrive - cchmc/Summer2018_ErikaRasnick/CCAAPS/Lead/cincinnati area Lead PM2.5 LC Summary.csv') %>%
#   filter(site %in% c('Covington', 'Taft')) %>%
#   st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
#   st_transform(3735)
# saveRDS(epa_monitors, 'epa_monitors.rds')
epa_monitors <- readRDS('epa_monitors.rds')


library(tmap)

hc_bbox <- st_bbox(counties %>% filter(NAME == "Hamilton"))
hc_bbox[1] <- hc_bbox[1] - 20000
hc_bbox[2] <- hc_bbox[2] - 30000
hc_bbox[3] <- hc_bbox[3] + 20000
hc_bbox[4] <- hc_bbox[4] + 20000

tm_lead <-
  tm_shape(counties %>% filter(NAME == "Hamilton"),
         is.master = TRUE,
         bbox = hc_bbox) +
  tm_polygons() +
tm_shape(counties) +
  tm_polygons(col = "NAME",
              palette = tmaptools::get_brewer_pal("Greys",
                                                  n = 9,
                                                  contrast = c(0.5, 0.1)),
              legend.show = FALSE) +
  tm_shape(roads) +
  tm_lines() +
  tm_shape(birth_lead_sf) +
  tm_symbols(col = "air_lead",
             title.col = expression("Air Lead Concentration" ~ (ng/m^3)),
          border.col = "black",
          palette = "OrRd",
          size = 0.25,
          style = "quantile",
          jitter = 0.005) +
  tm_shape(epa_monitors[1,]) +
  tm_symbols(shape = 23,
             col = '#41b6c4',
             border.col = 'black',
             size = 1) +
  tm_shape(epa_monitors[2,]) +
  tm_symbols(shape = 23,
             col = '#225ea8',
             border.col = 'black',
             size = 1) +
  tm_add_legend(type = 'symbol',
                shape = 23,
                col = c('#41b6c4', '#225ea8'),
                labels = c('Covington', 'Taft'),
                title = 'EPA Monitors') +
  tm_layout(frame = FALSE,
            legend.frame = TRUE,
            legend.format = list(digits = 2),
            legend.position = c("left", "top"),
            legend.text.size = 1,
            legend.title.size = 1,
            legend.height = -0.4,
            legend.width = -0.3) +
  tm_scale_bar(text.size = 0.75)

tmap_save(tm_lead, 'airborne_lead_map.png')

### time series plot
Lead.2sites <- readRDS('lead_2_sites.rds')

Lead.2sites <- Lead.2sites %>%
  mutate(Lead = Lead * 100) # convert to ng

epa_ts <- ggplot() +
  geom_line(data = Lead.2sites,
            aes(x = date,
                y = Lead,
                color = site)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_color_manual(values = c('#41b6c4', '#225ea8')) +
  ylab(expression("Air Lead Concentration" ~ (ng/m^3))) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
       # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave('epa_monitor_ts.png', width = 9.07, height = 4.22)

## variability of estimate for one random subject
set.seed(1234)
r <- lead0to12 %>%
  slice_sample(n = 1)

r_unadj <- lead0to12 %>%
  filter(id == r$id)

airpb_adj <- readRDS("new_pb_age12.Rds")

r_adj <- airpb_adj %>%
  filter(id == r$id)


age1_var <- ggplot() +
  geom_col(data = r_adj %>% filter(age_months > 0, age_months < 13),
             aes(x = age_months,
                 y = new_lead),
           fill = 'gray50') +
  geom_hline(yintercept = r_unadj$air_lead[r_unadj$RowNum == 12],
             color = 'black', lty = 2, lwd = 0.75) +
  annotate(geom = 'text', x = 9.5, y = 1.29,
           label = "Unadjusted Age 1 \nLead Estimate",
           color = 'black', size = 5) +
  scale_x_continuous(breaks = seq(0, 12, 1)) +
  labs(x = "Age (months)",
       y = expression("Air Lead Concentration" ~ (ng/m^3))) +
  theme_minimal() +
  theme(# panel.grid.major = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
        panel.grid.minor = element_blank())
ggsave('age1_variability.png', width = 9.07, height = 4.22)








