## ---- sensor-map
library(sugrrants)
library(lubridate)
library(tidyverse)
library(ggmap)

sensor_loc <- rwalkr::pull_sensor()
qmplot(x = Longitude, y = Latitude, data = sensor_loc,
  colour = I("#d95f02"), size = I(5))

## ---- theme-remark
theme_remark <- function() {
  theme_grey() +
  theme(
    axis.text = element_text(size = 14), 
    strip.text = element_text(size = 16), 
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16),
    legend.position = "bottom"
  )
}
theme_set(theme_remark())

## ---- selected-sensor
sensors <- c("Southern Cross Station", "Victoria Market", "Southbank")

sensor_more <- sensor_loc %>% 
  mutate(
    Sensor = if_else(Sensor == "QV Market-Peel St", "Victoria Market", Sensor),
    Selected = if_else(Sensor %in% sensors, TRUE, FALSE)
  )
sensor_unsel <- sensor_more %>% 
  filter(Selected == FALSE)
sensor_sel <- sensor_more %>% 
  filter(Selected == TRUE)
qmplot(
  x = Longitude, y = Latitude, data = sensor_unsel,
  colour = Selected, shape = Selected, size = I(5)
) +
geom_point(aes(x = Longitude, y = Latitude), data = sensor_sel, size = I(12)) +
scale_colour_brewer(palette = "Dark2") +
theme_remark()

## ---- ped-data
ped_run <- rwalkr::run_melb(year = 2017)
ped_walk <- rwalkr::walk_melb(
  from = as_date("2017-12-01"), to = as_date("2017-12-08")
)
pedestrian <- bind_rows(ped_run, ped_walk) %>% 
  mutate(
    Sensor = if_else(Sensor == "QV Market-Peel St", "Victoria Market", Sensor),
    Holiday = if_else(Date %in% c(au_holiday(2017)$date, as_date("2017-09-29")), 
    TRUE, FALSE)
  )

## ---- ped-sub
subdat <- pedestrian %>% 
  filter(Sensor %in% sensors) %>% 
  mutate(Day = wday(Date, label = TRUE, week_start = 1))

## ---- ts-plot
# conventional time series plot
subdat %>% 
  ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
  geom_line(size = 0.5) +
  facet_grid(
    Sensor ~ ., 
    labeller = labeller(Sensor = label_wrap_gen(20)),
    scales = "free_y"
  ) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme_remark() +
  xlab("Date Time") +
  ylab("Hourly Counts")

## ---- facet-time
# time series plot faceted by sensors and day of week
subdat %>% 
  ggplot(aes(x = Time, y = Count, group = Date, 
    colour = Sensor)) +
  geom_line(size = 0.5) +
  facet_grid(
    Sensor ~ Day, 
    labeller = labeller(Sensor = label_wrap_gen(20)),
    scales = "free_y"
  ) +
  scale_x_continuous(breaks = seq(6, 23, by = 6)) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme_remark() +
  xlab("Time") +
  ylab("Hourly Counts")

## ---- southbank-2016
# calendar plot for southbank
southbank <- subdat %>% 
  filter(Sensor == "Southbank")

southbank_cal <- southbank %>%
  frame_calendar(x = Time, y = Count, date = Date)
p_southbank <- southbank_cal %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date)) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  theme_remark()
prettify(p_southbank)

## ---- sx
sx_cal <- pedestrian %>% 
  filter(Sensor == "Southern Cross Station") %>% 
  frame_calendar(x = Time, y = Count, date = Date)
sx_cal %>% 
  select(Time, Count, Date, .Time, .Count)

## ---- sx-plot
p_sx <- ggplot(sx_cal, aes(x = .Time, y = .Count, group = Date)) +
  geom_line() +
  theme_remark()
p_sx

## ---- sx-prettify
prettify(p_sx)

## ---- sx-hol
p2_sx <- sx_cal %>% 
  ggplot(aes(.Time, .Count, group = Date, colour = Holiday)) +
  geom_line() +
  theme_remark()
prettify(p2_sx)

## ---- sx-march
p3_sx <- pedestrian %>% 
  filter(
    Sensor == "Southern Cross Station",
    Date >= as_date("2017-03-01"), Date <= as_date("2017-03-31")
  ) %>% 
  frame_calendar(x = Time, y = Count, date = Date) %>% 
  ggplot(aes(.Time, .Count, group = Date, colour = Holiday)) +
  geom_line() +
  theme_remark()
prettify(p3_sx, label = c("label", "text", "text2"), size = 5)

## ---- facet
# calendar plots faceted by the sensors
facet_cal <- subdat %>% 
  group_by(Sensor) %>% 
  frame_calendar(
    x = Time, y = Count, date = Date, nrow = 2
  )

p_facet <- facet_cal %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date)) +
  geom_line(aes(colour = Sensor)) +
  facet_grid(
    Sensor ~ ., 
    labeller = labeller(Sensor = label_wrap_gen(20))
  ) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme_remark()
prettify(p_facet, label = NULL)
