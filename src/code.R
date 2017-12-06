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

sensor_loc %>% 
  mutate(
    Sensor = if_else(Sensor == "QV Market-Peel St", "Victoria Market", Sensor),
    Selected = ifelse(Sensor %in% sensors, TRUE, FALSE)
  ) %>% 
  qmplot(
    x = Longitude, y = Latitude, data = .,
    colour = Selected, shape = Selected, size = I(5)
  ) +
  scale_colour_brewer(palette = "Dark2") +
  theme_remark()

## ---- ped-data
ped_run <- rwalkr::run_melb(year = 2017)
ped_walk <- rwalkr::walk_melb(
  from = as_date("2017-11-01"), to = as_date("2017-12-05")
)
pedestrian <- bind_rows(ped_run, ped_walk) %>% 
  mutate(
    Sensor = if_else(Sensor == "QV Market-Peel St", "Victoria Market", Sensor)
  )
pedestrian

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
  filter(Sensor == "Southbank") %>% 
  mutate(
    Holiday = ifelse(Date %in% c(au_holiday(2017)$date, as_date("2017-09-29")), 
    TRUE, FALSE)
  )

southbank_cal <- southbank %>%
  frame_calendar(x = Time, y = Count, date = Date)
p_southbank <- southbank_cal %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date)) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  theme_remark()
prettify(p_southbank)

## ---- southbank-2016-plot
p_southbank <- southbank_cal %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date, colour = Holiday)) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  theme_remark()
prettify(p_southbank)

## ---- weekly
southbank_weekly <- southbank %>%
  frame_calendar(x = Time, y = Count, date = Date, calendar = "weekly")
p_southbank_weekly <- southbank_weekly %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date, colour = Holiday)) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  theme_remark()
prettify(p_southbank_weekly)

## ---- daily
southbank_daily <- southbank %>%
  frame_calendar(x = Time, y = Count, date = Date, calendar = "daily")
p_southbank_daily <- southbank_daily %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date, colour = Holiday)) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  theme_remark()
prettify(p_southbank_daily, size = 5)

## ---- southbank-free
# calendar plot for southbank street station using local scale
southbank_cal_free <- southbank %>% 
  frame_calendar(
    x = Time, y = Count, date = Date, scale = "free"
  )

p_southbank_free <- southbank_cal_free %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date)) +
  geom_line()
prettify(p_southbank_free, size = 5)

## ---- scatterplot
# lagged scatterplot for southbank street station in the daily calendar format
southbank_cal_day <- southbank %>% 
  mutate(Lagged_Count = lag(Count)) %>% 
  frame_calendar(
    x = Lagged_Count, y = Count, date = Date, 
    width = 0.95, height = 0.8
  )

p_southbank_day <- southbank_cal_day %>% 
  ggplot(aes(x = .Lagged_Count, y = .Count, group = Date)) +
  geom_point(size = 0.7, alpha = 0.6)
prettify(p_southbank_day, size = 5)

## ---- overlay
# overlaying calendar plots 
subset_cal <- subdat %>% 
  frame_calendar(Time, Count, Date)

sensor_cols <- c(
  "#1b9e77" = "#1b9e77", 
  "#d95f02" = "#d95f02", 
  "#7570b3" = "#7570b3"
) # Dark2
p_three <- subset_cal %>% 
  ggplot() +
  geom_line(
    data = filter(subset_cal, Sensor == sensors[1]),
    aes(.Time, .Count, group = Date, colour = sensor_cols[1])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor == sensors[2]),
    aes(.Time, .Count, group = Date, colour = sensor_cols[2])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor == sensors[3]),
    aes(.Time, .Count, group = Date, colour = sensor_cols[3])
  ) +
  scale_colour_identity(
    name = "Sensor",
    breaks = names(sensor_cols),
    labels = c(
      "State Library", 
      "Flagstaff Station",
      "Flinders Street Station Underpass"
    ),
    guide = "legend"
  ) +
  theme(legend.position = "bottom")
prettify(p_three, size = 3, label.padding = unit(0.15, "lines"))

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

## ---- boxplot
# boxplots for hourly counts across all the sensors in 2017 March
pedestrian_mar <- pedestrian %>% 
  filter(Date >= as_date("2017-03-01"), Date <= as_date("2017-03-31")) %>% 
  frame_calendar(
    x = Time, y = Count, date = Date, 
    width = 0.97, height = 0.97
  )
sx <- pedestrian_mar %>% 
  filter(Sensor == "Southern Cross Station")
p_boxplot <- pedestrian_mar %>% 
  ggplot() +
  geom_boxplot(
    aes(x = .Time, y = .Count, group = Date_Time),
    outlier.size = 0.8, width = 0.005, 
    position = "identity", colour = "grey60"
  ) +
  geom_line(aes(.Time, .Count, group = Date), sx, colour = "#756bb1", size = 1)
prettify(p_boxplot, label = c("label", "text", "text2"), size = 5)
