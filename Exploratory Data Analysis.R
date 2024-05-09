library(tidyverse)
library(lubridate)
library(corrplot)

if (!file.exists("lax_to_jfk/lax_to_jfk.csv")) {
  url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

  download.file(url, destfile = "lax_to_jfk.tar.gz")
  untar("lax_to_jfk.tar.gz")
}

sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                          col_types = cols('DivDistance' = col_number(),
                                           'DivArrDelay' = col_number()))

airline_box <-ggplot(data = sub_airline, mapping = aes(x = Reporting_Airline, y = ArrDelay)) +
      geom_boxplot(fill = "bisque",color = "black", alpha = 0.3) +
      geom_jitter(aes(color = 'blue'), alpha=0.2) +
      labs(x = "Airline") +
      ggtitle("Arrival Delays by Airline") +
      guides(color = "none") +
      theme_minimal() +
      coord_cartesian(ylim = quantile(sub_airline$ArrDelay, c(0, 0.99)))

print(airline_box)

alaska_flights <- sub_airline %>%
  filter(Reporting_Airline == 'AS') %>%
  filter(!is.na(DepDelay) & !is.na(ArrDelay)) %>%
  filter(DepDelay < 40)

alaska_plot <- ggplot(data = alaska_flights, mapping = aes(x = DepDelay, y = ArrDelay)) +
  geom_point(na.rm = TRUE) +
  ggtitle("Alask Flight Departure Delays vs Arrival Delays")

print(alaska_plot)

dep_arr_scatter <- ggplot(data = sub_airline, mapping = aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE)

print(dep_arr_scatter)
print(cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes))

weather_arr_scatter <- ggplot(data = sub_airline, mapping = aes(x = WeatherDelay, y = ArrDelayMinutes)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE)

print(weather_arr_scatter)
print(cor(sub_airline$WeatherDelay, sub_airline$ArrDelayMinutes, use = "complete.obs"))

carrier_arr_scatter <- ggplot(data = sub_airline, mapping = aes(x = CarrierDelay, y = ArrDelayMinutes)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE)

print(carrier_arr_scatter)

summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(count = n(), 
            mean = mean(ArrDelayMinutes, na.rm = TRUE),
            std_dev = sd(ArrDelayMinutes, na.rm = TRUE), 
            min = min(ArrDelayMinutes, na.rm = TRUE), 
            median = median(ArrDelayMinutes, na.rm=TRUE),
            iqr = IQR(ArrDelayMinutes, na.rm = TRUE), 
            max = max(ArrDelayMinutes, na.rm = TRUE))

print(summary_airline_delays)

avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep')

print(head(avg_delays))

sorted <- avg_delays %>%
  arrange(desc(mean_delays))

print(head(sorted))

heatmap <- avg_delays %>%
  ggplot(aes(x = Reporting_Airline,
             y = DayOfWeek,
             fill = mean_delays)) +
  geom_tile(color = 'white', size = 0.2) +
  scale_fill_gradient(low = 'yellow', high = 'red')

print(heatmap)

avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep') %>%
  mutate(bins = cut(mean_delays, breaks = c(-0.1, 0.1, 10, 20, 30, 50, max(mean_delays)),
                    labels = c("0", "0-10", "10-20", "20-30", "30-50", ">50"))) %>%
  mutate(bins = factor(as.character(bins), levels = rev(levels(bins))))

heatmap_bonus <- ggplot(avg_delays, aes(x = Reporting_Airline,
                                        y = lubridate::wday(DayOfWeek, label = TRUE),
                                        fill = bins)) +
  geom_tile(color = 'white', size = 0.2) +
  geom_text(aes(label = round(mean_delays, 3))) +
  guides(fill = guide_legend(title = "Delays Time Scale")) +
  labs(x = "Reporting Airline", y = "Day of Week", title = "Average Arrival Delays") +
  scale_fill_manual(values = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4"))

print(heatmap_bonus)

mean_of_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(mean_delays = mean(ArrDelayMinutes))

print(mean_of_delays)

dep_arr_cor <- sub_airline %>%
  select(DepDelayMinutes, ArrDelayMinutes) %>%
  cor(method = "pearson")

print(dep_arr_cor)

dep_arr_sigtest <- sub_airline %>%
  cor.test(~DepDelayMinutes + ArrDelayMinutes, data = .)

print(dep_arr_sigtest)

correlation <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

print(correlation)


numerics_airlines <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)

airlines_cor <- cor(numerics_airlines, method = "pearson", use = "pairwise.complete.obs")

col <- colorRampPalette(c('#BB4444', '#ee9988', '#FFFFFF', '#77AADD', '#4477AA'))

correlation_plot <- corrplot(airlines_cor, method = 'color', col = col(200),
                             type = 'upper', order = 'hclust',
                             addCoef.col = 'black',
                             tl.col = 'black', tl.srt = 45)

print(correlation_plot)


summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(Average_Delays = mean(ArrDelayMinutes, na.rm = TRUE))

summ_airline_plot <- summary_airline_delays %>%
  ggplot(aes(x = Reporting_Airline, y = Average_Delays)) +
  geom_bar(stat = 'identity') +
  ggtitle("Average Arrival Delays by Airline")

print(summ_airline_plot)

aa_as_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'AS')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
print(summary(ad_aov))

aa_pa_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'PA (1)')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_pa_subset)
print(summary(ad_aov))