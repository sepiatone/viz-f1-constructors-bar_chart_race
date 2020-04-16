# 08jul19, sepiatone
# bar chart race depicting f1 constructors over the years


# load required packages
library(tidyverse)
library(gganimate)
# library(data.table)


# make_barchart_race(): function to make the bar chart race
# inputs: data, the dataset
#         labels, the column that contains the labels to plot
#         values, the column that contains the numeric values to plot
#         title, the title of the bar chart race, default is blank
#         caption, text to show in the footer, default is blank
#         num_competitors, the number of entries in the bar chart race in any period, default is 10
#         parameters to animate
make_barchart_race <- function(data,
                               labels,
                               values,
                               title = "Formula 1 Constructors - Ranking by Points Over the Years",
                               caption = "",
                               num_competitors = 10,
                               nframes = 300,
                               fps = 5,
                               end_pause = 20)
{
  labels <- rlang::enquo(labels)
  values <- rlang::enquo(values)
  num_competitors <- rlang::enquo(num_competitors)
  
  # take the input dataset, compute ranks within each time period
  # first group by the year, then arrange this grouping in the decreasin value of "values", then set the column "rank"
  # to the row_number(), then only keep the first "num_competitors" in each year
  data %>%
    group_by(year) %>%
    arrange(-!!values) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= !!num_competitors) -> data
  
  # plot the data
  data %>%
    ggplot(aes(x = -rank, y = !!values, fill = !!labels, group = !!labels)) +
    geom_tile(aes(y = !!values/2, height = !!values), width = 0.9, show.legend = F) +
    geom_text(aes(label = !!labels), hjust = "right", colour = "black", fontface = "bold", nudge_y = -1000) +
    geom_text(aes(label = scales::comma(!!values)), hjust = "left", nudge_y = 2000, colour = "grey30") +
    theme_minimal() +
    coord_flip(clip = "off") +
    scale_x_discrete("") +
    scale_y_continuous("", labels = scales::comma) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(size = 20, colour = "grey50", face = "bold"),
          plot.caption = element_text(colour = "grey50"),
          plot.subtitle = element_text(size = 20, colour = "grey50", face = "bold"),
          plot.margin = margin(1, 1, 1, 2, "cm"),
          axis.text.y = element_blank()) +
    
    # this bit does the animation by year
    transition_time(year) +
    labs(title = title, subtitle = '{round(frame_time,0)}', caption = caption) -> p
  
  # animate the plot - this is returned by the function
  animate(p, nframes = nframes, fps = fps, end_pause = end_pause)

  return(data)
}


path <- "C:/at/strategy/projects/bar_chart_race-f1_constructors/data/"
file_constructors <- "constructors.csv"
file_races <- "races.csv"
file_results <- "results.csv"

data_path <- paste0(path, file_constructors)
data_constructor <- read.csv(data_path, header = FALSE,
                 col.names = c("constructor_id", "constructor_ref", "constructor_name", "nationality", "url"))

data_path <- paste0(path, file_races)
data_races <- read.csv(data_path, header = FALSE,
                        col.names = c("race_id", "year", "round", "circuitId", "name", "date", "time", "url"))

data_path <- paste0(path, file_results)
data_results <- read.csv(data_path, header = FALSE,
                       col.names = c("result_id", "race_id", "driver_id", "constructor_id", "number", "grid",
                                     "position", "position_text", "positionOrder", "points", "laps", "time",
                                     "milliseconds", "fastest_lap", "rank", "fastest_lap_time", "fastest_lap_speed",
                                     "status_id"))

# drop the columns we are not interested in
data_results <- select(data_results, one_of(c("result_id", "race_id", "constructor_id", "position", "points")))
data_races <- select(data_races, one_of(c("race_id", "year")))

# add the year of the race
data_results$year <- data_races[match(data_results$race_id, data_races$race_id, nomatch = NA), 2]

# the current points system (fia 2019 formula 1 sporting regulations, section 6.4)
points_pos <- tibble(pos = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), points = c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, 0))

# "points_normalized", i.e., normalized to the current points system
data_results$points_normalized <- points_pos[match(data_results$position, points_pos$pos, nomatch = 11), 2]

# sum up the points for each constructor over a whole year ("points_year")
data_results %>% group_by(constructor_id, year) %>% summarise(points_year = sum(points_normalized)) -> data

# replace the contructor_id with the constructor name
data$name <- data_constructor[match(data$constructor_id, data_constructor$constructor_id), 3]

# calculate the cumulative sum of the points year by year
data %>% group_by(name) %>% arrange(year, .by_group = TRUE) %>% mutate(points_year_cum = cumsum(points_year)) -> data

# remove the fields we no longer require
data$constructor_id <- NULL
data$points_year <- NULL

# call make_barchart_race() and see the magic happen
data <- make_barchart_race(data, name, points_year_cum)

# save it
anim_save("bar_chart_race-f1_constructors.gif")