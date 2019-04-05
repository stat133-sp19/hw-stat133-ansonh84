"
Title: Making Shots_Data Charts
Description: The script will take the data for each player and create visual representations of the data
Inputs: nba-court.jpg
Outputs: klay-thompson-shot-chart.pdf, stephen-curry-shot-chart.pdf, andre-iguodala-shot-chart.pdf, draymond-green-shot-chart.pdf, kevin-durant-shot-chart.pdf, gsw-shots-chart.pdf, gsw-shots-chart.png
"

library(jpeg)
library(grid)
library(ggplot2)
court_file <- "images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))
klay_shot_chart <- ggplot(data = klay) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
pdf(file = "images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5)
klay_shot_chart
dev.off()
curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
pdf(file = "images/stephen-curry-shot-chart.pdf", width = 6.5, height = 5)
curry_shot_chart
dev.off()
iggy_shot_chart <- ggplot(data = iggy) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
pdf(file = "images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5)
iggy_shot_chart
dev.off()
green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
pdf(file = "images/draymond-green-shot-chart.pdf", width = 6.5, height = 5)
green_shot_chart
dev.off()
kd_shot_chart <- ggplot(data = kd) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
pdf(file = "images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5)
kd_shot_chart
dev.off()
shots_chart <- ggplot(data = shots_data) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: GSW (2016 season)') +
  theme_minimal() +
  facet_wrap( ~ name, ncol=3)
pdf(file = "images/gsw-shots-chart.pdf", width = 8, height = 7)
shots_chart
dev.off()
png(filename = "images/gsw-shots-chart.png", width = 8, height = 7, units = "in", res = 144)
shots_chart
dev.off()