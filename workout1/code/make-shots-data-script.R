"
Title: Making Shots_Data
Description: The script will take multiple csv files from each player and modify them, then create multiple summary txt files about each and about a compiled data frame at the end
Inputs: stephen-curry.csv, andre-iguodala.csv, klay-thompson.csv, draymond-green.csv, kevin-durant.csv
Outputs: stephen-curry-summary.txt, andrew-iguodala-summary.txt, klay-thompson-summary.txt, kevin-durant-summary.txt, draymond-green-summary.txt, shots-data-summary.txt
"

curry = read.csv("data/stephen-curry.csv", stringsAsFactors = FALSE)
curry$name = "Stephen Curry"
curry$shot_made_flag[curry$shot_made_flag == "y"] = "shot_yes"
curry$shot_made_flag[curry$shot_made_flag == "n"] = "shot_no"
curry$minute = curry$period*12 - curry$minutes_remaining
iggy = read.csv("data/andre-iguodala.csv", stringsAsFactors = FALSE)
iggy$name = "Andre Iguodala"
iggy$shot_made_flag[iggy$shot_made_flag == "y"] = "shot_yes"
iggy$shot_made_flag[iggy$shot_made_flag == "n"] = "shot_no"
iggy$minute = iggy$period*12 - iggy$minutes_remaining
green = read.csv("data/draymond-green.csv", stringsAsFactors = FALSE)
green$name = "Draymond Green"
green$shot_made_flag[green$shot_made_flag == "y"] = "shot_yes"
green$shot_made_flag[green$shot_made_flag == "n"] = "shot_no"
green$minute = green$period*12 - green$minutes_remaining
klay = read.csv("data/klay-thompson.csv", stringsAsFactors = FALSE)
klay$name = "Klay Thompson"
klay$shot_made_flag[klay$shot_made_flag == "y"] = "shot_yes"
klay$shot_made_flag[klay$shot_made_flag == "n"] = "shot_no"
klay$minute = klay$period*12 - klay$minutes_remaining
kd = read.csv("data/kevin-durant.csv", stringsAsFactors = FALSE)
kd$name = "Kevin Durant"
kd$shot_made_flag[kd$shot_made_flag == "y"] = "shot_yes"
kd$shot_made_flag[kd$shot_made_flag == "n"] = "shot_no"
kd$minute = kd$period*12 - kd$minutes_remaining
sink(file = "output/stephen-curry-summary.txt")
summary(curry)
sink()
sink(file = "output/andre-iguodala-summary.txt")
summary(iggy)
sink()
sink(file = "output/draymond-green-summary.txt")
summary(green)
sink()
sink(file = "output/klay-thompson-summary.txt")
summary(klay)
sink()
sink(file = "output/kevin-durant-summary.txt")
summary(kd)
sink()
shots_data = rbind(curry, iggy, green, klay, kd)
write.csv(shots_data, file = "data/shots-data.csv")
sink(file = "output/shots-data-summary.txt")
summary(shots_data)
sink()