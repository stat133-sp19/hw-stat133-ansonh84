workout1-Anson-Huang
================

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
shots_data = read.csv("/Users/anson/Desktop/workout1/data/shots-data.csv", stringsAsFactors = FALSE)
names = c("Stephen Curry", "Andre Iguodala", "Klay Thompson", "Draymond Green", "Kevin Durant")
total2 = c(nrow(filter(shots_data, name == "Stephen Curry" & shot_type == "2PT Field Goal")), nrow(filter(shots_data, name == "Andre Iguodala" & shot_type == "2PT Field Goal")), nrow(filter(shots_data, name == "Klay Thompson" & shot_type == "2PT Field Goal")), nrow(filter(shots_data, name == "Draymond Green" & shot_type == "2PT Field Goal")), nrow(filter(shots_data, name == "Kevin Durant" & shot_type == "2PT Field Goal")))
made2 = c(nrow(filter(shots_data, name == "Stephen Curry" & shot_type == "2PT Field Goal" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Andre Iguodala" & shot_type == "2PT Field Goal" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Klay Thompson" & shot_type == "2PT Field Goal" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Draymond Green" & shot_type == "2PT Field Goal" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Kevin Durant" & shot_type == "2PT Field Goal" & shot_made_flag == "shot_yes")))
twopointers = data.frame(names, total2, made2, made2/total2)
names(twopointers) = c("name", "total", "made", "perc_made")
arrange(twopointers, desc(perc_made))
```

    ##             name total made perc_made
    ## 1 Andre Iguodala   210  134 0.6380952
    ## 2   Kevin Durant   643  390 0.6065319
    ## 3  Stephen Curry   563  304 0.5399645
    ## 4  Klay Thompson   640  329 0.5140625
    ## 5 Draymond Green   346  171 0.4942197

``` r
total3 = c(nrow(filter(shots_data, name == "Stephen Curry" & shot_type == "3PT Field Goal")), nrow(filter(shots_data, name == "Andre Iguodala" & shot_type == "3PT Field Goal")), nrow(filter(shots_data, name == "Klay Thompson" & shot_type == "3PT Field Goal")), nrow(filter(shots_data, name == "Draymond Green" & shot_type == "3PT Field Goal")), nrow(filter(shots_data, name == "Kevin Durant" & shot_type == "3PT Field Goal")))
made3 = c(nrow(filter(shots_data, name == "Stephen Curry" & shot_type == "3PT Field Goal" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Andre Iguodala" & shot_type == "3PT Field Goal" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Klay Thompson" & shot_type == "3PT Field Goal" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Draymond Green" & shot_type == "3PT Field Goal" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Kevin Durant" & shot_type == "3PT Field Goal" & shot_made_flag == "shot_yes")))
threepointers = data.frame(names, total3, made3, made3/total3)
names(threepointers) = c("name", "total", "made", "perc_made")
arrange(threepointers, desc(perc_made))
```

    ##             name total made perc_made
    ## 1  Klay Thompson   580  246 0.4241379
    ## 2  Stephen Curry   687  280 0.4075691
    ## 3   Kevin Durant   272  105 0.3860294
    ## 4 Andre Iguodala   161   58 0.3602484
    ## 5 Draymond Green   232   74 0.3189655

``` r
total = c(nrow(filter(shots_data, name == "Stephen Curry")), nrow(filter(shots_data, name == "Andre Iguodala")), nrow(filter(shots_data, name == "Klay Thompson")), nrow(filter(shots_data, name == "Draymond Green")), nrow(filter(shots_data, name == "Kevin Durant")))
made = c(nrow(filter(shots_data, name == "Stephen Curry" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Andre Iguodala" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Klay Thompson" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Draymond Green" & shot_made_flag == "shot_yes")), nrow(filter(shots_data, name == "Kevin Durant" & shot_made_flag == "shot_yes")))
effective = data.frame(names, total, made, made/total)
names(effective) = c("name", "total", "made", "perc_made")
arrange(effective, desc(perc_made))
```

    ##             name total made perc_made
    ## 1   Kevin Durant   915  495 0.5409836
    ## 2 Andre Iguodala   371  192 0.5175202
    ## 3  Klay Thompson  1220  575 0.4713115
    ## 4  Stephen Curry  1250  584 0.4672000
    ## 5 Draymond Green   578  245 0.4238754

Narrative
=========

### Introduction

With the Golden State Warriors showing an incredibly dominant season and reaching the finals, we are here to analyze the starting lineup and to see if there is any room for improvement in terms of shooting. We have analyzed and visually representing each player's shooting behavior relative to the court and broken down their shooting percentages. Using this information, we can highlight general directions about how and where the players need to improve their own gameplay.

### Data

We first acquired data for each of the star five players on the Warriors. The data contains information about each shot attempt the player has taken throughout the 2016 season. The data details whether it is a 2 or 3 pointer, whether they made the shot, from exactly where on the court it was attempt, how many feet away from the basket it was and when during the game they attempted the shot. Using that information, we were able to create a visual representation of the basketball court, and added in data points to show where shots were attempted for each player and whether or not the shots made. Red points mean missed shots and blue points mean made shots. <img src="/Users/anson/Desktop/workout1/images/gsw-shots-chart.png" width="80%" style="display: block; margin: auto;" /> We then used the combined data to create a table of the 2-pointer, 3-pointer, and total effective shooting percentages of the players, and arranged them by descending percentages to show the more accurate shooters on the lineup.

``` r
arrange(twopointers, desc(perc_made))
```

    ##             name total made perc_made
    ## 1 Andre Iguodala   210  134 0.6380952
    ## 2   Kevin Durant   643  390 0.6065319
    ## 3  Stephen Curry   563  304 0.5399645
    ## 4  Klay Thompson   640  329 0.5140625
    ## 5 Draymond Green   346  171 0.4942197

``` r
arrange(threepointers, desc(perc_made))
```

    ##             name total made perc_made
    ## 1  Klay Thompson   580  246 0.4241379
    ## 2  Stephen Curry   687  280 0.4075691
    ## 3   Kevin Durant   272  105 0.3860294
    ## 4 Andre Iguodala   161   58 0.3602484
    ## 5 Draymond Green   232   74 0.3189655

``` r
arrange(effective, desc(perc_made))
```

    ##             name total made perc_made
    ## 1   Kevin Durant   915  495 0.5409836
    ## 2 Andre Iguodala   371  192 0.5175202
    ## 3  Klay Thompson  1220  575 0.4713115
    ## 4  Stephen Curry  1250  584 0.4672000
    ## 5 Draymond Green   578  245 0.4238754

### Analysis

Looking at the data and the outputs from manipulating the data, we can see that Andre Iguodala has the high scoring percentages for 2-point shots but shoots the least amount of shots. As a result, a potential direction for improvement is to allow Iguodala to be in possession of the ball more and to play aggressively past the three point line in order to take more 2-pointers. Iguodala's shooting effectiveness for 3-pointers are lackluster compared to the team and thus, he should focus on taking closer range shot attempts.

On the other side, Klay Thompson is the team's most effective three point shooter. While Thompson has taken many shot attempts compared to Iguodala, a very large portion of his attempts were two pointers. As a result, we recommend that Klay should play less aggressively and wait for the rest of his team to make space so that he can take long-ranged shots to maximize his advantageous accuracy from beyond the three point line.

Draymond Green is the team's least effective scorer, having the lowest shooting percentage in all the categories of shots. However, Green's role on the team is not to be a primary scorer (that is what Thompson, Curry, and Durant are for), but rather, to make space for the rest of the team to attempt shots. To improve the team, Green must be more aware of the positioning of his teammates and to open up space along with Iguodala in order for the other three members of the team to take safer shots.

Kevin Durant and Stephen Curry are the most aggressive scorers on the team, with both players having second and third in 2-pointers and 3-pointers. These two players differ in that Durant has the highest overall effective shooting percentage out of the entire team, and him taking the third most amount of attempts. Durant is the most consistent scorer and he should be getting the most support from his team, *generally*. Curry is not as consistent as Durant is, but his performance game to game is rather volatile. While the coaches should work on fixing Curry's consistency, the team should prioritize supporting Curry during the games when he is landing shots, and prioritizing Durant during the games when he is not.

### Conclusion

The team as a whole has been performing great, but there needs to be improvement if the Golden State Warriors are to win more championships. The team needs to open up more opportunities for Iguodala to score and that goes in hand with Draymond needing to focus less on scoring and focus more on opening up space for his teammates. Klay should specialize in taking long distance shots and leave the two-pointers for Iguodala or Durant. While Durant and Curry score the most, Durant should be prioritized due to his consistency, unless Curry has been landing shots throughout the game.
