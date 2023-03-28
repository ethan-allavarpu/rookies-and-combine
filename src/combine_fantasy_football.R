library(knitr)
library(MASS)
library(alr4)
library(leaps)
library(dplyr)
color_scheme <- c(rgb(0, 0, 0, alpha = 0.25),
                  rgb(0.5, 0, 0, alpha = 0.25),
                  rgb(0, 0.5, 0, alpha = 0.25),
                  rgb(0, 0, 0.5, alpha = 0.25),
                  rgb(0.5, 0, 0.5, alpha = 0.25),
                  rgb(0, 0.5, 0.5, alpha = 0.25))

# Data Gathering and Cleaning

## Download the Data
qb_combine_1 <- read.csv("../data/2000QBcombine.csv", stringsAsFactors = FALSE)
qb_combine_2 <- read.csv("../data/2010QBcombine.csv", stringsAsFactors = FALSE)
qb_combine <- rbind(qb_combine_1, qb_combine_2)

rb_combine_1 <- read.csv("../data/2000RBcombine.csv", stringsAsFactors = FALSE)
rb_combine_2 <- read.csv("../data/2008RBcombine.csv", stringsAsFactors = FALSE)
rb_combine_3 <- read.csv("../data/2014RBcombine.csv", stringsAsFactors = FALSE)
rb_combine <- rbind(rb_combine_1, rb_combine_2, rb_combine_3)

wr_combine_1 <- read.csv("../data/2000WRcombine.csv", stringsAsFactors = FALSE)
wr_combine_2 <- read.csv("../data/2005WRCombine.csv", stringsAsFactors = FALSE)
wr_combine_3 <- read.csv("../data/2010WRCombine.csv", stringsAsFactors = FALSE)
wr_combine_4 <- read.csv("../data/2014WRcombine.csv", stringsAsFactors = FALSE)
wr_combine <- rbind(wr_combine_1, wr_combine_2, wr_combine_3, wr_combine_4)

te_combine_1 <- read.csv("../data/2000TEcombine.csv", stringsAsFactors = FALSE)
te_combine_2 <- read.csv("../data/2011TEcombine.csv", stringsAsFactors = FALSE)
te_combine <- rbind(te_combine_1, te_combine_2)

## Write Helper Functions
clean_players <- function(fantasy) {
  players <- as.character(fantasy$Player)
  players <- strsplit(players, "[*+]")
  for (i in seq_along(players)) {
    fantasy$Player[i] <- players[[i]][1]
  }
  fantasy$Player
}

combine_and_football <- function(fantasy_year, position) {
  position <- casefold(position)
  if (position == "qb") {
    combine <- qb_combine
  } else if (position == "rb") {
    combine <- rb_combine
  } else if (position == "wr") {
    combine <- wr_combine
  } else if (position == "te") {
    combine <- te_combine
  } else {
    stop("Enter a valid position abbreviation!")
  }
  pos <- data.frame()
  for (i in fantasy_year) {
    fantasy <- read.csv(paste("../data/", i,"fantasy.csv", sep = ""),
                        stringsAsFactors = FALSE)
    combine_specific <- combine %>% filter(Year == i)
    fantasy$Player <- clean_players(fantasy)
    rookie <- fantasy[fantasy$Player %in% combine_specific$Player, ] %>%
      filter(FantPos == toupper(position))
    rookie <- rookie[order(rookie$Player), ]
    relevant <- combine_specific[as.character(combine_specific$Player) %in%
                                   rookie$Player, ]
    relevant <- relevant[order(relevant$Player), ]
    specific <- cbind(relevant, "Games" = rookie$G, "Points" = rookie$FantPt.)
    pos <- rbind(pos, specific)
  }
  pos
}

## Create the Necesssary Data Frames
### Years and Positions that Abide by the Function
qbs <- combine_and_football(2000:2019, "qb")

rbs_1 <- combine_and_football(2000:2001, "rb")
rbs_2 <- combine_and_football(2003:2006, "rb")
rbs_3 <- combine_and_football(2008:2019, "rb")

wrs_1 <- combine_and_football(2000:2006, "wr")
wrs_2 <- combine_and_football(2008:2009, "wr")
wrs_3 <- combine_and_football(2011:2019, "wr")

tes <- combine_and_football(2000:2019, "te")

### Years and Positions that are Outliers
position <- "RB"
i <- 2002
combine <- rb_combine
fantasy <- read.csv(paste("../data/", i,"fantasy.csv", sep = ""), stringsAsFactors = FALSE)
combine_specific <- combine %>% filter(Year == i)
fantasy$Player <- clean_players(fantasy)
rookie <- fantasy[fantasy$Player %in% combine_specific$Player, ] %>%
  filter(FantPos == toupper(position))
rookie <- rookie[order(rookie$Player), ]
relevant <- combine_specific[as.character(combine_specific$Player) %in%
                               rookie$Player, ]
relevant <- relevant[order(relevant$Player), ]
rookie <- rookie[-11, ]
rbs_2002 <- cbind(relevant, "Games" = rookie$G, "Points" = rookie$FantPt.)

position <- "RB"
i <- 2007
combine <- rb_combine
fantasy <- read.csv(paste("../data/", i,"fantasy.csv", sep = ""), stringsAsFactors = FALSE)
combine_specific <- combine %>% filter(Year == i)
fantasy$Player <- clean_players(fantasy)
rookie <- fantasy[fantasy$Player %in% combine_specific$Player, ] %>%
  filter(FantPos == toupper(position))
rookie <- rookie[order(rookie$Player), ]
relevant <- combine_specific[as.character(combine_specific$Player) %in%
                               rookie$Player, ]
relevant <- relevant[order(relevant$Player), ]
rookie <- rookie[-2, ]
rbs_2007 <- cbind(relevant, "Games" = rookie$G, "Points" = rookie$FantPt.)

position <- "WR"
i <- 2007
combine <- wr_combine
fantasy <- read.csv(paste("../data/", i,"fantasy.csv", sep = ""), stringsAsFactors = FALSE)
combine_specific <- combine %>% filter(Year == i)
fantasy$Player <- clean_players(fantasy)
rookie <- fantasy[fantasy$Player %in% combine_specific$Player, ] %>%
  filter(FantPos == toupper(position))
rookie <- rookie[order(rookie$Player), ]
relevant <- combine_specific[as.character(combine_specific$Player) %in%
                               rookie$Player, ]
relevant <- relevant[order(relevant$Player), ]
rookie <- rookie[-c(5, 17), ]
wrs_2007 <- cbind(relevant, "Games" = rookie$G, "Points" = rookie$FantPt.)

position <- "WR"
i <- 2010
combine <- wr_combine
fantasy <- read.csv(paste("../data/", i,"fantasy.csv", sep = ""), stringsAsFactors = FALSE)
combine_specific <- combine %>% filter(Year == i)
fantasy$Player <- clean_players(fantasy)
rookie <- fantasy[fantasy$Player %in% combine_specific$Player, ] %>%
  filter(FantPos == toupper(position))
rookie <- rookie[order(rookie$Player), ]
relevant <- combine_specific[as.character(combine_specific$Player) %in% rookie$Player, ]
relevant <- relevant[order(relevant$Player), ]
rookie <- rookie[-25, ]
wrs_2010 <- cbind(relevant, "Games" = rookie$G, "Points" = rookie$FantPt.)

rbs <- rbind(rbs_1, rbs_2002, rbs_2, rbs_2007, rbs_3)
wrs <- rbind(wrs_1, wrs_2007, wrs_2, wrs_2010, wrs_3)

## Subset the Proper Variables
qbs <- qbs %>% dplyr::select(Year:Pos, X40YD:Shuttle, Games:Points)
rbs <- rbs %>% dplyr::select(Year:Pos, X40YD:Shuttle, Games:Points)
wrs <- wrs %>% dplyr::select(Year:Pos, X40YD:Shuttle, Games:Points)
tes <- tes %>% dplyr::select(Year:Pos, X40YD:Shuttle, Games:Points)

## Remove Combine Non-Participants
no_combine <- function(position) {
  combine_stats <- position %>% dplyr::select(X40YD:Shuttle)
  any_combine <- apply(combine_stats, 1, is.na)
  none <- apply(any_combine, 2, sum)
  events <- ncol(combine_stats)
  position$Player[none == events]
}
no_combine(qbs)
qbs <- qbs[qbs$Player != no_combine(qbs)[1] &
             qbs$Player != no_combine(qbs)[2] &
             qbs$Player != no_combine(qbs)[3], ]
qbs <- qbs[!is.na(qbs$Points), ]
no_combine(rbs)
rbs <- rbs[rbs$Player != no_combine(rbs), ]
rbs <- rbs[!is.na(rbs$Points), ]
no_combine(wrs)
wrs <- wrs[wrs$Player != no_combine(wrs)[1] &
             wrs$Player != no_combine(wrs)[2] &
             wrs$Player != no_combine(wrs)[3] &
             wrs$Player != no_combine(wrs)[4]&
             wrs$Player != no_combine(wrs)[5], ]
wrs <- wrs[!is.na(wrs$Points), ]
no_combine(tes)
tes <- tes[tes$Player != no_combine(tes), ]
tes <- tes[!is.na(tes$Points), ]

## Eight Game Minimum
qbs <- qbs %>% filter(Games >= 8) %>% filter(Points >= 10)
rbs <- rbs %>% filter(Games >= 8) %>% filter(Points >= 10)
wrs <- wrs %>% filter(Games >= 8) %>% filter(Points >= 10)
tes <- tes %>% filter(Games >= 8) %>% filter(Points >= 10)

# Data Analysis

# Note: When determining analysis on the combine events, before simply fitting a
## model to everything, we first only consider the events in which at least 60%
## of the participants at that position partook in the event during the combine.
  
## Helper Function
total_nas <- function(position) {
  na_chart <- lapply(position, is.na)
  vapply(na_chart, sum, numeric(1))
}

## The Quarterback
nrow(qbs)
total_nas(qbs)
total_nas(qbs) / nrow(qbs)
names(qbs)[total_nas(qbs) / nrow(qbs) >= 0.4]
qb_rel_vars <- qbs %>% dplyr::select(Player,
                                     X40YD:Vertical,
                                     Broad.Jump:Shuttle,
                                     Points)
par(mfrow = c(2, 3))
plot(Points ~ X40YD, data = qb_rel_vars,
     xlab = "Forty Yard Dash Time (s)",
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[1],
     cex.lab = 1.25,
     las = 1)
plot(0:10, 0:10, type = "n",
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = "")
text(5, 5, "Fantasy Points\n vs.\nCombine Events",
     cex = 1.5)
plot(Points ~ Vertical, data = qb_rel_vars,
     xlab = "Vertical (in)",
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[2],
     cex.lab = 1.25,
     las = 1)
plot(Points ~ Broad.Jump, data = qb_rel_vars,
     xlab = "Broad Jump (in)",
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[3],
     cex.lab = 1.25,
     las = 1)
plot(Points ~ X3Cone, data = qb_rel_vars,
     xlab = "Three Cone Drill (s)",
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[4],
     cex.lab = 1.25,
     las = 1)
plot(Points ~ Shuttle, data = qb_rel_vars,
     xlab = "Shuttle (s)",
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[5],
     cex.lab = 1.25,
     las = 1)

with(qb_rel_vars, cor(X40YD, Points, use = "complete.obs"))
with(qb_rel_vars, cor(Vertical, Points, use = "complete.obs"))
with(qb_rel_vars, cor(Broad.Jump, Points, use = "complete.obs"))
with(qb_rel_vars, cor(X3Cone, Points, use = "complete.obs"))
with(qb_rel_vars, cor(Shuttle, Points, use = "complete.obs"))

### A Model
qb_model <- lm(Points ~ . - Player, data = qb_rel_vars)
par(mfrow = c(2, 2))
plot(qb_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(qb_model)

### A Transformation
summary(powerTransform(Points ~ . - Player, data = qb_rel_vars))
summary(powerTransform(cbind(X40YD, Vertical, Broad.Jump,
                             X3Cone, Shuttle) ~ 1,
                       data = qb_rel_vars))
qb_relevant_transformed <- qb_rel_vars %>% mutate("New_Points" = log(Points))
qb_transformed_model <- lm(New_Points ~ . - Points - Player,
                           data = qb_relevant_transformed)
par(mfrow = c(2, 2))
plot(qb_transformed_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(qb_transformed_model)

### Model Selection
qb_best_subsets <- regsubsets(New_Points ~ . - Points - Player,
                              data = qb_relevant_transformed,
                              nvmax = 5)
summary(qb_best_subsets)
plot(qb_best_subsets)
final_qb_model <- lm(New_Points ~ Broad.Jump + Shuttle,
                     data = qb_relevant_transformed)
par(mfrow = c(2, 2))
plot(final_qb_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(final_qb_model)

## The Running Back
nrow(rbs)
total_nas(rbs)
total_nas(rbs) / nrow(rbs)
names(rbs)[total_nas(rbs) / nrow(rbs) >= 0.4]
rb_rel_vars <- rbs %>% dplyr::select(Player,
                                     X40YD:Broad.Jump,
                                     Points)
par(mfrow = c(2, 2))
plot(Points ~ X40YD, data = rb_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[1])
plot(Points ~ Vertical, data = rb_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[2])
plot(Points ~ BenchReps, data = rb_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[3])
plot(Points ~ Broad.Jump, data = rb_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[4])

### A Model
rb_model <- lm(Points ~ . - Player, data = rb_rel_vars)
par(mfrow = c(2, 2))
plot(rb_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(rb_model)
mmps(rb_model)

### A Transformation
summary(powerTransform(Points ~ . - Player, data = rb_rel_vars))
summary(powerTransform(cbind(X40YD, Vertical,
                             BenchReps, Broad.Jump) ~ 1,
                       data = rb_rel_vars))
rb_relevant_transformed <- rb_rel_vars %>% mutate("New_Points" = log(Points))
rb_transformed_model <- lm(New_Points ~ . - Points - Player,
                           data = rb_relevant_transformed)
par(mfrow = c(2, 2))
plot(rb_transformed_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(rb_transformed_model)

### Model Selection
rb_best_subsets <- regsubsets(New_Points ~ . - Points - Player,
                              data = rb_relevant_transformed,
                              nvmax = 4)
summary(rb_best_subsets)
plot(rb_best_subsets)
final_rb_model <- lm(New_Points ~ Broad.Jump + X40YD, data = rb_relevant_transformed)
par(mfrow = c(2, 2))
plot(final_rb_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(final_rb_model)

## The Wide Receiver
nrow(wrs)
total_nas(wrs)
total_nas(wrs) / nrow(wrs)
names(wrs)[total_nas(wrs) / nrow(wrs) >= 0.4]
wr_rel_vars <- wrs %>% dplyr::select(Player,
                                     X40YD:Vertical,
                                     Broad.Jump:Shuttle,
                                     Points)
par(mfrow = c(2, 2))
plot(Points ~ X40YD, data = wr_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[1])
plot(Points ~ Vertical, data = wr_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[2])
plot(Points ~ Broad.Jump, data = wr_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[3])
plot(Points ~ X3Cone, data = wr_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[4])
plot(Points ~ Shuttle, data = wr_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[5])

### A Model
wr_model <- lm(Points ~ . - Player, data = wr_rel_vars)
par(mfrow = c(2, 2))
plot(wr_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(wr_model)
mmps(wr_model)

### A Transformation
summary(powerTransform(Points ~ . - Player, data = wr_rel_vars))
summary(powerTransform(cbind(X40YD, Vertical, Broad.Jump,
                             X3Cone, Shuttle) ~ 1,
                       data = wr_rel_vars))
wr_relevant_transformed <- wr_rel_vars %>% mutate("New_Points" = log(Points))
wr_transformed_model <- lm(New_Points ~ . - Points - Player,
                           data = wr_relevant_transformed)
par(mfrow = c(2, 2))
plot(wr_transformed_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(wr_transformed_model)

### Model Selection
wr_best_subsets <- regsubsets(New_Points ~ . - Points - Player,
                              data = wr_relevant_transformed,
                              nvmax = 5)
summary(wr_best_subsets)
plot(wr_best_subsets)
final_wr_model <- lm(New_Points ~ Shuttle, data = wr_relevant_transformed)
par(mfrow = c(2, 2))
plot(final_wr_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(final_wr_model)
plot(New_Points ~ Shuttle, data = wr_relevant_transformed,
     ylab = "Fantasy Points ^ (1/3)",
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25))
abline(final_wr_model, col = "red")

## The Tight End
nrow(tes)
total_nas(tes)
total_nas(tes) / nrow(tes)
names(tes)[total_nas(tes) / nrow(tes) >= 0.4]
te_rel_vars <- tes %>% dplyr::select(Player,
                                     X40YD:Shuttle,
                                     Points)
par(mfrow = c(2, 3))
plot(Points ~ X40YD, data = te_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[1])
plot(Points ~ Vertical, data = te_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[2])
plot(Points ~ BenchReps, data = te_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[3])
plot(Points ~ Broad.Jump, data = te_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[4])
plot(Points ~ X3Cone, data = te_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[5])
plot(Points ~ Shuttle, data = te_rel_vars,
     ylab = "Fantasy Points",
     pch = 19, col = color_scheme[6])

### A Model
te_model <- lm(Points ~ . - Player, data = te_rel_vars)
par(mfrow = c(2, 2))
plot(te_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(te_model)
mmps(te_model)

### A Transformation
summary(powerTransform(Points ~ . - Player, data = te_rel_vars))
summary(powerTransform(cbind(X40YD, Vertical, BenchReps,
                             Broad.Jump, X3Cone, Shuttle) ~ 1,
                       data = te_rel_vars))
te_relevant_transformed <- te_rel_vars %>% mutate("New_Points" = Points^(1/3),
                                                  "New_3_Cone" = X3Cone^(-11.81),
                                                  "New_Broad" = Broad.Jump^(-1))
te_transformed_model <- lm(New_Points ~ . - Points - X3Cone - Broad.Jump - Player,
                           data = te_relevant_transformed)
par(mfrow = c(2, 2))
plot(te_transformed_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(te_transformed_model)

### Model Selection
te_best_subsets <- regsubsets(New_Points ~ . - Points - X3Cone - Broad.Jump - Player,
                              data = te_relevant_transformed,
                              nvmax = 6)
summary(te_best_subsets)
plot(te_best_subsets)
final_te_model <- lm(New_Points ~ X40YD + Vertical,
                     data = te_relevant_transformed)
par(mfrow = c(2, 2))
plot(final_te_model,
     pch = 19,
     col = rgb(0, 0, 0, alpha = 0.25),
     add.smooth = FALSE)
summary(final_te_model)

# Final Models
## Quarterback
summary(final_qb_model)

  
## Running Back
summary(final_rb_model)

## Wide Receiver
summary(final_wr_model)

  
## Tight End
summary(final_te_model)

# How Good Are These Rookies?
## Quarterbacks
rookie_qb <- qbs %>% summarise(meanFPts = mean(Points)) %>% as.numeric
rookie_qb
rookie_qb / 16
with(qbs, tapply(Points, Year, mean))

### Change Over Time
ppy <- with(qbs, tapply(Points, Year, mean))
year <- 2001:2019
plot(ppy ~ year,
     xlab = "Year",
     ylab = "Average Points",
     main = "Average Rookie QB Fantasy Points \n by Year",
     pch = 19, col = rgb(0.5, 0, 0, alpha = 0.5))
qb_yr_lm <- lm(ppy ~ year)
summary(qb_yr_lm)
abline(qb_yr_lm, col = "red")


## Running Backs
rookie_rb <- rbs %>% summarise(meanFPts = mean(Points)) %>% as.numeric
rookie_rb
rookie_rb / 16
with(rbs, tapply(Points, Year, mean))

### Change Over Time
ppy <- with(rbs, tapply(Points, Year, mean))
year <- 2000:2019
plot(ppy ~ year,
     xlab = "Year",
     ylab = "Average Points",
     main = "Average Rookie RB Fantasy Points \n by Year",
     pch = 19, col = rgb(0, 0.5, 0, alpha = 0.5))
rb_yr_lm <- lm(ppy ~ year)
summary(rb_yr_lm)
abline(rb_yr_lm, col = "dark green")

## Wide Receivers
rookie_wr <- wrs %>% summarise(meanFPts = mean(Points)) %>% as.numeric
rookie_wr
rookie_wr / 16
with(wrs, tapply(Points, Year, mean))

### Change Over Time
ppy <- with(wrs, tapply(Points, Year, mean))
year <- 2000:2019
plot(ppy ~ year,
     xlab = "Year",
     ylab = "Average Points",
     main = "Average Rookie WR Fantasy Points \n by Year",
     pch = 19, col = rgb(0, 0, 0.5, alpha = 0.5))
wr_yr_lm <- lm(ppy ~ year)
summary(wr_yr_lm)
abline(wr_yr_lm, col = "dark blue")

## Tight Ends
rookie_te <- tes %>% summarise(meanFPts = mean(Points)) %>% as.numeric
rookie_te
rookie_te / 16
with(tes, tapply(Points, Year, mean))

### Change Over Time
ppy <- with(tes, tapply(Points, Year, mean))
year <- 2000:2019
plot(ppy ~ year,
     xlab = "Year",
     ylab = "Average Points",
     main = "Average Rookie TE Fantasy Points \n by Year",
     pch = 19, col = rgb(0.5, 0, 0.5, alpha = 0.5))
te_yr_lm <- lm(ppy ~ year)
summary(te_yr_lm)
abline(te_yr_lm, col = "purple")

# Predictions
combine2020 <- read.csv("../data/2020combine.csv")

## Quarterbacks
qbs_2020 <- filter(combine2020, Pos == "QB")
final_qb_model
qb_predictions <- predict(final_qb_model,
                          newdata = data.frame(Broad.Jump = qbs_2020$Broad.Jump,
                                               Shuttle = qbs_2020$Shuttle))
qb_predictions <- exp(qb_predictions)
names(qb_predictions) <- qbs_2020$Player
sort(qb_predictions, decreasing = TRUE, na.last = TRUE)

## Running Backs
rbs_2020 <- filter(combine2020, Pos == "RB")
final_rb_model
rb_predictions <- predict(final_rb_model,
                          newdata = data.frame(Broad.Jump = rbs_2020$Broad.Jump,
                                               X40YD = rbs_2020$X40YD))
rb_predictions <- exp(rb_predictions)
names(rb_predictions) <- rbs_2020$Player
sort(rb_predictions, decreasing = TRUE, na.last = TRUE)

## Wide Receivers
wrs_2020 <- filter(combine2020, Pos == "WR")
final_wr_model
wr_predictions <- predict(final_wr_model,
                          newdata = data.frame(Shuttle = wrs_2020$Shuttle))
wr_predictions <- exp(wr_predictions)
names(wr_predictions) <- wrs_2020$Player
sort(wr_predictions, decreasing = TRUE, na.last = TRUE)

## Tight Ends
tes_2020 <- filter(combine2020, Pos == "TE")
final_te_model
te_predictions <- predict(final_te_model,
                          newdata = data.frame(Vertical = tes_2020$Vertical,
                                               X40YD = tes_2020$X40YD))
te_predictions <- (te_predictions)^3
names(te_predictions) <- tes_2020$Player
sort(te_predictions, decreasing = TRUE, na.last = TRUE)


# Wonderlic Scores
wonderlic <- read.csv("../data/Wonderlic Scores - Sheet1.csv", stringsAsFactors = FALSE)
qb_wonder <- wonderlic %>% filter(Position == "QB")
qbs <- qbs[qbs$Player %in% qb_wonder$Player, ]
qb_wonder <- qb_wonder[qb_wonder$Player %in% qbs$Player, ]
qb_wonder <- qb_wonder[-47, ]
qb_wonder_score <- qb_wonder[[2]]
names(qb_wonder_score) <- qb_wonder[[1]]
qb_wonder_score <- qb_wonder_score[order(names(qb_wonder_score))]
qbs <- qbs[order(qbs$Player), ]
qb_and_wonder_score <- qbs %>% mutate(Wonderlic = qb_wonder_score)
fantasy_wonderlic <- lm(Points ~ Wonderlic, data = qb_and_wonder_score)
plot(Points ~ Wonderlic, data = qb_and_wonder_score,
     col = rgb(0, 0, 0, alpha = 0.25),
     pch = 19,
     las = 1,
     main = "Fantasy Points vs. Wonderlic Scores for QBs",
     ylab = "Rookie Fantasy Points")
abline(fantasy_wonderlic, col = "red")
legend("topright",
       legend = paste("Fantasy Points = ",
                      round(fantasy_wonderlic$coefficients[1], 2),
                      " - ",
                      round(abs(fantasy_wonderlic$coefficients[2]), 3),
                      "(Wonderlic)", sep = ""),
       col = "red", lty = 1,
       cex = 0.6)
summary(fantasy_wonderlic)