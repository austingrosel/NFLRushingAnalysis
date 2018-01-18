library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)

play <- read_csv("~/Desktop/nfl_00-16/PBP.csv")
chart <- read_csv("~/Desktop/nfl_00-16/CHART.csv")
player <- read_csv("~/Desktop/Football/nfl_00-16/PLAYER.csv")

plays = merge(play[,c(1:28)], chart, by = "pid")
plays = merge(plays, player[,c(1,5)], by.x = "bc", by.y = "player")

plays$form = plays$rb * 10 + plays$te

run_plays = plays[plays$type == "RUSH" & is.na(plays$kne) & !grepl("scramble", plays$detail) & plays$pos1 != "K", ]

run_plays$style = "Misc"
run_plays$style = ifelse(as.numeric(run_plays$form) < 12, "Spread", run_plays$style)
run_plays$style = ifelse(as.numeric(run_plays$form) == 12, "Ace", run_plays$style)
run_plays$style = ifelse(as.numeric(run_plays$form) == 21, "Pro", run_plays$style)
run_plays$style = ifelse((as.numeric(run_plays$form) > 21 | as.numeric(run_plays$form) == 13) & as.numeric(run_plays$form) != 30, "Tight", run_plays$style)

run_plays$form = as.factor(run_plays$form)
levels(run_plays$form)

run_plays$style = as.factor(run_plays$style)
levels(run_plays$style)

run_plays$direction = "Inside"
run_plays$direction = ifelse(run_plays$dir == "LE" | run_plays$dir == "RE", "End", run_plays$direction)
#run_plays$direction = ifelse(run_plays$dir == "LG" | run_plays$dir == "RG", "Guard", run_plays$direction)
run_plays$direction = ifelse(run_plays$dir == "LT" | run_plays$dir == "RT", "Tackle", run_plays$direction)

run_plays$success = ifelse(is.na(run_plays$succ), 0, 1)
run_plays$shotgun = ifelse(is.na(run_plays$sg), 0, 1)

run_types = run_plays %>% group_by(style, direction) %>% filter(dwn == 1, ytg == 10, pos1 != "QB") %>% summarise(n = n(), ypc = mean(yds), succ = mean(success)) %>% arrange(-ypc)
run_dir = run_plays %>% group_by(direction) %>% filter(dwn == 1, ytg == 10, pos1 != "QB") %>% summarise(n = n(), ypc = mean(yds), succ = mean(success)) %>% arrange(-ypc)
run_style = run_plays %>% group_by(style) %>% filter(dwn == 1, ytg == 10, pos1 != "QB") %>% summarise(n = n(), ypc = mean(yds), succ = mean(success)) %>% arrange(-ypc)

run_plays %>% group_by(shotgun) %>% filter(dwn == 1, ytg == 10, pos1 != "QB") %>% summarise(n = n(), ypc = mean(yds), succ = mean(success)) %>% arrange(-ypc)

ggplot(run_plays, aes(yds, fill = direction, colour = direction)) +
  geom_density(alpha = 0.2) + xlim(-10, 40)

short_ydg_runs = run_plays %>% filter((dwn == 4 | dwn == 3) & ytg < 3)
short_ydg_runs = short_ydg_runs[short_ydg_runs$pos1 != "K" & !grepl("Punt", short_ydg_runs$detail),]

short_ydg_runs$success = ifelse(is.na(short_ydg_runs$succ), 0, 1)
short_ydg_runs$shotgun = ifelse(is.na(short_ydg_runs$sg), 0, 1)

short_ydg_runs %>% summarise(suc = mean(success), n = n())
short_ydg_runs %>% group_by(pos1) %>% summarise(amt = n(), suc = mean(success)) %>% arrange(-suc)
short_ydg_runs %>% group_by(shotgun) %>% summarise(amt = n(), suc = mean(success)) %>% arrange(-suc)
short_ydg_runs %>% group_by(style, direction) %>% summarise(amt = n(), suc = mean(success)) %>% arrange(-suc)
short_ydg_runs %>% group_by(style) %>% summarise(amt = n(), suc = mean(success)) %>% arrange(-suc)
short_ydg_runs %>% group_by(direction) %>% summarise(amt = n(), suc = mean(success)) %>% arrange(-suc)
short_ydg_runs %>% group_by(style, direction) %>% filter(pos1 == "QB") %>% summarise(amt = n(), suc = mean(success)) %>% arrange(-suc)

short_ydg_runs %>% group_by(direction, style) %>% summarise(amt = n(), suc = mean(success)) %>% filter(amt > 50) %>% arrange(-suc)

g1 = ggplot(short_ydg_runs, aes(as.factor(pos1), ..count..)) + geom_bar(aes(fill = as.factor(success)), position = "dodge") + labs(title = "Position", fill = "success")
g2 = ggplot(short_ydg_runs, aes(as.factor(shotgun), ..count..)) + geom_bar(aes(fill = as.factor(success)), position = "dodge") + labs(title = "Shotgun", fill = "success")
g3 = ggplot(short_ydg_runs, aes(as.factor(style), ..count..)) + geom_bar(aes(fill = as.factor(success)), position = "dodge") + labs(title = "Style", fill = "success")
g4 = ggplot(short_ydg_runs, aes(as.factor(direction), ..count..)) + geom_bar(aes(fill = as.factor(success)), position = "dodge") + labs(title = "Direction", fill = "success")

grid.arrange(g1,g2,g3,g4,nrow = 2)


fourth_down_total = play %>% filter((dwn == 4 | dwn == 3) & ytg < 3 & type == "RUSH")

fourth_down_total$direction = "Middle"
fourth_down_total$direction = ifelse(fourth_down_total$dir == "LE" | fourth_down_total$dir == "RE", "End", fourth_down_total$direction)
fourth_down_total$direction = ifelse(fourth_down_total$dir == "LG" | fourth_down_total$dir == "RG", "Guard", fourth_down_total$direction)
fourth_down_total$direction = ifelse(fourth_down_total$dir == "LT" | fourth_down_total$dir == "RT", "Tackle", fourth_down_total$direction)

fourth_down_total$success = ifelse(is.na(fourth_down_total$succ), 0, 1)
fourth_down_total %>% group_by(direction) %>% summarise(amt = n(), suc = mean(success))

rush_total = play %>% filter(dwn == 1 & type == "RUSH")

rush_total$direction = "Middle"
rush_total$direction = ifelse(rush_total$dir == "LE" | rush_total$dir == "RE", "End", rush_total$direction)
rush_total$direction = ifelse(rush_total$dir == "LG" | rush_total$dir == "RG", "Guard", rush_total$direction)
rush_total$direction = ifelse(rush_total$dir == "LT" | rush_total$dir == "RT", "Tackle", rush_total$direction)
rush_total$success = ifelse(is.na(rush_total$succ), 0, 1)
rush_total %>% group_by(direction) %>% summarise(amt = n(), suc = mean(yds))


run_plays %>% group_by(style, direction, shotgun)  %>% filter(dwn == 1, ytg == 10, pos1 != "QB") %>% summarise(n = n(), ypc = mean(yds), succ = mean(success)) %>% filter(n > 150) %>% arrange(-ypc)
