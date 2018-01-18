#plays$form = as.numeric(plays$form)
plays$pass_bool = ifelse(plays$type == "PASS", 1, 0)

library(ggplot2)
library(dplyr)

plays$style = "Misc"
plays$style = ifelse(as.numeric(plays$form) < 12, "Spread", plays$style)
plays$style = ifelse(as.numeric(plays$form) == 12, "Ace", plays$style)
plays$style = ifelse(as.numeric(plays$form) == 21, "Pro", plays$style)
plays$style = ifelse(as.numeric(plays$form) > 21 & as.numeric(plays$form) != 30, "Tight", plays$style)

ggplot(plays, aes(style, ..count..)) + geom_bar(aes(fill = as.factor(pass_bool)), position = "dodge") + labs(title = "Form", fill = "pass_bool")

style_percentages = plays %>% group_by(style) %>% summarise(pass_perc = mean(pass_bool), plays = n()) %>% arrange(-pass_perc) %>% filter(plays > 50)

plays_filtered = plays

library(readr)
game = read_csv("~/Desktop/Football/nfl_00-16/GAME.csv")
pbp = merge(plays_filtered, game, by = "gid")

pbp$sprv = ifelse(pbp$off == pbp$h, pbp$sprv, pbp$sprv * -1)
pbp$pts_diff = pbp$ptso - pbp$ptsd

levels(as.factor(pbp$cond))
pbp$cond = ifelse(grepl("Roof", pbp$cond), "Dome", pbp$cond)
pbp$cond = ifelse(grepl("Rain", pbp$cond) | grepl("Showers", pbp$cond) | grepl("Thunderstorms", pbp$cond), "Rain", pbp$cond)
pbp$cond = ifelse(grepl("Snow", pbp$cond) | grepl("Flurries", pbp$cond), "Snow", pbp$cond)
pbp$cond = ifelse(pbp$cond == "Dome" | pbp$cond == "Rain" | pbp$cond == "Snow", pbp$cond, "Clear")
pbp$cond = as.factor(pbp$cond)

pbp$time_left = pbp$min * 60 + pbp$sec
pbp$wspd[is.na(pbp$wspd)] = 0

pbp$succ[is.na(pbp$succ)] = 0
pbp$prev_succ <- c(0, pbp$succ[-nrow(pbp)])
pbp$prev_succ = ifelse(pbp$dseq == 1, 0, pbp$prev_succ)
pbp$prev_type = c("PASS", pbp$type[-nrow(pbp)])
pbp$prev_pass_succ = ifelse(pbp$prev_type == "PASS" & pbp$prev_succ == "Y", 1, 0)
pbp$prev_rush_succ = ifelse(pbp$prev_type == "RUSH" & pbp$prev_succ == "Y", 1, 0)
pbp$prev_succ = NULL
pbp$prev_type = NULL
pbp$type = NULL

pbp$redzone_bool = ifelse(pbp$zone == 5, 1, 0)
pbp$sg[is.na(pbp$sg)] = 0
pbp$sg = ifelse(pbp$sg == "Y", 1, 0)

pbp_data = pbp %>% dplyr::select(pass_bool, qtr, time_left, dwn, ytg, yfog, sg, timo, timd, wspd, cond, 
                                     sprv, pts_diff, redzone_bool, style)
pbp_data = na.omit(pbp_data)
pbp_data$dwn = as.factor(pbp_data$dwn)

pbp_1 = pbp_data[pbp_data$qtr == 1, -2]

library(rpart)

pbp_1$dwn = as.factor(pbp_1$dwn)
pbp_1$sg = as.factor(pbp_1$sg)

# Create the three sets, train/validation 80%, test 20%
set.seed(123)
splitSample <- sample(1:3, size=nrow(pbp_1), prob=c(0.7,0.1,0.2), replace = TRUE)
train <- pbp_1[splitSample==1,]
valid <- pbp_1[splitSample==2,]
total_train = pbp_1[splitSample == 1 | splitSample == 2, ]
test <- pbp_1[splitSample==3,]

library(caret)
train_control <- trainControl(method="cv", number=10)

tree_fit1 = train(as.factor(pass_bool) ~ ., data = total_train, 
                  method = "glmStepAIC", 
                  trControl = train_control)
tree_fit1

tree_grid <-expand.grid(cp = seq(0.0001, 0.01, 0.001))
tree_grid

tree_fit2 = train(as.factor(pass_bool) ~ ., data = total_train, 
                  method = "rpart", 
                  trControl = train_control,
                  tuneGrid = tree_grid)
tree_fit2

plot(tree_fit2$finalModel)
text(tree_fit2$finalModel)

test_pred = predict(tree_fit1, test)
confMat <- table(test$pass_bool,test_pred)
sum(diag(confMat))/sum(confMat)

tree_cm = confusionMatrix(test$pass_bool, test_pred)
tree_cm

