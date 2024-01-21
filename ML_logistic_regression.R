# load libraries
if (!require(readr)) install.packages('readr')
if (!require(dplyr)) install.packages('dplyr')
if (!require(caret)) install.packages('caret')
if (!require(ggplot2)) install.packages('ggplot2')


# turn off scientific notation
options(scipen = 999)

# load the data frame
url <- 'https://raw.githubusercontent.com/statsoffnow/ML-logistic-regression/main/heart.csv'
df_heart <- read_csv(url)


## set seed
set.seed(123)

## split data into train set & test set
sample <-
  sample(c(TRUE, FALSE), nrow(df_heart),
                 replace = TRUE, prob = c(0.7, 0.3))

#train set
train_set <- df_heart[sample, ]
test_set <- df_heart[!sample, ]


# logistic regression model
model.one <- 
  glm(data = train_set,
      family = 'binomial',
      DEATH_EVENT ~ .)

summary(model.one)


# variable importance
importance <- varImp(model.one)



variable.names <- 
train_set %>% 
  select(-DEATH_EVENT) %>% 
  colnames()

var.importance <- data.frame(variable.names, importance)

var.importance$fraction <- 
var.importance$Overall/sum(var.importance$Overall)*100


ggplot(data = var.importance,
    aes(x=reorder(variable.names, fraction), y = fraction))+
  geom_segment(aes(xend = variable.names,
                   yend = 0))+
  geom_point(size=7)+
  geom_text(aes(label=round(fraction,0)), color='white')+
  xlab('Variable')+
  ylab('Importance %')+
  coord_flip()+
  theme_minimal()
  

#predict probability of being dead
probabilities <- 
predict(model.one,
        newdata = test_set,
        type = 'response')


pred <- ifelse(probabilities > 0.5, 1, 0)

data.frame(test_set$DEATH_EVENT, pred)

confusionMatrix(factor(pred), factor(test_set$DEATH_EVENT),
                positive = '1')






