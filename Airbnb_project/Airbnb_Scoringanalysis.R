# install and load the packages
packages<-c("tidyverse", "data.table", "lattice",
            "cluster", "gridExtra","quanteda.textstats",
            "quanteda","GGally","geosphere","modelr")

package.check <- lapply(
  packages,
  FUN = function(x){
    if(!require(x, character.only = TRUE)){
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE, quietly = T)}})
##---------------------------------------------------------------------------##
load('listing.RData')

##---------------------------------------------------------------------------##
# filter the score >= 4.5
listing <- listing %>%
  filter(review_scores_rating>3)

# data transformation -- 1 
listing_score <- listing %>%
  mutate(overall_rating = scale(review_scores_rating**2),
         accuracy = scale(review_scores_accuracy**2),
         cleaniness = scale(review_scores_cleanliness**2),
         checkin = scale(review_scores_checkin**2),
         communication = scale(review_scores_communication**2),
         location = scale(review_scores_location**2),
         value = scale(review_scores_value**2))

# Figure 7 : pair plot for each rating indicators
score_plot <- listing_score%>%
  select(overall_rating, 
         accuracy, 
         cleaniness,
         checkin,
         communication,
         location,
         value)
  
pairs(score_plot, lower.panel = NULL)

# model building 
model_score<- lm(overall_rating~
                   accuracy+
                   cleaniness+
                   checkin+
                   communication+
                   location+
                   value, listing_score)

summary(model_score)

# residual analysis
plot(model_score$residuals)

hist(model_score$residuals)

qqnorm(model_score$residuals)
qqline(model_score$residuals)

# box plot to detect the outliers
boxplot(score_plot$overall_rating) # we don't see the obvious outliers

# split the dataset into 2 sample
sample <- sample(c(TRUE, FALSE), nrow(score_plot), replace = T, prob = c(0.8,0.2))

train <- score_plot[sample, ]
test <- score_plot[!sample,]

# Figure 8 : linear regression model -- traning dataset

model_score_train<- lm(overall_rating~
                   accuracy+
                   cleaniness+
                   checkin+
                   communication+
                   location+
                   value, train)
summary(model_score_train)

model2_plot<-test%>%
  add_predictions(model_score_train)%>%
  ggplot(aes(accuracy, overall_rating))+
  geom_point()+
  geom_point(aes(y=pred), color="coral", alpha=.4)+
  theme_bw()+
  labs(title="Prediction Result on LM model")

model2_plot
