# import packages 
packages = c('data.table','tidyverse', 'writexl',
             'geosphere', 'quanteda', 'quanteda.textmodels','quanteda.textstats',
             'quanteda.textplots', 'stringr',
             'glmnet', 'qcc', 'Metrics','car')

package.check <- lapply(
  packages,
  FUN = function(x){
    if(!require(x, character.only = TRUE)){
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE, quietly = T)
    }
  }
)

#-------------------------------------------------------------------------------
# load dataset
load('listing.RData')

#-------------------------------------------------------------------------------
# data enrichement 
# calculate the distance to city center {lat,log)}= [29.941389, -90.086944]
p1 <- cbind(listing$longitude,listing$latitude)
p2 <- c(-90.086944, 29.941389)                                              # city center GPS of New Orlean
listing$dist2center <- distm(p1, p2, fun = distHaversine) / 1000            # calculate distance / unit: km

# 2. add amenities scores
f_amenities_score <- function (dt, method = 1){
  
  # select columns needed
  amenities <- dt %>% select(amenities)
  
  colnames(amenities) <- "equipement"   # change the column name
  
  amenities$equipement<-as.character(amenities$equipement) # convert into character
  
  amenities$equipement<-tolower(amenities$equipement)
  # set the list of the most important amenities
  vamen <- c("air conditioning", "wifi","heating","essentials", "smoke alarm", "tv","iron" , "hangers",
             "hair dryer","kitchen", "long term stays allowed", "fire extinguisher", "shampoo","hot water",
             "coffee maker","carbon monoxide alarm", "washer", "dishes silverware","refrigerator",
             "microwave", "dedicated workspace", "cooking basics", "stove", "oven", "bed_linens", "private entrance", "first aid kit", "dishwasher", "extra pillows blankets"
             , "free parking", "patio balcony", "lock", "backyard", "bathtub", "security cameras",
             "luggage dropoff allowed", "keypad","lockbox","fan","single level home", "crib", "shower gel", "body soap", "freezer", 
             "roomdarkening shades","dining", "baking sheet","elevator", "clothing storage","grill")
  
  # set the list name of the most important amenities
  namen<-c('air_conditioning', 'wifi', 'heating', 'essentials', 'smoke_alarm',
           'tv', 'iron', 'hangers', 'hair_dryer', 'kitchen', 
           'long_term_stays_allowed', 'fire_extinguisher', 'shampoo', 'hot_water', 'coffee_maker',
           'carbon_monoxide_alarm', 'washer', 'dishes_silverware', 'refrigerator', 'microwave',
           'dedicated_workspace', 'cooking_basics', 'stove', 'oven', 'bed_linens',
           'private_entrance', 'first_aid_kit', 'dishwasher', 'extra_pillows_blankets', 'free_parking',
           'patio_balcony', 'lock', 'backyard', 'bathtub', 'security_cameras',
           'luggage_dropoff_allowed', 'keypad', 'lockbox', 'fan', 'single_level_home',
           'crib', 'shower_gel', 'body_soap', 'freezer', 'roomdarkening_shades',
           'dining', 'baking_sheet','elevator', 'clothing_storage', 'grill')
  
  # detect the amenities for each Airbnb ID
  # b_amen_det <- as.data.table(lapply(vamen, f_amen_det))
  b_amen_det <- as.data.table(lapply(vamen, 
                                     function(x) as.data.frame(str_detect(amenities$equipement, x))))
  # convert TRUE / FALSE into 1 / 0
  b_amen_det <- b_amen_det*1
  
  colnames(b_amen_det) <- namen
  # method 1: scoring by order
  if (method ==1){
    b_amen_det <- data.table(mapply(`*`,b_amen_det, 
                                    data.table(t(c(50:1)))))
    
  } else{
    # method 2: scoring by sectors
    b_amen_det[,1:10] <-b_amen_det[,1:10]*50
    b_amen_det[,11:20]<-b_amen_det[,11:20]*40
    b_amen_det[,21:30]<-b_amen_det[,21:30]*30
    b_amen_det[,31:40]<-b_amen_det[,31:40]*20
    b_amen_det[,41:50]<-b_amen_det[,41:50]*10
  }
  
  amenities_score <- b_amen_det%>%mutate(all=rowSums(.))%>%select(all)
  
  return(amenities_score)
}

listing$amenities_score <- f_amenities_score(listing, method = 0)

# calculate review duration / unit: month
listing[, c("first_review", "last_review") := lapply(.SD, as.Date, 'yyyy-mm-dd'), .SDcols = c("first_review", "last_review")]
listing$review_duration <- as.numeric((listing$last_review - listing$first_review) / 31 )

# get bathroom type
listing[, "bathrooms" := as.numeric(str_extract(bathrooms_text, "([0-9])"))]
listing[, "bathrooms_type" := str_extract(bathrooms_text, "[a-z]+")]
listing$bathrooms_type[listing$bathrooms_type == "bath" | listing$bathrooms_type == "baths" | listing$bathrooms_type == "private"] = "private"

# calculate the host duration compared to 2021-12-07
listing$host_duration <- as.numeric((Sys.Date() - as.Date(listing$host_since, "yyyy-mm-dd"))/31) 

#-------------------------------------------------------------------------------
# data cleansing 
# clean missing value
listing$review_duration[listing$review_duration < 0] = NA
listing$host_duration[listing$review_duration   < 0] = NA

# field type change
listing<-listing%>%
  mutate_if(is.character, as.factor)
listing$price <- as.numeric(gsub("\\$","", listing$price))  
listing$host_response_rate <- as.numeric(gsub("\\%","", listing$host_response_rate))   / 100
listing$host_acceptance_rate  <- as.numeric(gsub("\\%","", listing$host_acceptance_rate)) / 100


# remove irrelevant fields, such as Name, Description. etc
listing <- listing%>%
  select(-name, -description, -neighborhood_overview, -host_since, 
         -host_location, -host_verifications, -host_neighbourhood, -latitude, 
         -longitude, - bathrooms_text, -amenities, -first_review, 
         -last_review, -license)

# price: remove price 0 and add logprice
listing<-listing%>%filter(price!=0)
listing$logPrice <- log10(listing$price)


# Outlier removal
listing<-listing%>%
  filter((room_type=="Entire home/apt"&logPrice>1.5 )|   # remove price lower  than $31.6/night
         (room_type=="Hotel room"&logPrice<2.69)|   # remove price higher than $490 /night
         (room_type=="Private room"&logPrice>1.25)|   # remove price lower  than $17.8/night
         (room_type=="Shared room"&logPrice<2.5 ))   # remove price higher than $316 /night

#-------------------------------------------------------------------------------
# data viz 
dt.fig <-  listing

# Figure 1: price=f(accommodates)
ggplot(dt.fig, aes(x=factor(accommodates), y=price)) +
  geom_point(aes(color=factor(accommodates)), alpha=0.6, position="jitter") +
  geom_boxplot(outlier.size = 0, alpha = 0.05) +
  theme(legend.position = "none") +
  ggtitle('Price=f(accommodates)')

# Figure 2: log10(price) =f(accommodates)
ggplot(dt.fig, aes(x=accommodates, y=price)) +
  geom_point(aes(color=factor(accommodates)), position="jitter", alpha=0.4) +
  scale_y_log10()+
  geom_smooth(formula = y ~ x, method="lm", color="red", se=F) +  
  theme(legend.position = "none") + 
  ggtitle('log10(Price)=f(accommodates)')


# Figure 3: Price Histogram
p1 <- ggplot(dt.fig, aes(x = price)) +
  geom_histogram(alpha = .6, fill = 'red', bins = 30)
p1 + labs(title = 'hist of Price',
          x = 'Price',
          y = 'Number')


# Figure 4: log(Price) Histogram
p2 <- ggplot(dt.fig, aes(x = log10(price))) +
  geom_histogram(alpha = .6, fill = 'blue', bins = 30)
p2 + labs(title = 'hist of log10(Price)',
          x = 'log10[Price]',
          y = 'Number')


# Figure 5: Pareto chart of neighborhood
cnt.neighbourhood <- dt.fig[, .(.N), by = .(neighbourhood_cleansed)][order(-N)]
y <- cnt.neighbourhood[,N]
names(y) <- cnt.neighbourhood$neighbourhood_cleansed
pareto.chart(y, 
             # ylim = c(0, 1000),
             ylabl = 'Property counts',
             cumperc = seq(0,100,10),
             main = 'Properties by neighbourhood')


# Figure 6: Metrics plot
dt <- dt.fig%>%
  select(price, room_type, accommodates, bedrooms, beds, review_scores_rating)%>%
  mutate(price = log10(price), review_scores_rating = log10(review_scores_rating))

colnames <- dt$room_type #define the factor for color 

pairs(dt,
      pch = 23,
      cex = 0.75,
      bg  = colors[as.factor(colnames)],
      lower.panel = NULL)

# set graphic parameter to clip plotting to the figure region
par(xpd = TRUE)

# add legend
legend(0, 0, horiz = TRUE, as.vector(unique(colnames)),
       fill = colors, bty = 'n'
)

#-------------------------------------------------------------------------------
# create train & test dataset
set.seed(1986)  # fix the random seed
ptrain <- 0.8   # assign the percentage of training dataset
ntrain   <- floor (dim(listing)[1]* ptrain)
Itrain   <- sample(dim(listing)[1], ntrain)
train <- listing[Itrain]
test  <- listing[-Itrain]

#-------------------------------------------------------------------------------
# we make the comparaison between 4 models
# linear 1 - select all 36 variables and apply ANOVA analysis: VIF default due to aliased coefficients in the model
lm1 <- lm(logPrice ~ host_response_time + host_response_rate + host_acceptance_rate + host_is_superhost + 
            host_listings_count + host_has_profile_pic + host_identity_verified + neighbourhood_cleansed +
            property_type + room_type + accommodates + bathrooms_type +
            bedrooms + beds + minimum_nights + maximum_nights + 
            has_availability + availability_30 + availability_60 + availability_90 + 
            availability_365 + number_of_reviews + number_of_reviews_ltm + number_of_reviews_l30d + 
            amenities_score + review_duration + review_scores_rating + review_scores_accuracy +
            review_scores_cleanliness + review_scores_checkin + review_scores_communication + review_scores_location +
            review_scores_value + instant_bookable + reviews_per_month + dist2center, dt.train)
summary(lm1)

# lm1: plot test predictions
ythd <- c(-3,3)
with(lm1, {plot(fitted.values,residuals,ylim = ythd) 
  points(c(min(fitted.values),max(fitted.values)), c(0,0), type="l")})
hist(lm1$residuals, main="")
qqnorm(lm1$residuals, ylab="Residuals", main="")
qqline(lm1$residuals)

#-------------------------------------------------------------------------------
# linear 2 - based on lm1 remove trivial variables and keep 30: host_acceptance_rate, host_is_superhost, host_has_profile_pic, host_identity_verified,  neighbourhood_cleansed, property_type, 
lm2 <- lm(logPrice ~ host_response_rate + host_acceptance_rate  + 
            host_listings_count +
            room_type + accommodates + bathrooms_type +
            bedrooms + beds + minimum_nights + maximum_nights + 
            has_availability + availability_30 + availability_60 + availability_90 + 
            availability_365 + number_of_reviews + number_of_reviews_ltm + number_of_reviews_l30d + 
            amenities_score + 
            review_duration + review_scores_rating + review_scores_accuracy +
            review_scores_cleanliness + review_scores_checkin + review_scores_communication + review_scores_location +
            review_scores_value + instant_bookable + reviews_per_month + dist2center, dt.train)
summary(lm2)
vif(lm2)   # check the collinearity

ythd <- c(-3,3)
with(lm2, {plot(fitted.values,residuals,ylim = ythd) 
  points(c(min(fitted.values),max(fitted.values)), c(0,0), type="l")})
hist(lm2$residuals, main="")
qqnorm(lm2$residuals, ylab="Residuals", main="")
qqline(lm2$residuals)

# Prediction of test and get evaluation
pred <- lm2 %>% predict(dt.test)
y.pred <- pred[!is.na(pred)]
y      <- dt.test$logPrice[!is.na(pred)]

# lm2: plot test predictions
estimate <- data.table(y.pred, y)
ggplot(estimate, aes(x = y, y = y.pred)) +
  geom_point(pch = 16, alpha = 0.5) +
  geom_smooth(formula = y ~ x, method = lm) +
  xlim(1, 3) +
  ylim(1, 3) +
  ggtitle('Test prediction of lm2 with 33 features')

k <- lm2$qr$rank                             # feature number
n <- length(y.pred)                          # valid train number
errorlm2 <- data.table(
  RMSE   = rmse(y, y.pred),
  R2     = cor(y, y.pred)^2
)                                            # calculate RMSE and R2
errorlm2$R2.ajs <-  
  1 - (1-errorlm2$R2) * (n - 1)/(n- k -1)    # get adjusted R2

#-------------------------------------------------------------------------------
#linear 3: remove 13 more variables and keep 17: host_acceptance_rate, host_listings_count, beds,maximum_nights, has_availability, availability_60,  availability_90, number_of_reviews_ltm, number_of_reviews_l30d, review_scores_accuracy,review_scores_cleanliness,review_scores_checkin,  instant_bookable, reviews_per_month
lm3 <- lm(logPrice ~ host_response_rate +
            room_type + accommodates + bathrooms_type +
            bedrooms +  minimum_nights + availability_30 + 
            availability_365 + number_of_reviews + number_of_reviews_l30d + 
            amenities_score + 
            review_duration + review_scores_rating + 
            review_scores_communication + review_scores_location +
            review_scores_value + dist2center, train)
summary(lm3)
vif(lm3)    # check the collinearity

# lm3: plot residuals
ythd <- c(-3,3)
with(lm3, {plot(fitted.values,residuals,ylim = ythd)
  points(c(min(fitted.values),max(fitted.values)), c(0,0), type="l")})

hist(lm3$residuals, main="")

qqnorm(lm3$residuals, ylab="Residuals", main="")
qqline(lm3$residuals)

# Prediction of test and get evaluation
pred <- lm3 %>% predict(dt.test)
y.pred <- pred[!is.na(pred)]
y      <- dt.test$logPrice[!is.na(pred)]

# lm3: plot test predictions
estimate <- data.table(y.pred, y)
ggplot(estimate, aes(x = y, y = y.pred)) +
  geom_point(pch = 16, alpha = 0.5) +
  geom_smooth(formula = y ~ x, method = lm) +
  xlim(1, 3) +
  ylim(1, 3) +
  ggtitle('Test prediction of lm3 with 17 features')

errorlm3 <- data.table(
  RMSE = rmse(y, y.pred),
  R2   = cor(y, y.pred)^2
)                                            # calculate RMSE and R2
k <- lm3$qr$rank                             # feature number
n <- length(y.pred)                          # valid train number
errorlm3$R2.ajs <-  
  1 - (1-errorlm3$R2) * (n - 1)/(n- k -1)    # get adjusted R2

#-------------------------------------------------------------------------------
# Lasso Regression

# STEP 1: prepare train and test for lasso regression
x.train <- train%>%
  select(host_response_rate , host_acceptance_rate, host_listings_count, accommodates,
         bedrooms , beds , minimum_nights , maximum_nights, availability_30 , availability_60, 
         availability_90 , availability_365 , number_of_reviews , number_of_reviews_ltm , 
         number_of_reviews_l30d , amenities_score , review_duration , review_scores_rating , 
         review_scores_accuracy ,review_scores_cleanliness , review_scores_checkin ,
         review_scores_communication , review_scores_location , review_scores_value, 
         reviews_per_month , dist2center)

x.test <- test%>%
  select(host_response_rate , host_acceptance_rate, host_listings_count, accommodates,
         bedrooms , beds , minimum_nights , maximum_nights, availability_30 , availability_60, 
         availability_90 , availability_365 , number_of_reviews , number_of_reviews_ltm , 
         number_of_reviews_l30d , amenities_score , review_duration , review_scores_rating , 
         review_scores_accuracy ,review_scores_cleanliness , review_scores_checkin ,
         review_scores_communication , review_scores_location , review_scores_value, 
         reviews_per_month , dist2center)

x.train <- as.matrix(x.train)
y.train <- as.vector(train$logPrice)

x.test  <- as.matrix(x.test)
y.test  <- as.vector(test$logPrice)

##----------------------------------------------------------------------------## 
# STEP 2: Choose to get minimum lambda instead of best lambda
cvfit <- cv.glmnet(x.train, y.train)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
plot(cvfit)

# plot the relationship between lambda coefficients
rr1 <- glmnet(x.train, y.train, alpha = 1)
plot(rr1, xvar = "lambda", label = TRUE)

##----------------------------------------------------------------------------## 
# STEP 3: Evaluate train and test results
# get train predicted value
y.train.pred <- predict(cvfit, newx = x.train, type = "response", s = "lambda.min")
est.train    <- data.table(y.train.pred, y.train)
names(est.train) <- c('y.pred', 'y.test')
ggplot(est.train, aes(x = y.train, y = y.train.pred)) +
  geom_point(pch = 16, alpha = 0.5) +
  geom_smooth(formula = y ~ x, method = lm) +
  ggtitle('Train result of Lasso with minimum lambda')

# get test predicted value
y.test.pred  <- predict(cvfit, newx = x.test , type = "response", s = "lambda.min")
est.test <- data.table(y.test.pred, y.test)
names(est.test) <- c('y.pred', 'y.test')
ggplot(est.test, aes(x = y.test, y = y.pred)) +
  geom_point(pch = 16, alpha = 0.5)           +
  geom_smooth(formula = y ~ x, method = lm)   +   
  ggtitle('Test prediction of Lasso with minimum lambda')

errorlasso.test <- data.table(
  RMSE = rmse(est.test$y.test, est.test$y.pred),
  R2   = cor(est.test$y.test , est.test$y.pred)^2
)
k <- length(coef(cvfit, s = "lambda.min")@i) - 1 # feature number
n <- length(est.test$y.pred) # valid test number
errorlasso.test$R2.ajs <-  
  1 - (1-errorlasso.test$R2) * (n - 1)/(n- k -1) # get adjusted R2

