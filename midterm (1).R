imdb = read.csv("/Users/valentinlehericy/Downloads/data_final.csv")
imdb = imdb[-c(1667),]

attach(imdb)

#useful command
options(scipen=999) # remove scientific notation
rm(list=ls(all=TRUE)) # clear data segment
par(mfrow=c(1,1)) # reset plot


#all packages
library(stargazer)
library(ggplot2)
require("psych")
require(lmtest)
require(plm)
library(car)
library(splines)
require(caTools)
library(boot)




#define continuous variables
continuous_variables = names(imdb)[c(5,6,7, 13, 15, 17, 19, 21, 23, 25, 27, 30, 32,33,34, 36,37,38, 52,53,54,55) ]
length(continuous_variables)
continuous_variables


#exploring continuous variables 
par(mfrow=c(2,3))
for(i in 1:6){
  plot(get(continuous_variables[i]), imdb_score, xlab= continuous_variables[i], ylab= "Imdb Score"  )
}

for(i in 7:12){
  plot(as.numeric(get(continuous_variables[i])), imdb_score, xlab= continuous_variables[i], ylab= "Imdb Score"  )
}

for(i in 13:18){
  plot(as.numeric(get(continuous_variables[i])), imdb_score, xlab= continuous_variables[i], ylab= "Imdb Score"  )
}

for(i in 19:23){
  plot(get(continuous_variables[i]), imdb_score, xlab= continuous_variables[i], ylab= "Imdb Score"  )
}

#CONTINUOUS VARIABLES  WHERE you have to change the scale to see the real distribution because outliers hide it 
plot(get(continuous_variables[4]), imdb_score, xlab= continuous_variables[4], ylab= "Imdb Score", xlim = c(0,15000)  )
plot(get(continuous_variables[6]), imdb_score, xlab= continuous_variables[6], ylab= "Imdb Score", xlim= c(0,100000)  )

plot(get(continuous_variables[7]), imdb_score, xlab= continuous_variables[7], ylab= "Imdb Score", xlim = c(0,100000)  )
plot(get(continuous_variables[8]), imdb_score, xlab= continuous_variables[8], ylab= "Imdb Score", xlim= c(0,30000)  )

plot(get(continuous_variables[14]), imdb_score, xlab= continuous_variables[14], ylab= "Imdb Score", xlim = c(0,50000)  )
plot(get(continuous_variables[15]), imdb_score, xlab= continuous_variables[15], ylab= "Imdb Score", xlim= c(0,30)  )
plot(get(continuous_variables[17]), imdb_score, xlab= continuous_variables[17], ylab= "Imdb Score", xlim = c(0,300000000)  )
plot(get(continuous_variables[18]), imdb_score, xlab= continuous_variables[18], ylab= "Imdb Score", xlim= c(0,10000)  )

plot(get(continuous_variables[19]), imdb_score, xlab= continuous_variables[19], ylab= "Imdb Score", xlim = c(0,300000)  )
plot(get(continuous_variables[20]), imdb_score, xlab= continuous_variables[20], ylab= "Imdb Score", xlim= c(0,350)  )


#linear regression and r squared

#extract r-squared
r_sq = rep(0, length(continuous_variables))
for(i in 1:length(continuous_variables)){
  r_sq[i] = summary(lm(imdb_score ~ as.numeric(get(continuous_variables[i]))))$r.squared
}
r_sq
plot(r_sq)
setNames(r_sq, continuous_variables )


#collinearity
quantvars = imdb[,c(5,6,7, 13, 15, 17, 19, 21, 23, 25, 27, 30, 32,33,34, 36,37,38, 52,53,54,55) ]
pairs.panels(quantvars)


#significant p-value
relev_var = continuous_variables[c(2,3,4,5,6,8,10,14,16,17,19,21,22)]
relev_var2 = continuous_variables[c(13,17,16,9,10,11,12,17,18)]

relev_var

r_sq = rep(0, length(relev_var))
for(i in 1:length(relev_var)){
  r_sq[i] = summary(lm(imdb_score ~ as.numeric(get(relev_var[i]))))$r.squared
}
r_sq
plot(r_sq)
setNames(r_sq, relev_var )

#num of votes
plot(number_of_votes, imdb_score, data=imdb2)

mreg = lm(imdb_score ~ poly(number_of_votes,1), data=imdb2)
mreg2 = lm(imdb_score ~ poly(number_of_votes,2), data=imdb2)
mreg3 = lm(imdb_score ~ poly(number_of_votes,3), data=imdb2)
mreg4 = lm(imdb_score ~ poly(number_of_votes,4), data=imdb2)
mreg5 = lm(imdb_score ~ poly(number_of_votes,5), data=imdb2)

lines(sort(imdb2$number_of_votes), predict(mreg)[order(imdb2$number_of_votes)], col="red")
lines(sort(imdb2$number_of_votes), predict(mreg2)[order(imdb2$number_of_votes)], col="red")
lines(sort(imdb2$number_of_votes), predict(mreg3)[order(imdb2$number_of_votes)], col="red")
lines(sort(imdb2$number_of_votes), predict(mreg4)[order(imdb2$number_of_votes)], col="red")

summary(mreg3)

#duration
plot(duration_mins, imdb_score, data=imdb2)

mreg = lm(imdb_score ~ poly(duration_mins,1), data=imdb2)
mreg2 = lm(imdb_score ~ poly(duration_mins,2), data=imdb2)
mreg3 = lm(imdb_score ~ poly(duration_mins,3), data=imdb2)
mreg4 = lm(imdb_score ~ poly(duration_mins,4), data=imdb2)
mreg5 = lm(imdb_score ~ poly(duration_mins,5), data=imdb2)

lines(sort(imdb2$duration_mins), predict(mreg)[order(imdb2$duration_mins)], col="red")
lines(sort(imdb2$duration_mins), predict(mreg2)[order(imdb2$duration_mins)], col="red")
lines(sort(imdb2$duration_mins), predict(mreg3)[order(imdb2$duration_mins)], col="red")
lines(sort(imdb2$duration_mins), predict(mreg4)[order(imdb2$duration_mins)], col="red")

summary(mreg2)

#number of new articles number_news_articles

plot(number_news_articles, imdb_score, data=imdb2, xlim=c(0,15000))

mreg = lm(imdb_score ~ poly(number_news_articles,1), data=imdb2)
mreg2 = lm(imdb_score ~ poly(number_news_articles,2), data=imdb2)
mreg3 = lm(imdb_score ~ poly(number_news_articles,3), data=imdb2)
mreg4 = lm(imdb_score ~ poly(number_news_articles,4), data=imdb2)
mreg5 = lm(imdb_score ~ poly(number_news_articles,5), data=imdb2)

lines(sort(imdb2$number_news_articles), predict(mreg)[order(imdb2$number_news_articles)], col="red")
lines(sort(imdb2$number_news_articles), predict(mreg2)[order(imdb2$number_news_articles)], col="red")
lines(sort(imdb2$number_news_articles), predict(mreg3)[order(imdb2$number_news_articles)], col="red")
lines(sort(imdb2$number_news_articles), predict(mreg4)[order(imdb2$number_news_articles)], col="red")

summary(mreg5)


#director_facebook_likes
plot(director_facebook_likes, imdb_score, data=imdb2 )

mreg = lm(imdb_score ~ poly(director_facebook_likes,1), data=imdb2)
mreg2 = lm(imdb_score ~ poly(director_facebook_likes,2), data=imdb2)
mreg3 = lm(imdb_score ~ poly(director_facebook_likes,3), data=imdb2)
mreg4 = lm(imdb_score ~ poly(director_facebook_likes,4), data=imdb2)
mreg5 = lm(imdb_score ~ poly(director_facebook_likes,5), data=imdb2)

lines(sort(imdb2$director_facebook_likes), predict(mreg)[order(imdb2$director_facebook_likes)], col="red" )
lines(sort(imdb2$director_facebook_likes), predict(mreg2)[order(imdb2$director_facebook_likes)], col="red")
lines(sort(imdb2$director_facebook_likes), predict(mreg3)[order(imdb2$director_facebook_likes)], col="red")
lines(sort(imdb2$director_facebook_likes), predict(mreg4)[order(imdb2$director_facebook_likes)], col="red")

summary(mreg2)

#Movie budget
plot(movie_budget, imdb_score, data=imdb2, xlim=c(0,20000000))

mreg = lm(imdb_score ~ poly(movie_budget,1), data=imdb2)
mreg2 = lm(imdb_score ~ poly(movie_budget,2), data=imdb2)
mreg3 = lm(imdb_score ~ poly(movie_budget,3), data=imdb2)
mreg4 = lm(imdb_score ~ poly(movie_budget,4), data=imdb2)
mreg5 = lm(imdb_score ~ poly(movie_budget,5), data=imdb2)

lines(sort(imdb2$movie_budget), predict(mreg)[order(imdb2$movie_budget)], col="red")
lines(sort(imdb2$movie_budget), predict(mreg2)[order(imdb2$movie_budget)], col="red")
lines(sort(imdb2$movie_budget), predict(mreg3)[order(imdb2$movie_budget)], col="red")
lines(sort(imdb2$movie_budget), predict(mreg4)[order(imdb2$movie_budget)], col="red")

summary(mreg)

#define categorical variables
cat = names(imdb)[c(4,5,6,8,9,10,11,12,14,16,20,24,29,31,39,40,41,42,43,44,45,46,47,48,49,50,51,56,57) ]
length(cat)
cat

#r-squared for cat
r_cat = rep(0, length(cat))
for(i in 1:length(cat)){
  r_cat[i] = summary(lm(imdb_score ~ as.factor(get(relev_var[i]))))$r.squared
}
r_cat
plot(r_cat)
setNames(r_cat, cat )



#testing poly model:
imdb2 = na.omit(imdb)

ureg = lm(imdb_score ~ poly(imdb2$number_of_votes,3) + poly(imdb2$duration_mins,2)+ imdb2$distributor + imdb2$director + imdb2$actor_1_name + imdb2$actor_2_name  , data=imdb2)
summary(ureg)

ureg1 = lm(imdb_score ~ poly(imdb2$number_of_votes,3) + poly(imdb2$duration_mins,2)  , data=imdb2)
summary(ureg1)

ureg2 = lm(imdb_score ~ poly(imdb2$number_of_votes,3) + poly(imdb2$duration_mins,2)+ poly(as.numeric(imdb2$release_year), 2) + action + adventure + scifi + thriller  + romance + horror + drama  + animation , data=imdb2)
summary(ureg2)

kglm.fit = glm(imdb2$imdb_score ~ + poly(imdb2$duration_mins,2) + imdb2$director , data=imdb2)
cv.glm(imdb2, kglm.fit, K=30)$delta[1]

kglm.fit = glm(imdb2$imdb_score ~  poly(imdb2$duration_mins,2) + imdb2$action + imdb2$adventure  + imdb2$thriller  + imdb2$romance + imdb2$horror + imdb2$drama  + imdb2$animation , data=imdb2)
cv.glm(imdb2, kglm.fit, K=30)$delta[1]

#Variables number

relev_var2 = continuous_variables[c(13,17,16,9,10,11,12,18)]
relev_var2

r_sq = rep(0, length(relev_var2))
for(i in 1:length(relev_var2)){
  r_sq[i] = summary(lm(imdb_score ~ as.numeric(get(relev_var2[i]))))$r.squared
}
r_sq
plot(r_sq)
setNames(r_sq, relev_var2 )

par(mfrow=c(1,1))

#user votes number
plot(as.numeric(user_votes_number), imdb_score, xlab = "user_votes_number", ylab= "Imdb Score", col="gray" )
reg1 =  lm(imdb_score ~ as.numeric(user_votes_number), data=imdb)
abline(reg1)
summary(reg1)
outlierTest(reg1)

residualPlots(reg1)

#not significant enough

#movie budget
plot(as.numeric(movie_budget), imdb_score, xlab = "movie_budget", ylab= "Imdb Score" , col="gray" )
reg2 =  lm(imdb_score ~ as.numeric(movie_budget), data=imdb)
abline(reg2)
summary(reg2)
outlierTest(reg2)

residualPlots(reg2)

#not significant

#user reviews number
plot(as.numeric(user_reviews_number), imdb_score, xlab = "user_reviews_number", ylab= "Imdb Score", col="gray")
reg3 =  lm(imdb_score ~ as.numeric(user_reviews_number), data=imdb)
abline(reg3)
summary(reg3)
outlierTest(reg3)

residualPlots(reg3)
#not significant

#actor 2 star meter
plot(as.numeric(actor_2_star_meter), imdb_score, xlab = "actor_2_star_meter", ylab= "Imdb Score" , col="gray")
reg4 =  lm(imdb_score ~ as.numeric(actor_2_star_meter), data=imdb)
abline(reg4)
summary(reg4)
outlierTest(reg4)

residualPlots(reg4)

#not significant

#actor 3 facebook likes
plot(as.numeric(actor_3_facebook_likes), imdb_score, xlab = "actor_3_facebook_likes", ylab= "Imdb Score" )
reg5 =  lm(imdb_score ~ as.numeric(actor_3_facebook_likes), data=imdb)
abline(reg5, col="red")
summary(reg5)
outlierTest(reg5)

mreg =  lm(imdb_score ~ poly(as.numeric(actor_3_facebook_likes), 3), data=imdb)
summary(mreg)


residualPlots(reg5)
#not significant


#actor 3 star meter
plot(as.numeric(actor_3_star_meter), imdb_score, xlab = "actor_3_star_meter", ylab= "Imdb Score" )
reg6 =  lm(imdb_score ~ as.numeric(actor_3_star_meter), data=imdb)
abline(reg6, col="red")
summary(reg6)
outlierTest(reg6)
residualPlots(reg6)
#not significant


#critic review number
plot(as.numeric(critic_reviews_number), imdb_score, xlab = "critic_reviews_number", ylab= "Imdb Score" )
reg7 =  lm(imdb_score ~ as.numeric(critic_reviews_number), data=imdb)
abline(reg7, col="red")
summary(reg7)
outlierTest(reg7)

mreg =  lm(imdb_score ~ poly(as.numeric(critic_reviews_number), 2), data=imdb2)
summary(mreg)

residualPlots(reg7)
#not significant


#movie facebook likes
plot(as.numeric(movie_facebook_likes), imdb_score, xlab = "movie_facebook_likes", ylab= "Imdb Score" , xlim=c(0,100000))
reg8 =  lm(imdb_score ~ as.numeric(movie_facebook_likes), data=imdb2)
abline(reg8, col="red")
summary(reg8)
outlierTest(reg8)

mreg =  lm(imdb_score ~ poly(as.numeric(movie_facebook_likes), 3), data=imdb2)
summary(mreg)

residualPlots(reg8)
#significant


#director facebook likes
plot(as.numeric(director_facebook_likes), imdb_score, xlab = "director_facebook_likes", ylab= "Imdb Score")
reg9 =  lm(imdb_score ~ as.numeric(director_facebook_likes), data=imdb2)
abline(reg9, col="red")
summary(reg9)
outlierTest(reg9)

mreg =  lm(imdb_score ~ poly(as.numeric(director_facebook_likes), 4), data=imdb2)
summary(mreg)

residualPlots(reg9)

#release year
plot(as.numeric(release_year), imdb_score, xlab = "release_year", ylab= "Imdb Score")
reg10 =  lm(imdb_score ~ as.numeric(release_year), data=imdb2)
abline(reg10, col="red")
summary(reg10)
outlierTest(reg10)

mreg =  lm(imdb_score ~ poly(as.numeric(release_year), 2), data=imdb2)
summary(mreg)

residualPlots(reg10)

#release_day
plot(as.numeric(release_day), imdb_score, xlab = "release_day", ylab= "Imdb Score")
reg10 =  lm(imdb_score ~ as.numeric(release_day), data=imdb2)
abline(reg10, col="red")
summary(reg10)
outlierTest(reg10)

mreg =  lm(imdb_score ~ poly(as.numeric(release_day), 2), data=imdb2)
summary(mreg)

residualPlots(reg10)

#content rating
plot(as.numeric(content_rating), imdb_score, xlab = "content_rating", ylab= "Imdb Score")
reg11 =  lm(imdb_score ~ as.numeric(content_rating), data=imdb2)
abline(reg11, col="red")
summary(reg11)
outlierTest(reg11)

mreg =  lm(imdb_score ~ poly(as.numeric(content_rating), 3), data=imdb2)
summary(mreg)

residualPlots(reg11)

#number of faces
plot(as.numeric(number_of_faces_in_movie_poster), imdb_score, xlab = "number_of_faces_in_movie_poster", ylab= "Imdb Score")
reg12 =  lm(imdb_score ~ as.numeric(number_of_faces_in_movie_poster), data=imdb2)
abline(reg12, col="red")
summary(reg12)
outlierTest(reg12)

mreg =  lm(imdb_score ~ poly(as.numeric(number_of_faces_in_movie_poster), 2), data=imdb2)
summary(mreg)

residualPlots(reg11)

#type
action = as.factor(action)
adventure = as.factor(adventure)
scifi = as.factor(scifi)
thriller = as.factor(thriller)
musical = as.factor(musical)
romance = as.factor(romance)
sport = as.factor(sport)
western = as.factor(western)
horror = as.factor(horror)
drama = as.factor(drama)
war = as.factor(war)
animation = as.factor(animation)
crime = as.factor(crime)



mreg = lm(imdb_score ~ action + adventure + scifi + thriller + musical + romance + sport + western + horror + drama + war + animation + crime )
summary(mreg)
kglm.fit = glm(imdb_score ~ action + adventure + scifi + thriller + musical + romance + sport + western + horror + drama + war + animation + crime )
cv.glm(imdb2, kglm.fit, K=30)$delta[1]



#content rating
plot(content_rating, imdb_score)
rem = lm(imdb_score ~ content_rating)
summary(rem)