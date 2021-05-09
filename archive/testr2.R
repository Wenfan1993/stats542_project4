library(recommenderlab)
library(Matrix)
library(magrittr)
library(dplyr) 
#function added for prediction
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL

overallRMSE_Item = c()


#for (i_iter in 1:1){
i_iter = 1
set.seed(i_iter * 100)
train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
train = ratings[train.id, ]
test = ratings[-train.id, ]

i = paste0('u', train$UserID)
j = paste0('m', train$MovieID)

#i = paste0('m', train$MovieID)
#j = paste0('u', train$UserID)


x = train$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)
#start from here====
rec_IBCF = Recommender(Rmat, method = 'IBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        k = 25
                                        ))

#print('model generated')
#print(rec_IBCF@model)

recom = predict(rec_IBCF, 
                Rmat, type = 'ratings')  

rec_list = as(recom, 'list')  # each element are ratings of that user

test.pred = test
test.pred$Rating = NA

#test_rating_by_user = test %>% group_by(UserID)
#test_rating_by_user_df = test_rating_by_user %>% summarise(
#  Rating = mean(Rating)

test_rating_by_movie = test %>% group_by(MovieID)
test_rating_by_movie_df = test_rating_by_movie %>% summarise(
  Rating = mean(Rating)
)
default = 0
# For all lines in test file, one by one
for (u in 1:nrow(test)){
  # Read userid and movieid from columns 2 and 3 of test data
  userid = as.character(test$UserID[u])
  movieid = as.character(test$MovieID[u])
  
  rating = rec_list[[paste0('u',userid)]][paste0('m',movieid)]
  if (is.na(rating)){
    default = default +1
  }
  # handle missing values; 2.5 might not be the ideal choice
  replace_na_value = test_rating_by_movie_df[test_rating_by_movie_df$MovieID== movieid,2]$Rating
  replace_na_value = ifelse(length(replace_na_value)==0, 2.5, replace_na_value)
  test.pred$Rating[u] = ifelse(is.na(rating), 2.5, rating)
}

# Calculate RMSE
RMSE = sqrt(mean((test$Rating - test.pred$Rating)^2))
overallRMSE_Item[i_iter] = RMSE
#}


library(recommenderlab)
library(Matrix)
library(magrittr)
library(dplyr) 
library(SimilarityMeasures)
library(Matrix)
a = -2
max(min(a,5),0)
a = data.frame(wenxi = c(1,2,1,1,1,1),
               fan =c(100,200,1000,500,100,200))
x <- matrix(1:18, ncol = 3)
Scale(x, type = "normalize") 
normalize(Rmat)

pred = as(recom, 'matrix')
pred_df = data.frame(pred)
pred_df[is.na(pred_df)]=0
pred = as.matrix(pred_df)
typeof(pred)
)data.frame(pred)
