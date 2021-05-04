library(recommenderlab)
library(Matrix)


#function added for prediction
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL

set.seed(100)
train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
train = ratings[train.id, ]
test = ratings[-train.id, ]

i = paste0('u', train$UserID)
j = paste0('m', train$MovieID)
x = train$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 25))
print('model generated')
print(rec_UBCF)

recom = predict(rec_UBCF, 
                Rmat, type = 'ratings')  

rec_list = as(recom, 'list')  # each element are ratings of that user

test.pred = test
test.pred$rating = NA

# For all lines in test file, one by one
for (u in 1:nrow(test)){
  
  # Read userid and movieid from columns 2 and 3 of test data
  userid = as.character(test$user[u])
  movieid = as.character(test$movie[u])
  
  rating = rec_list[[userid]][movieid]
  # handle missing values; 2.5 might not be the ideal choice
  test.pred$rating[u] = ifelse(is.na(rating), 2.5, rating)
}

# Calculate RMSE
sqrt(mean((test$rating - test.pred$rating)^2))