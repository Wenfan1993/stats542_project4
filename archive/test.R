library(recommenderlab)
library(Matrix)


calculateRecon = function (){
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

recom = predict(rec_UBCF, 
                Rmat[1:3], type = 'ratings')


#prediction
user_ratings <- data.frame(MovieID   = c(4,2,3,1),
                  Rating = c(3,2,2,3))

df$UserID = 6041
train_new = rbind(train, df)
i = paste0('u', train_new$UserID)
j = paste0('m', train_new$MovieID)
x = train_new$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

recom = predict(rec_UBCF, 
                Rmat[c('u6041'),]  , type = 'ratings')

pred = as(recom, 'matrix')
pred_sort = sort(pred[,!is.na(pred)], decreasing =TRUE)
recon_movie_id = names(pred3)[1:10]
user_predicted_id = c()
for (i in 1:10){
  movie_id = recon_movie_id[i]
  user_predicted_id[i] = strtoi(substr(movie_id,2,nchar(movie_id)))
}

user_predicted_id
}



recom_results <- data.table(Rank = 1:10, 
                            MovieID = movies$MovieID[user_predicted_ids], 
                            Title = movies$Title[user_predicted_ids], 
                            Predicted_rating =  pred_sort[1:10])