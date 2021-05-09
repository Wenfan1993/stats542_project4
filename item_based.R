library(recommenderlab)
library(Matrix)
library(magrittr)
library(dplyr) 
library(SimilarityMeasures)

myurl = "https://liangfgithub.github.io/MovieData/"

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

overallRMSE_User = c()

for (i_iter in 1:1){
  set.seed(i_iter * 100)
#set up parameters
  p <- list(
    k = 25,
    method="Cosine",
    normalize = "center",
    normalize_sim_matrix = FALSE,
    alpha = 0.5,
    na_as_zero = FALSE
  )
  
  #construct the sparsematrix
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
  data = new('realRatingMatrix', data = Rmat)
  
  #normalize
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
  
  # calculate similarity
  sim <- as.matrix(similarity(data, method=p$method, which="items",
                              args=list(alpha=p$alpha, na_as_zero=p$na_as_zero)))
  
  ## normalize rows to 1
  if(p$normalize_sim_matrix) sim <- sim/rowSums(sim, na.rm=TRUE)
  
  ## reduce similarity matrix to keep only the k highest similarities
  diag(sim) <- NA
  ##sim[!is.finite(sim)] <- NA
  
  for(i in 1:nrow(sim))
    sim[i,head(order(sim[i,], decreasing=FALSE, na.last=FALSE),
               ncol(sim) - p$k)] <- NA
  
  ## make sparse
  sim <- dropNA(sim)
  
  model <- c(list(
    description = "IBCF: Reduced similarity matrix",
    sim = sim
  ), p
  )
  
  predict <- function(model, newdata, n = 25,
                      data=NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {
    
    type <- match.arg(type)
    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }
    
    if(ncol(newdata) != nrow(model$sim)) stop("number of items in newdata does not match model.")
    
    n <- as.integer(n)
    
    if(!is.null(model$normalize))
      newdata <- normalize(newdata, method=model$normalize)
    
    ## predict all ratings
    sim <- model$sim
    u <- as(newdata, "dgCMatrix")
    
    ratings <- t(as(tcrossprod(sim,u) / tcrossprod(sim, u!=0), "matrix"))
    
    ratings <- new("realRatingMatrix", data=dropNA(ratings),
                   normalize = getNormalize(newdata))
    ratings <- denormalize(ratings)
    
    returnRatings(ratings, newdata, type, n)
  }
  
  #compile test dataframe
  m <- matrix(0, ncol = dim(data)[2], nrow = length(unique(test$UserID)))
  tem_test = data.frame((m))
  colnames(tem_test) = colnames(data)
  rownames(tem_test) = paste0('u',unique(test$UserID))
  
  for (i in 1:dim(test)[1]){
    test_userid = paste0('u',test[i,]$UserID)
    test_movie =  paste0('m',test[i,]$MovieID)
    test_rate = test[i,]$Rating
    tem_test[test_userid,test_movie] = test_rate
  }
  
  Rmat_test = Matrix(as.matrix(tem_test), sparse = TRUE) 
  rownames(Rmat_test) = rownames(tem_test)
  colnames(Rmat_test) = colnames(tem_test)
  Rmat_test = new('realRatingMatrix', data = Rmat_test)
  Rmat_test_use = Rmat_test[, colnames(data)]
  newdata = Rmat_test_use
  recom  = predict(model, newdata, type="ratings")
  
  #evaluation  
  rec_list = as(recom, 'list')  # each element are ratings of that user
  test.pred = test
  test.pred$Rating = NA
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
    test.pred$Rating[u] = ifelse(is.na(rating), replace_na_value, rating)
  }
  
  # Calculate RMSE
  test.pred$Rating[is.na(test.pred$Rating)] <- 2.5
  RMSE = sqrt(mean((test$Rating - test.pred$Rating)^2))
  overallRMSE_Item[i_iter] = RMSE
}

