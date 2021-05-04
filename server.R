## server.R
library(recommenderlab)
library(Matrix)
# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)

      #function added for prediction
      myurl = "https://liangfgithub.github.io/MovieData/"
      ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                         sep = ':',
                         colClasses = c('integer', 'NULL'), 
                         header = FALSE)
      colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
      ratings$Timestamp = NULL
      
      set.seed(100)
      #train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
      train = ratings#[train.id, ]
      #test = ratings[-train.id, ]
      
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

      #prediction
      user_ratings$UserID = 6041
      train_new = rbind(train, user_ratings)
      newi = paste0('u', train_new$UserID)
      newj = paste0('m', train_new$MovieID)
      newx = train_new$Rating
      newtmp = data.frame(newi, newj, newx, stringsAsFactors = T)
      colnames(newtmp) = c('i','j','x')
      newRmat = sparseMatrix(as.integer(newtmp$i), as.integer(newtmp$j), x = newtmp$x)
      rownames(newRmat) = levels(newtmp$i)
      colnames(newRmat) = levels(newtmp$j)
      newRmat = new('realRatingMatrix', data = newRmat)
      
      recom = predict(rec_UBCF, 
                      newRmat[c('u6041'),]  , type = 'ratings')
      
      pred = as(recom, 'matrix')
      pred_sort = sort(pred[,!is.na(pred)], decreasing =TRUE)
      recon_movie_id = names(pred_sort)[1:10]
      user_predicted_ids = c()
      for (i in 1:10){
        movie_id = recon_movie_id[i]
        user_predicted_ids[i] = strtoi(substr(movie_id,2,nchar(movie_id)))
      }
      
      #end of function added for prediction
      
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  pred_sort[1:10])
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function