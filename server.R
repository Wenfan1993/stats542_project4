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

# read ratings
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

top10_movies_genre <- function() {
  return(list("Action" = c(260, 1196, 1210, 480, 2028, 589, 2571, 1580, 1198, 110),
              "Adventure" = c(260, 1196, 1210, 480, 1580, 1198, 1197, 2916, 2987, 1259),
              "Animation" = c(1, 2987, 2355, 3114, 588, 3751, 2700, 364, 595, 2081),
              "Children's" = c(1097, 1, 34, 919, 2355, 3114, 588, 3751, 1073, 364),
              "Comedy" = c(2858, 1270, 1580, 2396, 1197, 1265, 2997, 356, 2716, 1),
              "Crime" = c(608, 1617, 858, 296, 50, 1221, 1213, 2000, 592, 1089),
              "Documentary" = c(2064, 246, 162, 3007, 1147, 1189, 2859, 2677, 2693, 1649),
              "Drama" = c(2858, 1196, 2028, 593, 608, 110, 527, 1097, 318, 858),
              "Fantasy" = c(260, 1097, 2174, 2797, 1073, 2968, 2872, 2140, 2161, 2087),
              "Film-Noir" = c(1617, 541, 2987, 1252, 913, 1179, 1748, 1267, 3683, 3435),
              "Horror" = c(2716, 1214, 1387, 1219, 1278, 1258, 3081, 1215, 1407, 1997),
              "Musical" = c(919, 588, 1220, 364, 1288, 595, 2081, 1028, 551, 1282),
              "Mystery" = c(1617, 924, 3176, 1252, 1732, 904, 913, 903, 1625, 3386),
              "Romance" = c(1210, 2396, 1197, 1265, 356, 912, 377, 1307, 1721, 2291),
              "Sci-Fi" = c(260, 1196, 1210, 480, 589, 2571, 1270, 1580, 1097, 1240),
              "Thriller" = c(589, 2571, 593, 608, 2762, 1617, 1240, 1214, 2916, 457),
              "War" = c(1196, 1210, 2028, 110, 527, 356, 1200, 780, 912, 750),
              "Western" = c(590, 1304, 3671, 1266, 1201, 368, 3037, 553, 2951, 1283)
              ))
}

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
  df <- eventReactive(input$btn2, {
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
      pred = as(recom, 'matrix')
      pred_df = data.frame(pred)
      pred_df[is.na(pred_df)]=0
      pred = as.matrix(pred_df)
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

  ############### Genre recommendations  #################
  # Calculate recommendations when the sbumbutton is clicked
  genre_df <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)

      user_results = (1:10)/10

      user_genre = input$movieGenre
      genre_recomms = top10_movies_genre()
      print(genre_recomms)
      user_predicted_ids = genre_recomms[[user_genre]]
      recom_results <- data.table(Rank = 1:10,
                                  MovieID = user_predicted_ids,
                                  Title = user_predicted_ids,
                                  Predicted_rating =  user_results)
    }) # still busy

  }) # clicked on button


  # display the recommendations
  output$genres <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- genre_df()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            div(style = "text-align:center",
                a(img(src = movies$image_url[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])], height = 150))
            ),
            div(style="text-align:center; font-size: 100%",
                strong(movies$Title[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])])
            )

        )
      }))) # columns
    }) # rows

  }) # renderUI function


}) # server function