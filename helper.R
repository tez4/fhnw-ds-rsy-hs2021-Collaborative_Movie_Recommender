# ------------------- Explorative Datenanalyse ---------------------------


data_reduction_dense <- function(ratingMatrix) {
  # convert into df
  data <- as(ratingMatrix, "data.frame")
  
  # get the 400 users with most ratings
  counts <- data %>% group_by(user) %>% count() %>% arrange(desc(n), user) %>% head(400)
  data <- inner_join(counts, data, by="user")
  data <- data %>% select(user, item, rating) %>% ungroup
  data <- as.data.frame(data)
  
  # get the 700 Movies with most ratings
  counts <- data %>% group_by(item) %>% count() %>% arrange(desc(n), item) %>% head(700)
  data <- inner_join(counts, data, by="item")
  data <- data %>% select(user, item, rating) %>% ungroup
  data <- as.data.frame(data)
  
  # convert back into realRatingMatrix
  ratingMatrix <- as(data, "realRatingMatrix")
  
  return (ratingMatrix)
}


show_coverage <- function(listOfDifferentN, recommender) {
  
  listOfCoverages <- vector()
  for (N in listOfDifferentN) {
    
    # predict top N movies
    pre <- predict(rec, train, n = N)
    reco_list <- as(pre, "list")
    recommendations <- as.data.frame(reco_list)
    all_recommendations <- list()
    
    # find true positives and false positives for all users and add them up
    for (i in colnames(recommendations)) {
      all_recommendations <- append(all_recommendations, dplyr::pull(recommendations[i]))
    }
    
    
    listOfCoverages <- c(listOfCoverages, round(length(unique(all_recommendations)) / dim(train)[2], digits = 4))
  }
  return (data.frame(N = listOfDifferentN, coverage = listOfCoverages))
}


show_novelty <- function(listOfDifferentN, recommender) {
  # create a dataset wit calculated popularity for every movie
  popularity <- as(MovieLense, "data.frame")
  popularity <- popularity %>%
    group_by(item) %>% 
    summarize(ratings = n() / dim(MovieLense)[2]) %>% 
    mutate(ratings = log2(ratings))
  
  listOfNovelties <- vector()
  for (N in listOfDifferentN) {
    # get top-N-list for a certain N
    pre <- predict(rec, train, n = N)
    reco_list <- as(pre, "list")
    recommendations <- as.data.frame(reco_list)
    
    
    total_novelty <- vector()
    for (i in colnames(recommendations)) {
      # calculate mean popularity of recommended items for user
      reco <- recommendations[i]
      colnames(reco)[1] <- "item"
      reco <- inner_join(popularity, reco, by = 'item')
      novelty <- mean(as.numeric(reco$ratings))
      total_novelty <- c(total_novelty, novelty)
    }
    listOfNovelties <- c(listOfNovelties, 0 - mean(total_novelty))
  }
  return (data.frame(N = listOfDifferentN, novelty = listOfNovelties))
}


show_precision <- function(listOfDifferentN, ratingMatrix, threshold) {
  
  # normalize the rating matrix
  ratingMatrix <- normalize(ratingMatrix, method="Z-score", row=TRUE)
  
  # create a training set and a test set with true positives for recall and precision
  data <- as(ratingMatrix, "data.frame")
  relevant <- data %>% group_by(user) %>% sample_n(30)
  true_positives <- relevant %>% filter(rating >= threshold)
  false_positives <- relevant %>% filter(rating < threshold)
  
  # remove testing observations from training set
  train <- anti_join(data, relevant,by=c('user','item'))
  train <- as(train, 'realRatingMatrix')
  
  # train model based on training set
  rec <- Recommender(train, method = "IBCF", param=list(method="Cosine", k=30, normalize = NULL, na_as_zero = TRUE)) #normalize = 'center', 'Z-score'
  
  for (N in listOfDifferentN) {
    
    # predict top N movies
    pre <- predict(rec, train, n = N)
    reco_list <- as(pre, "list")
    recommendations <- as.data.frame(reco_list)
    
    # find true positives and false positives for all users and add them up
    true_total <- 0
    false_total <- 0
    for (i in as.list(unique(true_positives['user']))$user) {
      our_user <- paste('X', i, sep = '')
      recommendations['item'] <- recommendations[our_user]
      
      true_total <- true_total + nrow(inner_join(recommendations['item'], true_positives %>% filter(user == as.integer(i)), by = 'item'))
      false_total <- false_total + nrow(inner_join(recommendations['item'], false_positives %>% filter(user == as.integer(i)), by = 'item'))
    }
    
    # print Summary
    print(paste('N =', N))
    print(paste('Number of True Positives:',true_total))
    print(paste('Number of False Positives:',false_total))
    print(paste('Precision:',true_total / (true_total + false_total)))
    print('')
  }
}


get_sparsity <- function(Matrix) {
  round(( 1 - (nratings(Matrix) / (dim(Matrix)[1] * dim(Matrix)[2]))) * 100,2)
}


show_sparsity <- function(Matrix, Name) {
  Measurement <- list('Matrix','Dimension', 'Sparsity', 'Density')
  Value <- list(Name, paste('(',toString(dim(Matrix)), ')'),paste(get_sparsity(Matrix), '%' ), paste(100 - get_sparsity(Matrix), '%' ))
  df <- cbind(Measurement,Value)
  head(df)
}


show_sparsity_change <- function(oldMatrix, newMatrix) {
  print(list(show_sparsity(oldMatrix, 'Old Matrix'), show_sparsity(newMatrix, 'New Matrix')))
}


show_change_of_rating_distribution <- function(oldRatingMatrix, newRatingMatrix) {
  old_matrix <- as(oldRatingMatrix, "data.frame") %>% 
    group_by(item) %>%  
    summarize(
      mean_rating = mean(rating),
      ratings = n()
    ) %>% 
    mutate(
      matrix = 'a) alte Matrix'
    )
  
  new_matrix <- as(newRatingMatrix, "data.frame") %>% 
    group_by(item) %>%  
    summarize(
      mean_rating = mean(rating),
      ratings = n()
    ) %>% 
    mutate(
      matrix = 'b) neue Matrix'
    )
  
  comparison <- bind_rows(old_matrix, new_matrix)
  
  ggplot(comparison, aes(x = mean_rating, fill = matrix)) +
    geom_density(alpha = 0.5, bw = 0.08) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    labs(
      title = "Verteilung mittlere Kundenratings pro Film",
      subtitle = "N = 1664 Filme",
      x = "Durchschnittliche Bewertung", 
      y = "Dichte",
      fill = element_blank()
    ) +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      legend.position = c(.90, .95)
    )
}


split_dataset <- function(ratingMatrix, trainSize) {
  # train-test split
  set.seed(42)
  data <- as(ratingMatrix, "data.frame")
  df <- data %>% group_by(user) %>% summarize(mean_rating = mean(rating))
  
  df <- sample_frac(df, size = trainSize, replace = FALSE)
  df_train <- semi_join(data,df,by='user')
  df_test <- anti_join(data,df_train,by='user')
  train <- as(df_train, "realRatingMatrix")
  test <- as(df_test, 'realRatingMatrix')
  
  return(list(train, test))
}

# -----------------------Implementierung Top-N Monitor----------------------------------
  
create_df_user_genres_top_n <- function(recommender, df_genres) {
  # create data frame with genres
  df_genres <- df_genres %>% group_by(item, genres) %>% summarise(mean(rating))
  
  # make predictions and create list with them
  pre <- predict(recommender, test, n = 20)
  reco_list <- as(pre, "list")
  
  # create data frame of top n recommendations
  df_recommendations <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("user", "item"))))
  
  for (i in 1:length(reco_list)) {
    for (j in 1:length(reco_list[[i]])) {
      df_recommendations[nrow(df_recommendations) + 1,] = c(names(reco_list[i]),reco_list[[i]][j])
    }
  }
  
  # get group size of recommended genres of all 80 users
  df_user_genres <- inner_join(df_recommendations, df_genres, by = 'item') %>% group_by(user, genres) %>% summarise(count_top_n = n())
  
  # select our users
  set.seed(42)
  users <- df_user_genres %>% group_by(user) %>% summarise(x = sum(count_top_n)) %>% sample_n(20) %>% select(user)
  df_user_genres <- inner_join(df_user_genres, users, by = 'user')
  df_user_genres <- df_user_genres %>% arrange('user')
  df_user_genres_top_n <- df_user_genres
  total <- df_user_genres_top_n %>% group_by(user) %>% summarise(total = sum(count_top_n))
  df_user_genres_top_n <- left_join(df_user_genres_top_n, total, by = 'user') %>% mutate(count_top_n = count_top_n / total * 100) %>% select(-total)
  
  return (df_user_genres_top_n)
}


create_df_user_genres_best <- function(movies, df_user_genres_top_n, df_genres) {
  users <- df_user_genres_top_n %>% group_by(user) %>% summarise(x = sum(count_top_n)) %>% select(user)
  df_genres <- df_genres %>% group_by(item, genres) %>% summarise(mean(rating))
  df_all <- inner_join(movies, users, by = 'user')
  df_best <- df_all %>% group_by(user) %>% summarise(mean_rating = mean(rating))
  df_best <- inner_join(df_all, df_best, by = 'user')
  df_best <- df_best %>% filter(rating > mean_rating + 0.5)
  df_user_genres <- inner_join(df_best, df_genres, by = 'item') %>% group_by(user, genres) %>% summarise(count_best = n())
  df_user_genres_best <- df_user_genres
  total <- df_user_genres_best %>% group_by(user) %>% summarise(total = sum(count_best))
  df_user_genres_best <- left_join(df_user_genres_best, total, by = 'user') %>% mutate(count_best = count_best / total * 100) %>% select(-total)
  
  return(df_user_genres_best)
}


create_df_user_genres <- function(df_user_genres_top_n, df_user_genres_best) {
  df_user_genres <- full_join(df_user_genres_top_n, df_user_genres_best, by = c('user','genres'))
  df_user_genres <- df_user_genres %>% replace(is.na(.), 0)
  df_user_genres <- gather(df_user_genres, list, count, c(count_top_n, count_best))
  df_user_genres <- df_user_genres %>% group_by(genres, list) %>% summarise(count  = mean(count))
  
  return(df_user_genres)
}

show_genre_fraction_plot <- function(df, x_variable, title) {
  # create plot
  colourCount = length(unique(df$genres))
  getPalette = colorRampPalette(brewer.pal(9, "Paired"))
  
  ggplot(data=df, aes(y=user, x=x_variable, fill=genres)) +
    geom_col(position = 'fill') +
    scale_y_discrete(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    labs(
      title = title,
      x = "Anteil",
      y = "Kundennummer",
      fill = element_blank()
    ) +
    scale_fill_manual(values = getPalette(colourCount)) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          text = element_text(size = 12) # text size
    )
}

show_cleveland_dot_plot <- function(df, subtitle) {
  ggplot(df, aes(genres, count), height = 500, width = 7) +
    scale_color_discrete(labels = c("bestbewertete Filme", "Top-N Empfehlungen")) +
    coord_flip() +
    geom_line() +
    geom_point(aes(color = list)) +
    theme_minimal() +
    labs(
      title = "Anteil Genres der bestbewerteten Filme im Vergleich zu \nden Top-N Empfehlungen der 20 Nutzer",
      subtitle = subtitle,
      x = element_blank(),
      y = "Anteil in Prozent",
      color = element_blank(),
    ) +
    theme(
      text = element_text(size = 12),
      legend.position = 'bottom'
    )
}
