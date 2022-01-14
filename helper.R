# ------------------- Explorative Datenanalyse ---------------------------

# ------------------- Datenreduktion ---------------------------


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


data_reduction_random <- function(ratingMatrix) {
  set.seed(42)
  # convert into df
  data <- as(ratingMatrix, "data.frame")
  
  # get 400 random users
  counts <- data %>% group_by(user) %>% count() %>% arrange(desc(n), user) %>% ungroup() %>%  sample_n(400)
  data <- inner_join(counts, data, by="user")
  data <- data %>% select(user, item, rating) %>% ungroup
  data <- as.data.frame(data)
  
  # get 700 random movies
  counts <- data %>% group_by(item) %>% count() %>% arrange(desc(n), item) %>% ungroup() %>% sample_n(700)
  data <- inner_join(counts, data, by="item")
  data <- data %>% select(user, item, rating) %>% ungroup
  data <- as.data.frame(data)
  
  # convert back into realRatingMatrix
  ratingMatrix <- as(data, "realRatingMatrix")
  
  return (ratingMatrix)
}


data_reduction_dense_user <- function(ratingMatrix) {
  set.seed(42)
  # convert into df
  data <- as(ratingMatrix, "data.frame")
  
  # get the 400 users with most ratings
  counts <- data %>% group_by(user) %>% count() %>% arrange(desc(n), user) %>% ungroup() %>%  head(400)
  data <- inner_join(counts, data, by="user")
  data <- data %>% select(user, item, rating) %>% ungroup
  data <- as.data.frame(data)
  
  # get 700 Movies random movies
  counts <- data %>% group_by(item) %>% count() %>% arrange(desc(n), item) %>% ungroup() %>% sample_n(700)
  data <- inner_join(counts, data, by="item")
  data <- data %>% select(user, item, rating) %>% ungroup
  data <- as.data.frame(data)
  
  # convert back into realRatingMatrix
  ratingMatrix <- as(data, "realRatingMatrix")
  
  return (ratingMatrix)
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


show_change_of_rating_distribution <- function(oldRatingMatrix, newRatingMatrix, Subtitle) {
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
      subtitle = Subtitle,
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


# ------------------- Analyse Ähnlichkeitsmatrix ---------------------------


split_dataset <- function(ratingMatrix, trainSize) {
  # train-test split
  set.seed(42)
  data <- as(ratingMatrix, "data.frame")
  df <- sample_frac(unique(data$user), size = trainSize, replace = FALSE)
  
  df_train <- movies %>% filter(item %in% mov_sample)
  df_test <- anti_join(data,df_train,by='user')
  train <- as(df_train, "realRatingMatrix")
  test <- as(df_test, 'realRatingMatrix')
  
  return(list(train, test))
}


# ------------------- Analyse Top-N-Listen - IBCF vs UBCF ---------------------------

plot_most_occ_item <- function(topnlist_df, Subtitle) {
  
  ggplot(topnlist_df, aes(x = n_appearances, y = movies))+
    geom_col(alpha = 1, fill = 'steelblue')+
    scale_y_discrete(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    geom_text(aes(label=round(n_appearances,2)), hjust = 1.3, color = 'white') +
    labs(
      title = "Meist vorgeschlagene Filme",
      subtitle = Subtitle,
      y = element_blank(),
      x = "Anzahl Vorkommen in TopNListe"
    ) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          text = element_text(size = 12) # text size
    )
}

# ------------------- Analyse Top-N-Listen - Ratings ---------------------------

intersect_ibcf_ubcf <- function(dataset, datasetname) {
  
  #ibcf recommender with Cosine similarity
  rec_ibcf_cos <- Recommender(train, method = "IBCF", param=list(method="Cosine", 
                                                                 k=30, normalize = NULL, 
                                                                 na_as_zero = TRUE))
  topn_ibcf_cos <- predict(rec_ibcf_cos, test, n = 15)
  topn_ibcf_cos_list <- as(topn_ibcf_cos, "list")
  
  #ibcf recommender with Jaccard similarity
  rec_ibcf_jac <- Recommender(train, method = "IBCF", param=list(method="Jaccard", 
                                                                 k=30, normalize = NULL, 
                                                                 na_as_zero = TRUE))
  topn_ibcf_jac <- predict(rec_ibcf_jac, test, n = 15)
  topn_ibcf_jac_list <- as(topn_ibcf_jac, "list")
  
  #ubcf recommender with Cosine similarity
  rec_ubcf_cos <- Recommender(train, method = "UBCF", param=list(method="Cosine", normalize = NULL))
  topn_ubcf_cos <- predict(rec_ubcf_cos, test, n = 15)
  topn_ubcf_cos_list <- as(topn_ubcf_cos, "list")
  
  #ubcf recommender with Jaccard similarity
  rec_ubcf_jac <- Recommender(train, method = "UBCF", param=list(method="Jaccard", 
                                                                 normalize = NULL))
  topn_ubcf_jac <- predict(rec_ubcf_jac, test, n = 15)
  topn_ubcf_jac_list <- as(topn_ubcf_jac, "list")
  
  comparison <- data.frame('comparison' = character(), 'intersection' = numeric(), 
                           'dataset' = character())
  colnames(comparison) <- c('comparison', 'intersection', 'dataset')
  
  e <- evaluationScheme(dataset, method="split", train=0.8, k=1, given=0)
  train <- getData(e, "train")
  test <- getData(e, 'unknown')
  
  #intersect of top 15 recommendations IBCF vs UBCF cosine similarity
  comp_ord_cos<- 0
  for (n in 1:length(topn_ibcf_cos_list)) {
    intersection <- length(intersect(unlist(topn_ibcf_cos_list[n]), 
                                     unlist(topn_ubcf_cos_list[n]))) / 15
    comp_ord_cos <- comp_ord_cos + intersection
  }
  comp <- c('ibcf cos - ubcf cos', round(comp_ord_cos / length(topn_ibcf_cos_list), 2),
            as.character(datasetname))
  comparison[nrow(comparison) + 1,] = comp
  
  #intersect of top 15 recommendations IBCF vs UBCF Jaccard similarity
  comp_bin_jac<- 0
  for (n in 1:length(topn_ibcf_jac_list)) {
    intersection <- length(intersect(unlist(topn_ibcf_jac_list[n]), 
                                     unlist(topn_ubcf_jac_list[n]))) / 15
    comp_bin_jac <- comp_bin_jac + intersection
  }
  comp <- c('ibcf cos - ubcf jac', round(comp_bin_jac / length(topn_ibcf_jac_list), 2),
            as.character(datasetname))
  comparison[nrow(comparison) + 1,] = comp
  
  #intersect of top 15 recommendations UBCF Cosine Similarity vs UBCF Jaccard similarity
  comp_cos_jac<- 0
  for (n in 1:length(topn_ubcf_jac_list)) {
    intersection <- length(intersect(unlist(topn_ubcf_jac_list[n]), 
                                     unlist(topn_ubcf_cos_list[n]))) / 15
    comp_cos_jac <- comp_cos_jac + intersection
  }
  comp <- c('ubcf cos - ubcf jac', round(comp_cos_jac / length(topn_ubcf_jac_list), 2),
            as.character(datasetname))
  comparison[nrow(comparison) + 1,] = comp
  
  ggplot(comparison, aes(x = comparison, y = intersection)) +
    geom_col(alpha = 1, fill = 'steelblue') +
    #geom_text(aes(label=count), vjust=1.5, color = 'white') +
    scale_y_discrete(expand = c(0,0)) +
    labs(
      title = paste("Anteil Ueberschneidung der Recommender", as.character(datasetname)),
      # subtitle = paste("N = ", nrow(df3), " Filme"),
      x = "verglichene Recommender", 
      y = "Ueberschneidungsanteil"
    ) +
    theme_classic() +
    theme(text = element_text(size = 12),
          #axis.text.x = element_text(angle = 60, hjust = 1)
    )
}

# ------------------- Analyse Top-N-Listen - IBCF vs SVD ---------------------------




# ------------------- Wahl des optimalen Recommenders ---------------------------

evaluate_model <- function(data, sub)
{
  set.seed(123)
  # evaluation scheme with k=10 (10 fold cross validation) and a rating split of 5
  eval_k <- 15
  given_n <- 15
  good_rating <- 3
  
  #scheme
  scheme <- evaluationScheme(data, method = "cross-validation", k = 10, given = given_n, goodRating = good_rating)
  
  algorithms <- list(
    "popular items" = list(name="POPULAR", param=NULL),
    "UBCF cosine" = list(name="UBCF", param=list(method = "cosine")),
    "UBCF pearson" = list(name="UBCF", param=list(method = "pearson")),
    "IBCF cosine" = list(name="IBCF", param=list(k = eval_k, method = "cosine")),
    "IBCF jaccard" = list(name="IBCF", param=list(k = eval_k, method = "jaccard")),
    "IBCF pearson" = list(name="IBCF", param=list(k = eval_k, method = "pearson")),
    "SVD k=15" = list(name="SVD", param=list(k = eval_k)),
    "SVD k=40" = list(name="SVD", param=list(k = 40)),
    "SVD k=5" = list(name="SVD", param=list(k = 5)),
    "SVD k=2" = list(name="SVD", param=list(k = 2))
  )
  
  results <- evaluate(scheme, algorithms, type = "topNList", n=c(10, 15, 20, 25, 30))
  #results
  #plot(results, annotate=c(1,3), legend="bottomright")
  #plot(results, "prec/rec", annotate=3, legend="topleft")
  
  
  # extract results for own plot
  ns <- c(10, 15, 20, 25, 30)
  
  for (i in 1:length(ns)) 
  {
    rec <- Recommender(data, method = "IBCF", param=list(method="Cosine", k = ns[i], normalize = 'center', na_as_zero = TRUE))
    #recom <- predict(rec, test, n=1)
    
    # print(scheme)
    break
  }
  
  prrc <- data.frame(model = "", precision = 0, recall = 0, n = 0)
  
  for (i in 1:length(avg(results)))
  {
    model_result = avg(results)[[i]]
    #print(model_result)
    alg <- names(algorithms)[i]
    for (j in 1:length(model_result[, 'precision']))
    {
      pr <- model_result[, 'precision'][j]
      rc <- model_result[, 'recall'][j]
      n <- ns[j]
      prrc <- rbind(prrc, c(alg, pr, rc, n))
    }
  }
  
  prrc <- prrc[-1,]
  
  #dtype conversion
  for(i in 2:dim(prrc)[2])
  {
    prrc[,i] <- as.numeric(prrc[,i])
  }

  ggplot(data=prrc, aes(x=recall, y=precision, color=model)) + 
    geom_line() + 
    geom_text(aes(label=n), vjust=-.2, hjust=-.0, show.legend = FALSE) +
    labs(
      title = paste0("Precision vs Recall, goodRating=", good_rating),
      subtitle = sub,
      y = "Precision",
      x = "Recall"
    )
}

hyper_param_svd <- function(seq, data, sub)
{
  set.seed(123)
  
  # extract results for own plot
  ns <- c(10, 15, 20, 25, 30)
  
  eval_k <- 15
  given_n <- 15
  good_rating <- 3
  
  #scheme
  scheme <- evaluationScheme(data, method = "cross-validation", k = 10, given = given_n, goodRating = good_rating)
  
  algorithms <- list()
  for(i in seq)
  {
    name <- paste0("SVD k=", i)
    #algorithms <- c(algorithms, assign(name, list(list(name="SVD", param=list(k = i)))))
    algorithms[name] <- list(list(name="SVD", param=list(k = i)))
  }
  
  
  
  results <- evaluate(scheme, algorithms, type = "topNList", n=c(10, 15, 20, 25, 30))
  
  prrc <- data.frame(model = "", precision = 0, recall = 0, n = 0)
  
  for (i in 1:length(avg(results)))
  {
    model_result = avg(results)[[i]]
    #print(model_result)
    alg <- names(algorithms)[i]
    for (j in 1:length(model_result[, 'precision']))
    {
      pr <- model_result[, 'precision'][j]
      rc <- model_result[, 'recall'][j]
      n <- ns[j]
      prrc <- rbind(prrc, c(alg, pr, rc, n))
    }
  }
  
  prrc <- prrc[-1,]
  
  #dtype conversion
  for(i in 2:dim(prrc)[2])
  {
    prrc[,i] <- as.numeric(prrc[,i])
  }
  
  
  
  ggplot(data=prrc, aes(x=recall, y=precision, color=model)) + 
    geom_jitter() + 
    geom_text(aes(label=n), vjust=-.0, hjust=-.2, show.legend = FALSE) +
    labs(
      title = paste0("Precision vs Recall, goodRating=", good_rating),
      subtitle = sub,     
      y = "Precision",
      x = "Recall"
    )
}


# ------------------- Implementierung Ähnlichkeitsmatrix ---------------------------


sample_similarity <- function(movies)
{
  #image(as(similarity(train, method = "Cosine", which = "items"), "matrix"))
  
  # plotSimilarityMatrix(train, y = NULL, clusLabels = NULL, colX = NULL, colY = NULL, myLegend = NULL, fileName = "posteriorSimilarityMatrix", savePNG = FALSE, semiSupervised = FALSE, showObsNames = FALSE, clr = FALSE, clc = FALSE, plotWidth = 500, plotHeight = 450)
  
  #dim(as.matrix(subset(movies_wider, select = -c(user))))
  
  #sample <- movies_wider[, colnames(movies_wider) != 'user']
  #sample <- sample[, sample(ncol(sample), size=100)]
  
  #sample_matrix <- as.matrix
  
  set.seed(123)
  mov_sample <- sample(unique(movies$item), size=100)
  
  sample_sim <- movies %>% filter(item %in% mov_sample)

  #length(unique(sample_sim$item))
  #true_positives <- relevant %>% sum(rating >= threshold)
  #false_positives <- relevant %>% sum(rating < threshold)
  
  return(sample_sim)
}

cosine_sim <- function(A, B)
{
  #len <- dim(wide_matrix)[2]
  #res <- diag(len)
  #for(i in 1:len)
  #{
  #  for(j in 1:len)
  #  {
  #    if(i < j & i != j)
  #    {
  #      res[i,j] <- cosine_sim(wide_matrix[,i], wide_matrix[,j])
  #      res[j,i] <- res[i,j]
  #    }
  #  }
  #}
  similarity <- A %*% B / (norm(A, type="2") * norm(B, type="2"))
  return(similarity)
}

cosine_sim2 <- function(A, B)
{
  zae <- A %*% t(B)
  nen1 <- A %*% t(A)
  nen2 <- B %*% t(B)
  nen <- sqrt(diag(nen1) %*% t(diag(nen2)))
  #print(dim(zae))
  #print(dim(nen))
  similarity <- zae / nen
  return(similarity)
}

jaccard_sim <- function(A, B)
{
  # len <- dim(sample_bin)[2]
  # res <- diag(len)
  # for(i in 1:len)
  # {
  #   for(j in 1:len)
  #   {
  #     if(i < j & i != j)
  #     {
  #       res[i,j] <- jaccard_sim(sample_bin[,i], sample_bin[,j])
  #       res[j,i] <- res[i,j]
  #     }
  #   }
  # }
  
  inter = length(intersect(A, B))
  union = length(A) + length(B) - inter
  jac = inter / union
  return (jac)
}

jaccard_sim2 <- function(A, B)
{
  inter <- A %*% t(B)
  B_rev <- (1 - B)
  int_r <- ((1 - A) %*% t(1 - B))
  nom <- dim(A)[2] - int_r
  jac <- inter / nom
  
  return (jac)
}

plot_sim <- function(A, title)
{
  rownames(A) <- c()
  colnames(A) <- c()
  #if(min(A) < 0)
  #{
  #  levelplot(A, xlab="items", ylab="items", main="IBCF similarity visualization", col.regions=colorRampPalette(c("red", "white", "black")))
  #}
  #else
  #{
  levelplot(A, xlab="items", ylab="items", main=title, col.regions=colorRampPalette(c("white", "black")))
  #}
}


# ------------------- Implementierung Top-N Metriken ---------------------------


show_coverage <- function(listOfDifferentN, recommender, matrix) {
  
  listOfCoverages <- vector()
  for (N in listOfDifferentN) {
    
    # predict top N movies
    pre <- predict(recommender, matrix, n = N)
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


show_novelty <- function(listOfDifferentN, recommender, matrix) {
  # create a data set wit calculated popularity for every movie
  popularity <- as(MovieLense, "data.frame")
  popularity <- popularity %>%
    group_by(item) %>% 
    summarize(ratings = n() / dim(MovieLense)[1]) %>% 
    mutate(ratings = log2(ratings))
  
  listOfNovelties <- vector()
  for (N in listOfDifferentN) {
    # get top-N-list for a certain N
    pre <- predict(recommender, matrix, n = N)
    reco_list <- as(pre, "list")
    # This data frame has: Rows = N Movies, Columns = Users
    recommendations <- as.data.frame(reco_list)
    
    
    total_novelty <- vector()
    # this loop could be vectorized: https://swcarpentry.github.io/r-novice-inflammation/15-supp-loops-in-depth/
    # didn't do it yet because computational overhead is still small
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


coverage_novelty <- function(matrix) {
  rec <- Recommender(matrix, method = "IBCF", param=list(method="Cosine", k=30, normalize = NULL, na_as_zero = TRUE)) #normalize = 'center', 'Z-score'
  
  df_coverage <- show_coverage(c(5,10,15,20,25,30), rec, matrix)
  df_novelty <- show_novelty(c(5,10,15,20,25,30), rec, matrix)
  
  df_combined <- inner_join(df_coverage, df_novelty, by = 'N')
  
  ggplot(data=df_combined, aes(x=coverage, y=novelty, group=1)) +
    geom_line() +
    geom_text(aes(label=N), vjust=-.25, hjust=-.05, show.legend = FALSE) +
    labs(
      title = "Coverage gegenueber Novelty fuer verschiedene N",
      y = "Novelty",
      x = "Coverage"
    ) +
    theme(text = element_text(size = 12))
}


# uncomment with: ctrl+shift+c
# show_precision <- function(listOfDifferentN, ratingMatrix, threshold) {
# 
#   # normalize the rating matrix
#   ratingMatrix <- normalize(ratingMatrix, method="Z-score", row=TRUE)
# 
#   # create a training set and a test set with true positives for recall and precision
#   data <- as(ratingMatrix, "data.frame")
#   relevant <- data %>% group_by(user) %>% sample_n(30)
#   true_positives <- relevant %>% filter(rating >= threshold)
#   false_positives <- relevant %>% filter(rating < threshold)
# 
#   # remove testing observations from training set
#   train <- anti_join(data, relevant,by=c('user','item'))
#   train <- as(train, 'realRatingMatrix')
# 
#   # train model based on training set
#   rec <- Recommender(train, method = "IBCF", param=list(method="Cosine", k=30, normalize = NULL, na_as_zero = TRUE)) #normalize = 'center', 'Z-score'
# 
#   for (N in listOfDifferentN) {
# 
#     # predict top N movies
#     pre <- predict(rec, train, n = N)
#     reco_list <- as(pre, "list")
#     recommendations <- as.data.frame(reco_list)
# 
#     # find true positives and false positives for all users and add them up
#     true_total <- 0
#     false_total <- 0
#     for (i in as.list(unique(true_positives['user']))$user) {
#       our_user <- paste('X', i, sep = '')
#       recommendations['item'] <- recommendations[our_user]
# 
#       true_total <- true_total + nrow(inner_join(recommendations['item'], true_positives %>% filter(user == as.integer(i)), by = 'item'))
#       false_total <- false_total + nrow(inner_join(recommendations['item'], false_positives %>% filter(user == as.integer(i)), by = 'item'))
#     }
# 
#     # print Summary
#     print(paste('N =', N))
#     print(paste('Number of True Positives:',true_total))
#     print(paste('Number of False Positives:',false_total))
#     print(paste('Precision:',true_total / (true_total + false_total)))
#     print('')
#   }
# }


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
      color = element_blank()
    ) +
    theme(
      text = element_text(size = 12),
      legend.position = 'bottom'
    )
}


compute_mean_absolute_percentage_error <- function(df_user_genres) {
  df_user_genres_wide <- spread(df_user_genres, key = list, value = count) %>% transmute(count_diff = abs(count_best - count_top_n))
  
  return(round(mean(df_user_genres_wide$count_diff), 3))
}
