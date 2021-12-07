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