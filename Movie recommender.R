library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

movies = read.csv("C:/Users/Administrator/Desktop/movies.csv",stringsAsFactors=FALSE)

ratings = read.csv("C:/Users/Administrator/Desktop/ratings.csv")

summary(movies)
summary(ratings)

str(movies)
str(ratings)

genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)


genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
?tstrsplit

genre_list = c(unique(genres2[,1]),unique(genres2[,2]),
               unique(genres2[,3]),unique(genres2[,4]),unique(genres2[,5])
               ,unique(genres2[,6]),unique(genres2[,7]),
               unique(genres2[,8]),unique(genres2[,9]),unique(genres2[,10]))

genre_list = unique(genre_list)
View(genre_list)

genre_list = genre_list[-c(19,20,21)]

genre_matrix <- matrix(0,9126,18)
genre_matrix[1,] <- genre_list
View(genre_matrix)
colnames(genre_matrix) <- genre_list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}
View(genre_matrix2)

genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE)

for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])  #convert from characters to integers
} 

#Creating a search matrix with movies, id and genres matrix

search_matrix <- cbind(movies[,1:2], genre_matrix2)
View(search_matrix)

#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
View(ratingmat[1:5,])
ratingmat <- as.matrix(ratingmat[,-1])
View(ratingmat[,1:5])

#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")
ratingmat

similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                      
                                        which = "users")
?similarity
as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "User similarity")
similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)

image(as.matrix(similarity_items), main = "Movies similarity")


vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings)

table_ratings <- table(vector_ratings) # what is the count of each rating value
table_ratings


vector_ratings <- vector_ratings[vector_ratings != 0] # rating == 0 are NA values
vector_ratings <- factor(vector_ratings)

plot(vector_ratings)


library(dplyr)
views_per_movie = colCounts(ratingmat)
View(views_per_movie)
table_views = data.frame(movie = names(views_per_movie),
                         views = views_per_movie)
View(table_views)
table_views = table_views %>% arrange(-views)
table_views$title <- NA




for (i in 1:9066)
{
  table_views[i,3] <- as.character(subset(movies, 
                                          movies$movieId == table_views[i,1])$title)
}

ggplot(table_views[1:20, ], aes(x = reorder(title,views), y = views)) +
  geom_bar(stat="identity") + coord_flip()


average_ratings <- colMeans(ratingmat)
View(average_ratings)
hist(average_ratings)

average_ratings_relevant <- average_ratings[views_per_movie > 50]
# we select only those movies which have been watched atleast 50 times.
View(average_ratings_relevant)
hist(average_ratings_relevant)

ratings_movies <- ratingmat[rowCounts(ratingmat) > 50,
                            colCounts(ratingmat) > 50]

ratings_movies_norm <- normalize(ratings_movies)


#item based collaborative filtering
#Here, for each two items we will measure how similar they are in terms of users giving them the same rating. we will use cosine similarity.
#We will also identify the k most similar items
#for each user we will recommend movies based on the movies he has rated.
#Splitting the data into train and test

which_train = sample(x = c(TRUE, FALSE), 
                     size = nrow(ratings_movies),
                     replace = TRUE, 
                     prob = c(0.8, 0.2))


recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]
#Building the IBCF recommender
recc_model <- Recommender(data = recc_data_train, 
                          method = "IBCF",
                          parameter = list(k = 30, measure = 'cosine'))

recc_model

model_details = getModel(recc_model)
model_details$description




#Extracting recommendations
#For each user the algorithm extracts its rated movies. For each movie, it identifies the similar movies based on the similarity matrix.
#Each rating is multiplied by the similarity measure. The top n recommendations are returned.
recc_predicted = predict(recc_model, newdata = recc_data_test, n = 10)

recc_predicted
## Recommendations as 'topNList' with n = 10 for 96 users.
recc_user_1 = recc_predicted@items[[1]] # recommendation for the first user
movies_user_1 = recc_predicted@itemLabels[recc_user_1]
movies_user_1
movies_user_2 = movies_user_1
for (i in 1:10){
  movies_user_2[i] <- as.character(subset(movies, 
                                          movies$movieId == movies_user_1[i])$title)
}
movies_user_2

recc_matrix <- sapply(recc_predicted@items, 
                      function(x){ as.integer(colnames(ratings_movies)[x]) })
# matrix with the recommendations for each user

number_of_items <- factor(table(recc_matrix))

chart_title <- "Distribution of the number of items for IBCF"
qplot(number_of_items) + ggtitle(chart_title)


number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
View(number_of_items_sorted)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)

for (i in 1:4){
  table_top[i,1] <- as.character(subset(movies, 
                                        movies$movieId == table_top[i,1])$title)
}

colnames(table_top) <- c("Movie title", "No of items")
head(table_top)


#User based Collaborative Filtering
recc_model <- Recommender(data = recc_data_train, method = "UBCF")
recc_model
## Recommender of type 'UBCF' for 'realRatingMatrix' 
## learned using 325 users.
model_details <- getModel(recc_model)
model_details$data
## 325 x 444 rating matrix of class 'realRatingMatrix' with 30151 ratings.
## Normalized using center on rows.
recc_predicted <- predict(object = recc_model,
                          newdata = recc_data_test, 
                          n = 10) 
number_of_items <- factor(table(recc_matrix))
plot(number_of_items)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)), number_of_items_top)

for (i in 1:4){
  table_top[i,1] <- as.character(subset(movies, 
                                        movies$movieId == table_top[i,1])$title)
}
colnames(table_top) <- c("Movie title", "No of items")
head(table_top)
