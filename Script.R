if(!require(tinytex)) install.packages("tinytex")
if(!require(float)) install.packages("float")
if(!require(caret)) install.packages("caret")
if(!require(data.table)) install.packages("data.table")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lubridate)) install.packages("lubridate")
if(!require(stringr)) install.packages("stringr")
if(!require(recosystem)) install.packages("recosystem")
if(!require(recommenderlab)) install.packages("recommenderlab")
if(!require(tidyr)) install.packages("tidyr")
if(!require(GGally)) install.packages("GGally")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(stringr)) install.packages("stringr")
if(!require(lattice)) install.packages("lattice")
if(!require(rpart)) install.packages("rpart")
if(!require(knitr)) install.packages("knitr")
if(!require(rmarkdown)) install.packages("rmarkdown")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(corrplot)) install.packages("corrplot")
if(!require(reshape2)) install.packages("reshape2")
if(!require(dslabs)) install.packages("dslabs")


library(tinytex)
library(float)
library(caret)
library(data.table)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(recosystem)
library(recommenderlab)
library(tidyr)
library(GGally)
library(ggthemes)
library(stringr)
library(lattice)
library(rpart)
library(knitr)
library(rmarkdown)
library(gridExtra)
library(ggrepel)
library(corrplot)
library(reshape2)
library(dslabs)


#####################################################
#                                                   #
#   Part I - Data structure and data reprocessing   #
#                                                   #
#####################################################

# Chapter 1 Loading and transform the Movielens data set from https://grouplens.org/datasets/movielens/10m/
  ## MovieLens 10M dataset:
  ## https://grouplens.org/datasets/movielens/10m/
  ## http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

invisible(invisible(gc())) # for cleaning memory

# Chapter 2 - Analyse the data structure
  ## We will observe the structure of the movielens data set

str(movielens)
head(movielens)

  ## We will make sure the the edx dataset haven't NA's values
apply(movielens, 2, function(x) {
  any(is.na(x))
})

# Chapter 3 - Data reprocessing
  ## We will reprocess the "timestamp" column in order to exploit data the date in which users have rated movies. Then we will delete the "timestamp" column
movielens_reprocessing<- movielens %>% mutate( year_rated = year(as_datetime(timestamp))) %>% select(-timestamp)
head(movielens_reprocessing)

  ## We will reprocess the "tittle" column in order to extract the movie release year contained in this column
movielens_reprocessing<- movielens_reprocessing %>% mutate(year_release = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),
       title = str_remove(title, "[/(]\\d{4}[/)]$"))
head(movielens_reprocessing)

  ## We will ensure that the rated year by the user is less than the release year of the movie
movielens_reprocessing %>% mutate(nyear_after_release= year_rated - year_release) %>% filter(nyear_after_release < 0)

movielens_reprocessing %>% mutate(nyear_after_release= year_rated - year_release) %>% filter(nyear_after_release < 0) %>% 
  select(title, year_release) %>% distinct()

  ## We will replace this by the date of the release year
movielens_reprocessing<- movielens_reprocessing %>% 
  mutate(year_rated= if_else(year_rated - year_release <0, year_release, year_rated))

  ## We do the same control to ensure that the rated year by the user is less than the release year of the movie
movielens_reprocessing %>% mutate(nyear_after_release= year_rated - year_release) %>% filter(nyear_after_release < 0)

## We will ensure that movies have only one release year
movielens_reprocessing %>% select(movieId, year_release) %>% distinct() %>% group_by(movieId) %>% summarize(n= n(), year_release= year_release) %>% filter(n >= 2) %>%
  select(movieId,year_release) %>% ungroup() %>% head()


  ## We will add a column for the age of the movie (from today until his release year)

movielens_reprocessing <- movielens_reprocessing %>% group_by(movieId) %>% mutate(movie_age= 2022 - year_release) %>% ungroup()
head(movielens_reprocessing)

save(movielens_reprocessing, file = "object_movielens_reprocessing.Rdata")

# Reprocessing the "genres" column
  ## We will create a new dataframe for later analysing the distribution of the data associated with genres data
data_genres<- movielens  %>% separate_rows(genres, sep= "\\|") # Take time
head(data_genres)

invisible(invisible(gc())) # for cleaning memory

save(data_genres, file = "object_data_genres.Rdata")

  ## We will delete rows for which there is no genres mentioned
data_genres<- data_genres %>% filter(genres != "(no genres listed)")

invisible(invisible(gc())) # for cleaning memory

  ## We will create a matrix from movielens data for later analysing the correlation between movies genres
    ### First, we will select variables and we will store them into a new data set
movielens_matrix<- movielens %>% select(userId,movieId,rating,genres)
head(movielens_matrix)

    # ## And we will delete rows for which there is no genres mentioned
movielens_matrix<- movielens_matrix %>% filter(genres != "(no genres listed)")

    ### Secondly, We will store movies genres into a list
list<- c("Action", "Adventure", "Comedy", "Romance", "Crime",  "Thriller", "Drama", "Sci-Fi", "Children",
         "Fantasy", "War", "Animation", "Musical", "Western", "Mystery", "Film-Noir", "Horror",
         "Documentary", "IMAX")

    ### Then, we will implement a function which goal is to create a column for each movies genres. We will mention in each column a 1 in so far as
    ### the rated movie correspond to the column genres. Otherwise, we will mention a -1.
for(i in list){
  movielens_matrix<- movielens_matrix %>% mutate(!! i := if_else(str_detect(genres, i), 1, -1))
}

rm(i, list)

invisible(invisible(gc())) # for cleaning memory

    ### We will remove the initial genres column and transform the data set into a matrix
movielens_matrix<- movielens_matrix %>% select(-genres) %>% as.matrix()
head(movielens_matrix)

invisible(invisible(gc())) # for cleaning memory

save(movielens_matrix, file = "object_movielens_matrix.Rdata")

# Chapter 4 - We will split the movielens_reprocessing data set in a training test and a validation test
# The training test will be used to train ours predictive algorithms and select the best parameters which we will permits us to minimize the RMSE
# The test set will be used to evaluate ours predictive algorithms

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens_reprocessing$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens_reprocessing[-test_index,]
temp <- movielens_reprocessing[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, removed)


#####################################################
#                                                   #
#          Part II - Data visualization             #
#                                                   #
#####################################################

# Chapter 1 - movies data visualization

  ## We will analyse the number of movies contained into the Movielens data set
n_distinct(movielens_reprocessing$movieId)

  ## We will analyse the principal statistical variables of the number of ratings by movies

x<- movielens_reprocessing %>% group_by(movieId) %>% mutate(ratings_number= n()) %>%
  ungroup() %>% select(movieId,ratings_number) %>% distinct() # Evaluate the ratings number by movies

as.data.frame(quantile(x$ratings_number, c(0.1, .25, 0.5, 0.75, .9, .95))) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(x$ratings_number),2), median= round(median(x$ratings_number),2),
                                                               mean= round(mean(x$ratings_number),2),
           standard_deviation= round(sd(x$ratings_number),2), max.= round(max(x$ratings_number),2)) # Evaluate min, max, median, mean and standard deviation

  ## we will represent a density plot of the movies' ratings number
  
x_plot<- x %>% ggplot(aes(ratings_number)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) + 
  scale_x_continuous(breaks = seq(0,35000,5000)) +
  labs(title = "Density curve of the number of ratings",x= "Number of ratings", y= "Frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We do the same but with a log2 for the x-axis
x_log2<- x %>% ggplot(aes(ratings_number)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) +
  scale_x_continuous(trans = "log2") + labs(title = "Density curve of the most ratings number",x= "Number of ratings (log2)", y= "Frequency") + theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

  ## We will plot the cumulative sum of the number of ratings
x_cum<- movielens_reprocessing %>% group_by(movieId) %>% summarize(n= n()) %>% arrange(desc(n)) %>%
  mutate(cs= cumsum(n), cs= cs / max(cs), movie= row_number()) %>%
  ggplot(aes(movie, cs)) +
  geom_line(color = "red", alpha= .6) + scale_x_continuous(limits = c(0,10000), breaks = seq(0,10000,1000)) +
  labs(title = "Cumulative density curve", x= "Number of movies", y= "frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We will merge these 3 graphs on one
grid.arrange(x_plot, x_log2, x_cum, widths= c(1,1), layout_matrix= rbind(c(1), c(2,3)))


  ## We will represent in a table the number of ratings for the twenty most movies
movielens_reprocessing %>% group_by(title) %>% summarize(n= n()) %>% arrange(desc(n)) %>% top_n(20)

  ## We will do the same but for the twenty worst movies
movielens_reprocessing %>% group_by(title) %>% summarize(n= n()) %>% arrange(n) %>% slice(1:20)

  ## We will analyse the frequency of the number of users by movies

y<- movielens_reprocessing %>% group_by(movieId) %>% mutate(users_number= n_distinct(userId)) %>%
  ungroup() %>% select(movieId,users_number) %>% distinct() # Evaluate the ratings number by movies

  ## we will represent a density curve of the movies' ratings number and users number

left_join(x,y, by= "movieId") %>% pivot_longer(cols = -movieId, names_to = "frequency") %>%
  ggplot(aes(value, color= frequency)) + geom_density(kernel= "optcosine", fill= "blue", alpha= .1) +
  labs(x= "Number of users or ratings by movies", y="Frequency") + theme_classic()

  ## The distribution is the same that the number of ratings by movie. Then, a user never rated the same twice
movielens %>% group_by(movieId, userId) %>% summarize(n= n()) %>% filter(n > 1)

rm(x,y,x_plot,x_log,x_cum)

# Chapter 2 - Users data visualization

  ## We will analyse the number of users contained into the Movielens_reprocessing data set

n_distinct(movielens_reprocessing$userId)

  ## We will analyse the principal statistical variables of the number of ratings by users

x<- movielens_reprocessing %>% group_by(userId) %>% mutate(ratings_number= n()) %>%
  ungroup() %>% select(userId,ratings_number) %>% distinct() # Evaluate the ratings number by users

as.data.frame(quantile(x$ratings_number, c(0.1, .25, 0.5, 0.75, .9, .95))) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(x$ratings_number),2), median= round(median(x$ratings_number),2),
           mean= round(mean(x$ratings_number),2),
           standard_deviation= round(sd(x$ratings_number),2), max.= round(max(x$ratings_number),2)) # Evaluate min, max, median, mean and standard deviation

  ## we will represent a density plot of the users' ratings number
x_plot<- x %>% ggplot(aes(ratings_number)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) + 
  scale_x_continuous(breaks = seq(0,7500,500)) +
  labs(title = "Density curve of the number of ratings",x= "Number of ratings", y= "Frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We do the same but with a log2 for the x-axis
x_log2<- x %>% ggplot(aes(ratings_number)) + geom_density(kernel= "optcosine", adjust= 1.5, fill= "blue", alpha= .1) +
  scale_x_continuous(trans = "log2") + labs(title = "Density curve of the most ratings number",x= "Number of ratings (log2)", y= "Frequency") + theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

  ## We will plot the cumulative sum of the number of ratings
x_cum<- movielens_reprocessing %>% group_by(userId) %>% summarize(n= n()) %>% arrange(desc(n)) %>%
  mutate(cs= cumsum(n), cs= cs / max(cs), user= row_number()) %>%
  ggplot(aes(user, cs)) +
  geom_line(color = "red", alpha= .6) + scale_x_continuous(limits = c(0,70000), breaks = seq(0,70000,10000)) +
  labs(title = "Cumulative density curve", x= "Number of users", y= "frequency") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

## We will merge these 3 graphs on one
grid.arrange(x_plot, x_log2, x_cum, widths= c(1,1), layout_matrix= rbind(c(1), c(2,3)))

rm(x,y,x_cum,x_plot,x_log2)

# Chapter 3 - ratings data visualization

  ## We will analyse the principal statistical variables of the number of ratings by stars

as.data.frame(quantile(movielens_reprocessing$rating, c(0.1, .25, 0.5, 0.75, .9, .95))) # Evaluate some quantiles and deciles 

data.frame(min.= round(min(movielens_reprocessing$rating),2), median= round(median(movielens_reprocessing$rating),2),
           mean= round(mean(movielens_reprocessing$rating),2),
           standard_deviation= round(sd(movielens_reprocessing$rating),2), 
           max.= round(max(movielens_reprocessing$rating),2)) # Evaluate min, max, median, mean and standard deviation

  ## We will plot an histogram of the ratings number by stars
x<- movielens_reprocessing %>% group_by(rating) %>% summarize(ratings_number= n()) # Evaluate the ratings number by stars

x_hist<- movielens_reprocessing %>% ggplot(aes(rating)) + geom_histogram(bins = 10 ,color= "black", fill= "steelblue", alpha= .1) + 
  scale_y_continuous(breaks = seq(0,3000000,500000)) + scale_x_continuous(breaks = seq(0,5,.5)) +
  labs(x= "Stars", y= "Ratings number")  + theme_classic()

x_nratings<- x %>% mutate(ratings_number= format(ratings_number,big.mark= " "))
x_nratings<- tableGrob(x_nratings, rows = NULL, cols = c("Stars", "Ratings number"), theme = ttheme_minimal())

grid.arrange(x_hist,x_nratings, ncol= 2, widths= c(8,2))


  ## We will analyse the distribution of ratings by users

x_users<- movielens_reprocessing %>% group_by(userId) %>% summarize(mean= round(mean(rating),2), 
                                                                    min.= round(min(rating),2), 
                                                                    max.= round(max(rating),2), 
                                                                    standard_deviation= round(sd(rating),2),
                                                                    ratings_number= n_distinct(movieId)) %>% ungroup()

    ### We will plot the distribution of ratings by users
x_plot<- movielens_reprocessing %>% group_by(userId) %>% summarize(m= mean(rating), min= min(rating), max= max(rating), sd= sd(rating),
                                                          n= n_distinct(movieId)) %>% 
  arrange(desc(n)) %>% ggplot(aes(userId,n)) + geom_line(aes(m), color= "blue") + labs(title = "Distribution of the ratings mean by users", x= "Stars", y= "Ratings number by users") +
  scale_y_continuous(breaks = seq(0,7500,500)) +  scale_x_continuous(breaks = seq(0,5,.5)) + theme_classic() + theme(plot.title = element_text(hjust = 0.5))

    ### We will represent the density curve of the mean and the standard deviation of the ratings by users
x_density<- x_users %>% pivot_longer(cols = c(mean, standard_deviation), names_to = "Variables") %>% ggplot(aes(value, color= Variables)) +
  geom_density(aes(fill= Variables),kernel= "gaussian", adjust= 1.5, alpha= .1)  + scale_x_continuous(breaks = seq(0,5,.5)) +
  labs(title = "Density curve of the mean of ratings and of the standard deviation by users",
                                                                                        x= "Stars") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

    ### We will represent the minimum and the maximun of rating by users

x_hist<- x_users %>% pivot_longer(cols = c(min.,max.), names_to = "variables") %>% ggplot(aes(value, color= variables)) + 
  geom_bar(aes(fill= variables), bins=10, position = position_dodge()) + scale_y_continuous(breaks = seq(0,70000,10000)) + scale_x_continuous(breaks = seq(0,5,.5)) +
  labs(title = "Histogram of minimum and maximum ratings by users", x= "Stars", y= "Number of users") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

x_users_min<- x_users %>% group_by(min.) %>% summarize(n_min= n()) %>% ungroup()
x_users_max<- x_users %>% group_by(max.) %>% summarize(n_max= n()) %>% ungroup()

names(x_users_min)[names(x_users_min) == "min."] <- "stars"
names(x_users_max)[names(x_users_max) == "max."] <- "stars"

x_users_intervals<- left_join(x_users_min,x_users_max, by= "stars")
x_users_intervals<- x_users_intervals %>% mutate(n_min= format(n_min, big.mark= " "), n_max= format(n_max, big.mark= " "))
x_users_intervals<- tableGrob(x_users_intervals, rows = NULL, cols = c("Stars", "Number of Min.", "Number of Max."), theme = ttheme_minimal())

grid.arrange(x_plot, x_hist, x_users_intervals, x_density, widths= c(1,1), layout_matrix= rbind(c(1), c(2,3), c(4)))

rm(x, x_density, x_hist, x_nratings, x_plot, x_users, x_users_intervals, x_users_min,x_users_max)
invisible(invisible(gc())) # for cleaning memory

  ## We will analyse the distribution of ratings by movies

x_movies<- movielens_reprocessing %>% group_by(movieId) %>% summarize(mean= round(mean(rating),2), 
                                                                    min.= round(min(rating),2), 
                                                                    max.= round(max(rating),2), 
                                                                    standard_deviation= round(sd(rating),2),
                                                                    ratings_number= n_distinct(movieId)) %>% ungroup()

### We will plot the distribution of ratings by movies
x_plot<- movielens_reprocessing %>% group_by(movieId) %>% summarize(m= mean(rating), min= min(rating), max= max(rating), sd= sd(rating),
                                                                   n= n_distinct(userId)) %>% 
  ggplot(aes(movieId,n)) + geom_line(aes(m), color= "blue") + labs(title = "Distribution of the ratings mean by movies", x= "Stars", y= "Ratings number by movies") +
  scale_y_continuous(breaks = seq(0,35000,5000)) + scale_x_continuous(breaks = seq(0,5,0.5)) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

### We will represent the density curve of the mean and the standard deviation of the ratings by users
x_density<- x_movies %>% pivot_longer(cols = c(mean, standard_deviation), names_to = "Variables") %>% ggplot(aes(value, color= Variables)) +
  geom_density(aes(fill= Variables),kernel= "gaussian", adjust= 1.5, alpha= .1)  + scale_x_continuous(breaks = seq(0,5,.5)) +
  labs(title = "Density curve of the mean of ratings and of the standard deviation by movies",
                                                                                        x= "Stars") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

### We will represent the minimum and the maximum of rating by movies

x_hist<- x_movies %>% pivot_longer(cols = c(min.,max.), names_to = "variables") %>% ggplot(aes(value, color= variables)) + 
  geom_bar(aes(fill= variables), bins=10, position = position_dodge()) + scale_y_continuous(breaks = seq(0,80000,1000)) + 
  scale_x_continuous(breaks = seq(0,5,.5)) +
  labs(title = "Histogram of minimum and maximum ratings by movies", x= "Stars", y= "Number of movies") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

x_movies_min<- x_movies %>% group_by(min.) %>% summarize(n_min= n()) %>% ungroup()
x_movies_max<- x_movies %>% group_by(max.) %>% summarize(n_max= n()) %>% ungroup()

names(x_movies_min)[names(x_movies_min) == "min."] <- "stars"
names(x_movies_max)[names(x_movies_max) == "max."] <- "stars"

x_movies_intervals<- left_join(x_movies_min,x_movies_max, by= "stars")
x_movies_intervals<- x_movies_intervals %>% mutate(n_min= format(n_min, big.mark= " "), n_max= format(n_max, big.mark= " "))
x_movies_intervals<- tableGrob(x_movies_intervals, rows = NULL, cols = c("Stars", "Number of Min.", "Number of Max."), theme = ttheme_minimal())

grid.arrange(x_plot, x_hist, x_movies_intervals, x_density, widths= c(1,1), layout_matrix= rbind(c(1), c(2,3), c(4)))

rm(x_movies,x_plot, x_density, x_hist, x_movies_max,x_movies_min, x_movies_intervals)
invisible(invisible(gc())) # for cleaning memory

# Chapter 4 - genres data visualization
  ## We will represent the number of ratings by genre
plot_genres<- data_genres %>% group_by(genres) %>% summarize(n= n()) %>% arrange(desc(n)) %>% 
  ggplot(aes(reorder(genres, -n),n)) + geom_bar(stat = "identity", fill= "blue", alpha= .1) + coord_flip() + scale_y_continuous(breaks = seq(0,4000000,500000)) +
  labs(title = "Fig 6.1 - The number of ratings by genre", x= "Ratings number", y="Genres") + theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We will represent the number of movies by genre
plot_movies_genres<- data_genres %>% group_by(genres) %>% summarize(n= n_distinct(movieId)) %>%
  ggplot(aes(reorder(genres,-n),n)) + geom_bar(stat = "identity", fill= "blue", alpha= .1) + coord_flip() + scale_y_continuous(breaks = seq(0,5000,500)) +
  labs(title = "Fig 6.2 - The number of movies by genre", x= "Movies number", y="Genres") + theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We will represent the number of users by genre
plot_users_genres<- data_genres %>% group_by(genres) %>% summarize(n= n_distinct(userId)) %>%
  ggplot(aes(reorder(genres,-n),n)) + geom_bar(stat = "identity", fill= "blue", alpha= .1) + coord_flip() + scale_y_continuous(breaks = seq(0,70000,10000)) +
  labs(title = "Fig 6.3 - The number of users by genre", x= "Users number", y="Genres") + theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We will represent the distribution of the ratings by genre
plot_rating_genres<- data_genres %>% ggplot(aes(rating, genres)) + geom_boxplot(outlier.color = "red",fill= "blue", color= "black", alpha=.1) + 
  geom_vline(xintercept = mean(data_genres$rating),col = "red", linetype = "dashed") + scale_x_continuous(breaks = seq(0,5,.5)) +
  labs(title = "Fig 6.4 - Distribution of the ratings by genre", x= "Stars", y= "Genres") + theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ## We will represent the distribution of the number of the ratings and the mean of the ratings by genres

plot_points_genres<- data_genres %>% group_by(genres) %>% summarize(n= n(), m= mean(rating)) %>%
  ggplot(aes(m,n, label= genres)) + geom_point(color="black") + geom_text_repel(color="black") + 
  geom_vline(xintercept = mean(movielens_reprocessing$rating), col= "red", linetype= "dashed") + annotate("text", x= 3.55, y= 3500000, label= "Mean = 3.53", color= "red", alpha= .6) +
  scale_y_continuous(breaks = seq(0,4000000,500000)) +
  labs(title = "Fig 6.5 - Distribution of the number of ratings and the ratings mean by genre", x= "Stars", y="Ratings Number") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot_genres, plot_movies_genres, plot_users_genres, plot_rating_genres, plot_points_genres, widths= c(1,1), layout_matrix= rbind(c(1,2), c(3,4), c(5)))

rm(data_genres,plot_genres, plot_movies_genres, plot_points_genres, plot_rating_genres, plot_users_genres)
invisible(invisible(gc())) # for cleaning memory

# Chapter 5 - years data visualization
  ### We will represent the number of ratings by year release and the first rating by user
plot_y_1<- movielens_reprocessing %>% group_by(year_release) %>% summarize(n=n()) %>%
  ggplot(aes(year_release,n)) + geom_point(color= "steelblue", alpha= .6) + geom_line(color="steelblue", alpha=.6) + 
  geom_vline(xintercept = min(movielens_reprocessing$year_rated), color="red",  linetype= "dashed") + 
  annotate("text", x= 1996, y= 100000, angle= 90, label= "First rating", color= "red", alpha= .6) +
  scale_x_continuous(breaks = seq(1900,2010,10)) + scale_y_continuous(breaks = seq(0,800000,100000)) +
  labs(title = "Fig 7.1 - Distribution of the number of ratings by release year", x= "Release years", y= "Ratings number") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ### We will represent the number of ratings by year of rating
plot_y_2<- movielens_reprocessing %>% group_by(year_rated) %>% summarize(n=n()) %>%
  ggplot(aes(year_rated,n)) + geom_point(color= "steelblue", alpha=.6) + geom_line(color="steelblue", alpha=.6) + 
  geom_vline(xintercept = min(movielens_reprocessing$year_rated), color="red",  linetype= "dashed") + 
  annotate("text", x= 1995.2, y= 1000000, angle= 90, label= "First rating", color= "red", alpha= .6) +
  labs(title = "Fig 7.2 - Distribution of the number of ratings by rated year", x= "rated years", y= "Ratings number") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

  ### We will represent the number of movies by year of release
plot_y_3<- movielens_reprocessing %>% group_by(year_release) %>% summarize(nmovies= n_distinct(movieId)) %>%
  ggplot(aes(year_release, nmovies)) + geom_point(color= "steelblue", alpha=.6) + geom_line(color= "steelblue", alpha=.6) +
  labs(title = "Fig 7.3 - Distribution of the number of movies by release year", x= "Release years", y= "Movies number") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
  
  ### We will represent the number of users by year of release
plot_y_4<- movielens_reprocessing %>% group_by(year_release) %>% summarize(nusers= n_distinct(userId)) %>%
  ggplot(aes(year_release, nusers)) + geom_point(color= "steelblue", alpha=.6) + geom_line(color= "steelblue", alpha=.6) +
  labs(title = "Fig 7.4 - Distribution of the number of users by release year", x= "Release years", y= "Users number") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot_y_1,plot_y_2,plot_y_3,plot_y_4, widths= c(1,1), layout_matrix= rbind(c(1,2), c(3,4)))

rm(plot_y_1,plot_y_2,plot_y_3,plot_y_4)
invisible(invisible(gc())) # for cleaning memory

# Chapter 6 -The correlation analysis between variables

    ### We will analyse the Correlation between genres
 cor_genres<- movielens_matrix %>% cor(use = "pairwise.complete.obs")

 corrplot(cor_genres, p.mat = cor.mtest(movielens_matrix, conf.level= 0.95)$p, sig.level = 0.01, method = "number", type = "upper")
 
 rm(cor_genres)
 invisible(invisible(gc())) # for cleaning memory
 
    ### We will analyse the correlation between number of ratings and mean of ratings by movies
plot_cor_1<- movielens_reprocessing %>% group_by(movieId) %>% summarize(nrating= n(), mean_rating= mean(rating)) %>%
  ggplot(aes(nrating,mean_rating)) + geom_point(color= "Blue", alpha= .1) + geom_smooth(method = "loess",color="red", fill= "red", alpha= .1) + 
  labs(title = "Fig. 8.1 - Correlation between ratings number and ratings mean by movies",x= "Ratings number", y="Ratings mean") + scale_x_continuous(breaks = seq(0,35000,5000)) + scale_y_continuous(breaks = seq(0,5,.5)) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

cor_1<- tableGrob(movielens_reprocessing %>% group_by(movieId) %>% summarize(n= n(), m= mean(rating)) %>% summarize(round(cor(n, m),2)),
                  cols = "Correlation", rows = NULL, theme = ttheme_minimal())

a<- grid.arrange(cor_1,plot_cor_1, heights= c(1,12))


    ### We will analyse the correlation between the age of movies and their mean ratings
plot_cor_2<- movielens_reprocessing %>% group_by(movie_age) %>% summarize(mean_rating= mean(rating)) %>%
  ggplot(aes(movie_age,mean_rating)) + geom_point(color= "blue", alpha= .1) + geom_smooth(method = "loess",color="red", fill= "red", alpha= .1) +
  labs(title = "Fig. 8.2 - Correlation between age of movies and ratings mean by the age of movies",x="Age of movies", y="Ratings mean") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))


cor_2<- tableGrob(movielens_reprocessing %>% group_by(movie_age) %>% summarize(mean_rating= mean(rating)) %>% ungroup() %>% summarize(round(cor(movie_age,mean_rating), 2)),
                  cols = "Correlation", rows = NULL, theme = ttheme_minimal())

b<- grid.arrange(cor_2,plot_cor_2, heights= c(1,12))


    ### We will analyse the correlation between the mean rating and year that the movie was released
plot_cor_3<- movielens_reprocessing %>% group_by(year_release) %>% summarize(mrating= mean(rating)) %>%
  ggplot(aes(year_release,mrating)) + geom_point(color= "blue", alpha= .1) + geom_smooth(method = "loess",color="red", fill= "red", alpha= .1) +
  labs(title = "Fig. 8.3 - Correlation between release year of movies and ratings mean by release year", x= "Release year", y="Ratings mean") +
 theme_classic() + theme(plot.title = element_text(hjust = 0.5))

cor_3<- tableGrob(movielens_reprocessing %>% group_by(year_release) %>% summarize(m= mean(rating)) %>% ungroup() %>% summarize(round(cor(year_release, m), 2)),
                  cols = "Correlation", rows = NULL, theme = ttheme_minimal())

c<- grid.arrange(cor_3, plot_cor_3, heights= c(1,12))

    ### We will analyse the correlation between the movies was rated and their mean rating
plot_cor_4<- movielens_reprocessing %>% group_by(year_rated) %>% summarize(mrating= mean(rating)) %>%
  ggplot(aes(year_rated,mrating)) + geom_point(color= "blue", alpha= .1) + geom_smooth(method = "loess",color="red", fill= "red", alpha= .1) +
  labs(title = "Fig. 8.4 - Correlation between rated year of movies and ratings mean by rated year", x= "Rated year", y="Ratings mean") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

cor_4<- tableGrob(movielens_reprocessing %>% group_by(year_rated) %>% summarize(m= mean(rating)) %>% ungroup() %>% summarise(round(cor(year_rated, m), 2)),
                  cols = "Correlation", rows = NULL, theme = ttheme_minimal())

d<- grid.arrange(cor_4, plot_cor_4, heights= c(1,12))

grid.arrange(a, b, c, d, widths= c(1,1), layout_matrix= rbind(c(1,2), c(3,4)))

rm(a, b, c, d, cor_1, cor_2, cor_3, cor_4, plot_cor_1, plot_cor_2, plot_cor_3, plot_cor_4)
invisible(invisible(gc())) # for cleaning memory


#####################################################
#                                                   #
#               Regularization model                #
#                                                   #
#####################################################

# We will implement in order to caculate the RMSE

RMSE<- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# First of all, we will split edx data set into a training set and a test set in order to avoid over training

set.seed(1988, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set_reg <- edx[-test_index,]
test_set_reg <- edx[test_index,]

  ## We will make sure userId and movieId in test set are also in the training data

test_set_reg <- test_set_reg %>% 
  semi_join(train_set_reg, by = "movieId") %>%
  semi_join(train_set_reg, by = "userId") 

rm(test_index) # we will remove the test_index data set in order to get memory

# Secondly, we will evaluate the mean of the true rating of all movies and users
mu_reg<- mean(train_set_reg$rating)

# We will implement a regularization model for training our algorithm from the training set and the test set to determine parameters that minimize the RMSE
  ## We will use cross validation to determine the lambda parameter that minimize the RMSE

lambdas <- seq(0, 10, 0.25) #the lambda parameter that minimize the RMSE

rmses<- sapply(lambdas, function(l){
  mu_reg<- mean(train_set_reg$rating) # the true rating for all movies and users
  
  b_i<- train_set_reg %>% group_by(movieId) %>% summarize(b_i= sum(rating - mu_reg)/(n()+l)) # estimate movies effect with a penalty term
  
  b_u<- train_set_reg %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u= sum(rating - b_i - mu_reg)/(n()+l)) # estimate users effect with a penalty term
  
  b_y<- train_set_reg %>% left_join(b_i, by="movieId") %>% left_join(b_u, by= "userId") %>% group_by(year_release) %>%
    summarize(b_y= sum(rating - b_i - b_u - mu_reg) / (n()+l))
  
  b_ry<- train_set_reg %>% left_join(b_i, by="movieId") %>% left_join(b_u, by= "userId") %>% left_join(b_y, by= "year_release") %>%
    group_by(year_rated) %>% summarize(b_ry= sum(rating - b_i - b_u - b_y - mu_reg) / n()+l)
  
  b_g<- train_set_reg %>% left_join(b_i, by="movieId") %>% left_join(b_u, by= "userId") %>% left_join(b_y, by= "year_release") %>%
    left_join(b_ry, by= "year_rated") %>% group_by(genres) %>% summarize(b_g= sum(rating - b_i - b_u - b_y - b_ry - mu_reg) / n()+l)
  
  predicted_ratings_reg<- test_set_reg %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% 
    left_join(b_y, by= "year_release") %>% left_join(b_ry, by= "year_rated") %>% left_join(b_g, by= "genres") %>%
    mutate(pred= mu_reg + b_i + b_u + b_y + b_ry + b_g) %>% .$pred
  
  return(RMSE(predicted_ratings_reg, test_set_reg$rating))
})

qplot(lambdas, rmses) + theme_classic() + scale_y_continuous(breaks = seq(0,10,1))
lambda<- lambdas[which.min(rmses)]
lambda

# We will estimate the RMSE from validation test and therefore we will evaluate the efficiency of our model
mu<- mean(edx$rating) # the true rating for all movies and users

b_i<- edx %>% group_by(movieId) %>% summarize(b_i= sum(rating - mu)/(n()+lambda))

b_u<- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u= sum(rating - b_i - mu)/(n()+lambda))

b_y<- edx %>% left_join(b_i, by="movieId") %>% left_join(b_u, by= "userId") %>% group_by(year_release) %>%
  summarize(b_y= sum(rating - b_i - b_u - mu) / (n()+lambda))

b_ry<- edx %>% left_join(b_i, by="movieId") %>% left_join(b_u, by= "userId") %>% left_join(b_y, by= "year_release") %>%
  group_by(year_rated) %>% summarize(b_ry= sum(rating - b_i - b_u - b_y - mu) / n()+lambda)

b_g<- edx %>% left_join(b_i, by="movieId") %>% left_join(b_u, by= "userId") %>% left_join(b_y, by= "year_release") %>%
  left_join(b_ry, by= "year_rated") %>% group_by(genres) %>%
  summarize(b_g= sum(rating - b_i - b_u - b_y - b_ry - mu) / n()+lambda)

y_hat<- validation %>% left_join(b_i, by= "movieId") %>% left_join(b_u, by= "userId") %>% 
  left_join(b_y, by= "year_release") %>% left_join(b_ry, by= "year_rated") %>% left_join(b_g, by="genres") %>%
  mutate(y_hat= mu + b_i + b_u + b_y + b_ry + b_g) %>% .$y_hat

RMSE_reg<- RMSE(y_hat,validation$rating)

results<- tibble(Model = "Regularization model", RMSE= RMSE_reg)

rm(b_g, b_i, b_ry,b_u,b_y,edx,lambda, lambdas, mu, mu_reg, rmses, test_set_reg, train_set_reg, validation, y_hat)
invisible(invisible(gc())) # for cleaning memory


#####################################################
#                                                   #
#   Matrix factorization with recommender lab       #
#                                                   #
#####################################################

# Selection of a movielens' data part

movielens_recommender<- movielens %>% group_by(movieId) %>% filter(n() >= 5000) %>% ungroup() %>%
  group_by(userId) %>% filter(n() >= 20) %>% ungroup() %>% select(userId, movieId, rating)

# We will place movieId in column. Each column represents a movieId, then we will transform it into a matrix
movielens_recommender<- movielens_recommender %>% spread(movieId, rating)
movielens_recommender<- movielens_recommender %>% as.matrix()

rownames(movielens_recommender)<- movielens_recommender[,1]
movielens_recommender<- movielens_recommender[,-1]

## We will implement a rating matrix with real valued ratings stored in sparse format
movielens_recommender<- as(movielens_recommender, "realRatingMatrix")

# We will use cross validation in order to select the given parameter which minimize the RMSE
given<- c(-10,-1,5,10,15)
best<- sapply(given, function(g){
  eval<- evaluationScheme(movielens_recommender, method= "cross", k= 10, given= g)
  rec <- Recommender(getData(eval, "train"), "LIBMF")
  predictions<- predict(rec, getData(eval, "known"), type="ratings")
  acc<- calcPredictionAccuracy(predictions, getData(eval, "unknown"))
})

# Value of given that minimize the RMSE
best_given<- given[which.min(best[1,])]


# We will repeat the same process to evaluate the RMSE of our model
eval<- evaluationScheme(movielens_recommender, method= "cross", k= 10, given= best_given)
rec <- Recommender(getData(eval, "train"), "LIBMF")
predictions<- predict(rec, getData(eval, "known"), type="ratings")
acc<- calcPredictionAccuracy(predictions, getData(eval, "unknown"))
acc


#We will make 10 movies recommendations for the first 3 users
LIBMF_reco<- rec %>% predict(getData(eval, "known"), n= 10)
reco_users<- as(LIBMF_reco, "list") %>% head(3)

  ## Movies recommendations for UserId = 9
movielens_reprocessing %>% filter(movieId %in% reco_users[[1]]) %>% select(title, genres) %>% distinct()

  ## Movies recommendations for UserId = 12
movielens_reprocessing %>% filter(movieId %in% reco_users[[2]]) %>% select(title, genres) %>% distinct()

  ## Movies recommendations for UserId = 17
movielens_reprocessing %>% filter(movieId %in% reco_users[[3]]) %>% select(title, genres) %>% distinct()

invisible(invisible(gc())) # for cleaning memory

#####################################################
#                                                   #
#   Matrix factorization with recosystem lab        #
#                                                   #
#####################################################

# We will set a train and a test set
train_set <- edx %>% select(userId, movieId,rating) %>% as.matrix()
test_set <- validation %>% select(userId, movieId,rating) %>% as.matrix()

# We will save them on hard disk
write.table(train_set , file = "train_set.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(test_set, file = "test_set.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

invisible(invisible(gc())) # for cleaning memory


train_set <- data_file( "train_set.txt")
test_set <- data_file( "test_set.txt")

# We will create a model object
r = Reco()

# We will use tune function, which is based on cross validation, in order to select the best tuning parameter before training our predictive algorithm
set.seed(2022, sample.kind="Rounding")
opts = r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2), costp_l1 = 0, costq_l1 = 0, nthread = 1, niter = 10))
opts$min %>% as_tibble()

invisible(invisible(gc())) # for cleaning memory

# We will train our recommender model from train data set and we will save the result in the working directory
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20), out_model = file.path(getwd(), "model.txt"))

invisible(invisible(gc())) # for cleaning memory


#  We will use pred function to predict unknown ratings from the test set
r$predict(test_set, file.path(getwd(), "pred.txt"))

#We will show the 10 first predictions
print(scan("pred.txt", n= 10))

real_ratings <- read.table("test_set.txt", header = FALSE, sep = " ")$V3
pred_ratings <- scan("pred.txt")

result_recosystem <- RMSE(real_ratings,pred_ratings)
result_recosystem

invisible(invisible(gc())) # for cleaning memory
