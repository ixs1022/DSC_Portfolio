# DSC520 Final Project
# Chess Game Dataset (Lichess)
# Isabella Sturm
# 2021-02-28

# Load libraries
library(ggplot2)

# Load the data
setwd('/Users/isabellasturm/Documents/DataScience/DSC520Winter2020/ChessFinal/')
full_chess_df <- read.csv('games.csv')

# Clean the data
# Remove unneeded columns
chess_df <- subset(full_chess_df, select=c(id, created_at, last_move_at, turns,
                                         victory_status, winner, white_rating, 
                                         black_rating, moves, opening_eco, 
                                         opening_name, opening_ply))
# Set factors: victory_status, winner
chess_df$victory_status <- as.factor(chess_df$victory_status)
chess_df$winner <- as.factor(chess_df$winner)
# Set winner to numeric: white = 1, black = 0, draw = -1
chess_df$winner_num[chess_df$winner == "white"] <- 1
chess_df$winner_num[chess_df$winner == 'black'] <- 0
chess_df$winner_num[chess_df$winner == 'draw'] <- -1
chess_df$winner_num <- as.numeric(chess_df$winner_num)
# Set dates: created_at, last_move_at
chess_df$created_at <- as.Date.numeric(chess_df$created_at, origin='1970-01-01')
chess_df$last_move_at<- as.Date.numeric(chess_df$last_move_at, origin = '1970-01-01')
# Create new column: game_length, rating_diff
chess_df$game_length <- difftime(chess_df$last_move_at, chess_df$created_at, unit="secs")
chess_df$rating_diff <- chess_df$white_rating - chess_df$black_rating # negative if black is rated higher

# Get data summary
summary(chess_df)
hist(chess_df$winner_num, breaks=3)


# white_win_counts = table(chess_df$winner[chess_df$winner=='white'], chess_df$rating_diff[chess_df$winner=='white'])
# black_win_counts = table(chess_df$winner[chess_df$winner=='black'], chess_df$rating_diff[chess_df$winner=='black'])
# draw_win_counts = table(chess_df$winner[chess_df$winner=='draw'], chess_df$rating_diff[chess_df$winner=='draw'])
# barplot(white_win_counts)
# barplot(black_win_counts)
# barplot(draw_win_counts)
# 
# win_counts = table(chess_df$winner, chess_df$rating_diff)
# barplot(win_counts, main="Distribution of Wins based on Rating Difference",
#         xlab="Difference in Player Rating", col=c("blue", "black", "red"),
#         legend=rownames(win_counts), besides=TRUE)
# 
# win_counts = table(chess_df$winner, chess_df$rating_diff)
# barplot(win_counts, main="Distribution of Wins based on Rating Difference",
#         xlab="Difference in Player Rating", col=c("blue", "black", "red"),
#         legend=rownames(win_counts))

ggplot(data=chess_df, aes(x=rating_diff, y=turns, color=factor(victory_status))) +
         geom_point()    

ggplot(data=chess_df, aes(x=rating_diff, y=turns, color=factor(winner))) + 
        geom_point()

ggplot(data=chess_df, aes(x=winner, y=rating_diff)) +
        geom_point()

median(chess_df$black_rating)
median(chess_df$white_rating)

hist(chess_df$white_rating)
hist(chess_df$black_rating)


# Find correlation between game length and game duration
cor(data=chess_df, x=rating_diff, y=winner_num)

# Find covariance between rating difference and winner
cov(chess_df, x=rating_diff, y=winner_num)

# Find correlation between rating difference and winner
cor(data=chess_df, x=rating_diff, y=game_length)

# Find covariance between difference in player rating and game length (duration)
cov(data=chesss_df, x=rating_diff, y=game_length)

chess_lm = lm(winner_num ~ white_rating + black_rating + moves, data=chess_df)
summary(chess_lm)


