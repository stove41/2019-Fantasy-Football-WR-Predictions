nfl_df = read.csv('NFL_WR_data_wk1-10.csv')
head(nfl_df)
str(nfl_df)

#Group data by player and sum each column for total stats for the first 10 games.
#this will be fed into kmeans to determine different caliber WRs.
player_stats = aggregate(. ~ player, nfl_df, sum)
player_counts = aggregate(. ~ player, nfl_df, length)
games_played = player_counts[,2]
player_stats
players = player_stats[1]
str(players)
player_stats = player_stats[-1]
player_counts = player_counts[-1]
player_stats = player_stats[-19]
player_counts = player_counts[-19]
normed_player_stats = player_stats / player_counts
normed_player_stats = normed_player_stats[-1]
normed_player_stats = cbind(players, normed_player_stats, games_played)
normed_player_stats

set.seed(0)
clusters <- kmeans(normed_player_stats[,2:19], 4)
cluster = clusters$cluster
normed_player_stats = cbind(normed_player_stats, cluster)
clusters$centers
normed_player_stats
clusters$size
plot(normed_player_stats$rec_yds, normed_player_stats$pts, col=normed_player_stats$cluster, main = 'Wide Receiver Clusters', xlab = 'Receiving Yards', ylab = 'Points Scored', pch = 19, cex = 1)


#Use only players from cluser 1 which has the highest pts/game.
cluster_one = normed_player_stats[which(normed_player_stats$cluster == 1),]
str(cluster_one)

selected_players = cluster_one[,1]
selected_players
nfl_df
player_indices = matrix(NA, nrow = 30, ncol = 10)
for(i in 1:length(selected_players)){
    indices = which(nfl_df[,1] == selected_players[i])
    for(j in 1:length(indices)){
        player_indices[i,j] = indices[j]
    }
}
player_indices
#################################################################################################
#Create dataframes for each of the 30 players in cluster 1 to do timeseries predictions for pts.
for(i in 1:30) { 
 nam <- paste("df", i, sep = "_")
 assign(nam, nfl_df[player_indices[i,],])
}

#Function to insert row at specific index
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

df_1
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_1 = insertRow(df_1, new_row, 6)
df_1
df_1 = df_1[-11,]
df_1



df_2
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2 = insertRow(df_2, new_row, 8)
df_2
df_2 = df_2[-11,]



df_3
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_3 = insertRow(df_3, new_row, 7)
df_3
df_3 = df_3[-11,]



df_4
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_4 = insertRow(df_4, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_4 = insertRow(df_4, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_4 = insertRow(df_4, new_row, 7)
df_4 = df_4[-c(11,12,13),]
df_4

df_5
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_5 = insertRow(df_5, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_5 = insertRow(df_5, new_row, 10)
df_5
df_5 = df_5[-c(11,12),]

df_6
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_6 = insertRow(df_6, new_row, 10)
df_6
df_6 = df_6[-11,]


df_7
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_7 = insertRow(df_7, new_row, 10)
df_7
df_7 = df_7[-11,]


df_8
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_8 = insertRow(df_8, new_row, 7)
df_8
df_8 = df_8[-11,]

df_9
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_9 = insertRow(df_9, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_9 = insertRow(df_9, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_9 = insertRow(df_9, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_9 = insertRow(df_9, new_row, 8)
df_9
df_9 = df_9[-c(11:14),]

df_10
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_10 = insertRow(df_10, new_row, 10)
df_10
df_10 = df_10[-11,]

df_11
#Desean Jackson IR.

df_12
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
df_12 = insertRow(df_12, new_row, 1)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)
df_12 = insertRow(df_12, new_row, 2)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3)
df_12 = insertRow(df_12, new_row, 3)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4)
df_12 = insertRow(df_12, new_row, 4)
df_12
df_12 = df_12[-c(11:14),]


df_13
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_13 = insertRow(df_13, new_row, 7)
df_13
df_13 = df_13[-11,]


df_14
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_14 = insertRow(df_14, new_row, 6)
df_14
df_14 = df_14[-11,]

df_15
#John ross IR


df_16
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_16 = insertRow(df_16, new_row, 10)
df_16
df_16 = df_16[-11,]

df_17
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_17 = insertRow(df_17, new_row, 9)
df_17
df_17 = df_17[-11,]

df_18

df_19
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_19 = insertRow(df_19, new_row, 5)
df_19
df_19 = df_19[-11,]

df_20
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_20 = insertRow(df_20, new_row, 5)
df_20
df_20 = df_20[-11,]


df_21
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3)
df_21 = insertRow(df_21, new_row, 3)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4)
df_21 = insertRow(df_21, new_row, 4)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_21 = insertRow(df_21, new_row, 8)
df_21
df_21 = df_21[-c(11:13),]


df_22
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_22 = insertRow(df_22, new_row, 9)
df_22
df_22 = df_22[-11,]


df_23
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_23 = insertRow(df_23, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_23 = insertRow(df_23, new_row, 7)
df_23
df_23 = df_23[-c(11:12),]


df_24
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_24 = insertRow(df_24, new_row, 7)
df_24
df_24 = df_24[-11,]


df_25
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_25 = insertRow(df_25, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_25 = insertRow(df_25, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_25 = insertRow(df_25, new_row, 7)
df_25
df_25 = df_25[-c(11:13),]


df_26


df_27
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)
df_27 = insertRow(df_27, new_row, 2)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_27 = insertRow(df_27, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_27 = insertRow(df_27, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_27 = insertRow(df_27, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_27 = insertRow(df_27, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_27 = insertRow(df_27, new_row, 10)
df_27
df_27 = df_27[-c(11:16),]


df_28
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_28 = insertRow(df_28, new_row, 9)
df_28
df_28 = df_28[-11,]


df_29


df_30
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)
df_30 = insertRow(df_30, new_row, 2)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3)
df_30 = insertRow(df_30, new_row, 3)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4)
df_30 = insertRow(df_30, new_row, 4)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_30 = insertRow(df_30, new_row, 5)
df_30
df_30 = df_30[-c(11:14),]


#######################################################################
#check each df, if the player is out in week 9 and 10, we will not consider them.


for(i in seq_len(30)) {
     if(is.na(get(paste("df", i, sep = '_'))[9,1]) & is.na(get(paste("df", i, sep = '_'))[10,1])){
         print(i)
     }
 }

sel_play = selected_players
sel_play = sel_play[-5]
sel_play = sel_play[-11]
sel_play = sel_play[-15]
sel_play = sel_play[-27]
sel_play

######################################################################################################
#Get 2017 and 2018 data from csv.
nfl_2018 = read.csv('NFL_WR_data_2018_wk1-16.csv', stringsAsFactors = FALSE)
head(nfl_2018)
str(nfl_2018)
sel_play = as.character(sel_play)



player_indices_2018 = matrix(NA, nrow = 26, ncol = 16)
for(i in 1:length(sel_play)){
    indices = which(nfl_2018[,1] == sel_play[i])
    for(j in 1:length(indices)){
        player_indices_2018[i,j] = indices[j]
    }
}
player_indices_2018


#################################################################################################
#Create dataframes for each of the 30 players in cluster 1 to do timeseries predictions for pts.
for(i in 1:26) { 
 nam <- paste("df_2018", i, sep = "_")
 assign(nam, nfl_2018[player_indices_2018[i,],])
}


df_2018_1
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_2018_1 = insertRow(df_2018_1, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2018_1 = insertRow(df_2018_1, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2018_1 = insertRow(df_2018_1, new_row, 9)
df_2018_1
df_2018_1 = df_2018_1[-c(17:19),]


df_2018_2
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2018_2 = insertRow(df_2018_2, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2018_2 = insertRow(df_2018_2, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2018_2 = insertRow(df_2018_2, new_row, 8)
df_2018_2
df_2018_2 = df_2018_2[-c(17:19),]


df_2018_3
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_2018_3 = insertRow(df_2018_3, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2018_3 = insertRow(df_2018_3, new_row, 15)
df_2018_3
df_2018_3 = df_2018_3[-c(17:18),]

df_2018_4
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2018_4 = insertRow(df_2018_4, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2018_4 = insertRow(df_2018_4, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2018_4 = insertRow(df_2018_4, new_row, 15)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2018_4 = insertRow(df_2018_4, new_row, 16)
df_2018_4
df_2018_4 = df_2018_4[-c(17:20),]


df_2018_5
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2018_5 = insertRow(df_2018_5, new_row, 10)
df_2018_5
df_2018_5 = df_2018_5[-17,]


df_2018_6
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
df_2018_6 = insertRow(df_2018_6, new_row, 1)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3)
df_2018_6 = insertRow(df_2018_6, new_row, 3)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2018_6 = insertRow(df_2018_6, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2018_6 = insertRow(df_2018_6, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2018_6 = insertRow(df_2018_6, new_row, 10)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_2018_6 = insertRow(df_2018_6, new_row, 12)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_2018_6 = insertRow(df_2018_6, new_row, 13)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2018_6 = insertRow(df_2018_6, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2018_6 = insertRow(df_2018_6, new_row, 15)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2018_6 = insertRow(df_2018_6, new_row, 16)
df_2018_6
df_2018_6 = df_2018_6[-c(17:26),]


df_2018_7
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4)
df_2018_7 = insertRow(df_2018_7, new_row, 4)
df_2018_7
df_2018_7 = df_2018_7[-17,]


df_2018_8
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2018_8 = insertRow(df_2018_8, new_row, 7)
df_2018_8
df_2018_8 = df_2018_8[-17,]


df_2018_9
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2018_9 = insertRow(df_2018_9, new_row, 10)
df_2018_9 = df_2018_9[-17,]


df_2018_10
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_2018_10 = insertRow(df_2018_10, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_2018_10 = insertRow(df_2018_10, new_row, 13)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2018_10 = insertRow(df_2018_10, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2018_10 = insertRow(df_2018_10, new_row, 15)
df_2018_10 = df_2018_10[-c(17:20),]


df_2018_11
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11)
df_2018_11 = insertRow(df_2018_11, new_row, 11)
df_2018_11 = df_2018_11[-17,]


df_2018_12
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2018_12 = insertRow(df_2018_12, new_row, 10)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_2018_12 = insertRow(df_2018_12, new_row, 13)
df_2018_12 = df_2018_12[-c(17:18),]


df_2018_13
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_2018_13 = insertRow(df_2018_13, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2018_13 = insertRow(df_2018_13, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2018_13 = insertRow(df_2018_13, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2018_13 = insertRow(df_2018_13, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2018_13 = insertRow(df_2018_13, new_row, 9)
df_2018_13 = df_2018_13[-c(17:21),]


df_2018_14
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
df_2018_14 = insertRow(df_2018_14, new_row, 1)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)
df_2018_14 = insertRow(df_2018_14, new_row, 2)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3)
df_2018_14 = insertRow(df_2018_14, new_row, 3)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4)
df_2018_14 = insertRow(df_2018_14, new_row, 4)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11)
df_2018_14 = insertRow(df_2018_14, new_row, 11)
df_2018_14 = df_2018_14[-c(17:21),]


df_2018_15
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2018_15 = insertRow(df_2018_15, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2018_15 = insertRow(df_2018_15, new_row, 15)
df_2018_15 = df_2018_15[-c(17:18),]


df_2018_16
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2018_16 = insertRow(df_2018_16, new_row, 6)
df_2018_16 = df_2018_16[-c(17),]



df_2018_17
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2018_17 = insertRow(df_2018_17, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11)
df_2018_17 = insertRow(df_2018_17, new_row, 11)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_2018_17 = insertRow(df_2018_17, new_row, 12)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_2018_17 = insertRow(df_2018_17, new_row, 13)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2018_17 = insertRow(df_2018_17, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2018_17 = insertRow(df_2018_17, new_row, 15)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2018_17 = insertRow(df_2018_17, new_row, 16)
df_2018_17 = df_2018_17[-c(17:23),]



df_2018_18
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2018_18 = insertRow(df_2018_18, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2018_18 = insertRow(df_2018_18, new_row, 15)
df_2018_18 = df_2018_18[-c(17:18),]


df_2018_19
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2018_19 = insertRow(df_2018_19, new_row, 6)
df_2018_19 = df_2018_19[-c(17),]


df_2018_20
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_2018_20 = insertRow(df_2018_20, new_row, 5)
df_2018_20 = df_2018_20[-c(17),]


df_2018_21
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2018_21 = insertRow(df_2018_21, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2018_21 = insertRow(df_2018_21, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2018_21 = insertRow(df_2018_21, new_row, 15)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2018_21 = insertRow(df_2018_21, new_row, 16)
df_2018_21 = df_2018_21[-c(17:20),]


df_2018_22
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4)
df_2018_22 = insertRow(df_2018_22, new_row, 4)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2018_22 = insertRow(df_2018_22, new_row, 10)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_2018_22 = insertRow(df_2018_22, new_row, 12)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_2018_22 = insertRow(df_2018_22, new_row, 13)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2018_22 = insertRow(df_2018_22, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2018_22 = insertRow(df_2018_22, new_row, 15)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2018_22 = insertRow(df_2018_22, new_row, 16)
df_2018_22 = df_2018_22[-c(17:23),]


df_2018_23
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2018_23 = insertRow(df_2018_23, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2018_23 = insertRow(df_2018_23, new_row, 10)
df_2018_23 = df_2018_23[-c(17:18),]


df_2018_24
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2018_24 = insertRow(df_2018_24, new_row, 9)
df_2018_24 = df_2018_24[-c(17),]


df_2018_25
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2018_25 = insertRow(df_2018_25, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2018_25 = insertRow(df_2018_25, new_row, 16)
df_2018_25 = df_2018_25[-c(17:18),]



df_2018_26
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2018_26 = insertRow(df_2018_26, new_row, 7)
df_2018_26 = df_2018_26[-c(17),]

######################################################################################################
#Get 2017 data from csv.
nfl_2017 = read.csv('NFL_WR_data_2017_wk1-16.csv', stringsAsFactors = FALSE)
head(nfl_2017)
str(nfl_2017)

player_indices_2017 = matrix(NA, nrow = 26, ncol = 16)
for(i in 1:length(sel_play)){
    indices = which(nfl_2017[,1] == sel_play[i])
    for(j in 1:length(indices)){
        player_indices_2017[i,j] = indices[j]
    }
}
player_indices_2017


#################################################################################################
#Create dataframes for each of the 30 players in cluster 1 to do timeseries predictions for pts.
for(i in 1:26) { 
 nam <- paste("df_2017", i, sep = "_")
 assign(nam, nfl_2017[player_indices_2017[i,],])
}



df_2017_1
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)
df_2017_1 = insertRow(df_2017_1, new_row, 2)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3)
df_2017_1 = insertRow(df_2017_1, new_row, 3)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4)
df_2017_1 = insertRow(df_2017_1, new_row, 4)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_2017_1 = insertRow(df_2017_1, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2017_1 = insertRow(df_2017_1, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2017_1 = insertRow(df_2017_1, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2017_1 = insertRow(df_2017_1, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2017_1 = insertRow(df_2017_1, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2017_1 = insertRow(df_2017_1, new_row, 10)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11)
df_2017_1 = insertRow(df_2017_1, new_row, 11)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_2017_1 = insertRow(df_2017_1, new_row, 12)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_2017_1 = insertRow(df_2017_1, new_row, 13)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2017_1 = insertRow(df_2017_1, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2017_1 = insertRow(df_2017_1, new_row, 15)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2017_1 = insertRow(df_2017_1, new_row, 16)
df_2017_1 = df_2017_1[-c(17:31),]


df_2017_2
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2017_2 = insertRow(df_2017_2, new_row, 10)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_2017_2 = insertRow(df_2017_2, new_row, 13)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2017_2 = insertRow(df_2017_2, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2017_2 = insertRow(df_2017_2, new_row, 15)
df_2017_2 = df_2017_2[-c(17:20),]


df_2017_3
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
df_2017_3 = insertRow(df_2017_3, new_row, 1)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)
df_2017_3 = insertRow(df_2017_3, new_row, 2)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4)
df_2017_3 = insertRow(df_2017_3, new_row, 4)
df_2017_3 = df_2017_3[-c(17:19),]


df_2017_4
df_2017_4[is.na(df_2017_4)] = 0


df_2017_5
df_2017_5[is.na(df_2017_5)] = 0


df_2017_6
df_2017_6[is.na(df_2017_6)] = 0


df_2017_7
df_2017_7[is.na(df_2017_7)] = 0


df_2017_8
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2017_8 = insertRow(df_2017_8, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2017_8 = insertRow(df_2017_8, new_row, 16)
df_2017_8 = df_2017_8[-c(17:18),]


df_2017_9
anew_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2017_9 = insertRow(df_2017_9, new_row, 7)
df_2017_9 = df_2017_9[-c(17),]


df_2017_10
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
df_2017_10 = insertRow(df_2017_10, new_row, 1)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2017_10 = insertRow(df_2017_10, new_row, 16)
df_2017_10 = df_2017_10[-c(17:18),]


df_2017_11
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
df_2017_11 = insertRow(df_2017_11, new_row, 1)
df_2017_11 = df_2017_11[-c(17),]


df_2017_12
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)
df_2017_12 = insertRow(df_2017_12, new_row, 2)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3)
df_2017_12 = insertRow(df_2017_12, new_row, 3)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2017_12 = insertRow(df_2017_12, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_2017_12 = insertRow(df_2017_12, new_row, 12)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_2017_12 = insertRow(df_2017_12, new_row, 13)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2017_12 = insertRow(df_2017_12, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2017_12 = insertRow(df_2017_12, new_row, 15)
df_2017_12 = df_2017_12[-c(17:23),]


df_2017_13
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
df_2017_13 = insertRow(df_2017_13, new_row, 1)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3)
df_2017_13 = insertRow(df_2017_13, new_row, 3)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4)
df_2017_13 = insertRow(df_2017_13, new_row, 4)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_2017_13 = insertRow(df_2017_13, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2017_13 = insertRow(df_2017_13, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2017_13 = insertRow(df_2017_13, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2017_13 = insertRow(df_2017_13, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2017_13 = insertRow(df_2017_13, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2017_13 = insertRow(df_2017_13, new_row, 10)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11)
df_2017_13 = insertRow(df_2017_13, new_row, 11)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_2017_13 = insertRow(df_2017_13, new_row, 12)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_2017_13 = insertRow(df_2017_13, new_row, 13)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2017_13 = insertRow(df_2017_13, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2017_13 = insertRow(df_2017_13, new_row, 15)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2017_13 = insertRow(df_2017_13, new_row, 16)
df_2017_13 = df_2017_13[-c(17:31),]


df_2017_14
df_2017_14[is.na(df_2017_14)] = 0


df_2017_15
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2017_15 = insertRow(df_2017_15, new_row, 9)
df_2017_15 = df_2017_15[-c(17),]


df_2017_16
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4)
df_2017_16 = insertRow(df_2017_16, new_row, 4)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_2017_16 = insertRow(df_2017_16, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2017_16 = insertRow(df_2017_16, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2017_16 = insertRow(df_2017_16, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2017_16 = insertRow(df_2017_16, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2017_16 = insertRow(df_2017_16, new_row, 9)
df_2017_16 = df_2017_16[-c(17:22),]


df_2017_17
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2017_17 = insertRow(df_2017_17, new_row, 7)
df_2017_17 = df_2017_17[-c(17),]


df_2017_18
df_2017_18[is.na(df_2017_18)] = 0


df_2017_19
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_2017_19 = insertRow(df_2017_19, new_row, 5)
df_2017_19 = df_2017_19[-c(17),]


df_2017_20
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
df_2017_20 = insertRow(df_2017_20, new_row, 1)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2017_20 = insertRow(df_2017_20, new_row, 10)
df_2017_20 = df_2017_20[-c(17:18),]


df_2017_21
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
df_2017_21 = insertRow(df_2017_21, new_row, 1)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2017_21 = insertRow(df_2017_21, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2017_21 = insertRow(df_2017_21, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2017_21 = insertRow(df_2017_21, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2017_21 = insertRow(df_2017_21, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2017_21 = insertRow(df_2017_21, new_row, 10)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11)
df_2017_21 = insertRow(df_2017_21, new_row, 11)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_2017_21 = insertRow(df_2017_21, new_row, 12)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_2017_21 = insertRow(df_2017_21, new_row, 13)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14)
df_2017_21 = insertRow(df_2017_21, new_row, 14)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15)
df_2017_21 = insertRow(df_2017_21, new_row, 15)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16)
df_2017_21 = insertRow(df_2017_21, new_row, 16)
df_2017_21 = df_2017_21[-c(17:28),]


df_2017_22
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5)
df_2017_22 = insertRow(df_2017_22, new_row, 5)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2017_22 = insertRow(df_2017_22, new_row, 8)
df_2017_22 = df_2017_22[-c(17:18),]


df_2017_23
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2017_23 = insertRow(df_2017_23, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2017_23 = insertRow(df_2017_23, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2017_23 = insertRow(df_2017_23, new_row, 9)
df_2017_23 = df_2017_23[-c(17:19),]



df_2017_24
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2017_24 = insertRow(df_2017_24, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2017_24 = insertRow(df_2017_24, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2017_24 = insertRow(df_2017_24, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11)
df_2017_24 = insertRow(df_2017_24, new_row, 11)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_2017_24 = insertRow(df_2017_24, new_row, 12)
df_2017_24 = df_2017_24[-c(17:21),]


df_2017_25
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2)
df_2017_25 = insertRow(df_2017_25, new_row, 2)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2017_25 = insertRow(df_2017_25, new_row, 6)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7)
df_2017_25 = insertRow(df_2017_25, new_row, 7)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8)
df_2017_25 = insertRow(df_2017_25, new_row, 8)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9)
df_2017_25 = insertRow(df_2017_25, new_row, 9)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10)
df_2017_25 = insertRow(df_2017_25, new_row, 10)
df_2017_25 = df_2017_25[-c(17:22),]


df_2017_26
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6)
df_2017_26 = insertRow(df_2017_26, new_row, 6)
df_2017_26 = df_2017_26[-c(17),]



####################################################################
#Combine data frames to create 3 yrs of data for each player.



total_1 = rbind(df_2017_1, df_2018_1, df_1)
write.csv(total_1, 'total_1.csv', row.names = FALSE)
total_2 = rbind(df_2017_2, df_2018_2, df_2)
write.csv(total_2, 'total_2.csv', row.names = FALSE)
total_3 = rbind(df_2017_3, df_2018_3, df_3)
write.csv(total_3, 'total_3.csv', row.names = FALSE)
total_4 = rbind(df_2017_4, df_2018_4, df_4)
write.csv(total_4, 'total_4.csv', row.names = FALSE)
total_5 = rbind(df_2017_5, df_2018_5, df_6)
write.csv(total_5, 'total_5.csv', row.names = FALSE)
total_6 = rbind(df_2017_6, df_2018_6, df_7)
write.csv(total_6, 'total_6.csv', row.names = FALSE)
total_7 = rbind(df_2017_7, df_2018_7, df_8)
write.csv(total_7, 'total_7.csv', row.names = FALSE)
total_8 = rbind(df_2017_8, df_2018_8, df_9)
write.csv(total_8, 'total_8.csv', row.names = FALSE)
total_9 = rbind(df_2017_9, df_2018_9, df_10)
write.csv(total_9, 'total_9.csv', row.names = FALSE)
total_10 = rbind(df_2017_10, df_2018_10, df_12)
write.csv(total_10, 'total_10.csv', row.names = FALSE)
total_11 = rbind(df_2017_11, df_2018_11, df_13)
write.csv(total_11, 'total_11.csv', row.names = FALSE)
total_12 = rbind(df_2017_12, df_2018_12, df_14)
write.csv(total_12, 'total_12.csv', row.names = FALSE)
total_13 = rbind(df_2017_13, df_2018_13, df_16)
write.csv(total_13, 'total_13.csv', row.names = FALSE)
total_14 = rbind(df_2017_14, df_2018_14, df_17)
write.csv(total_14, 'total_14.csv', row.names = FALSE)
total_15 = rbind(df_2017_15, df_2018_15, df_18)
write.csv(total_15, 'total_15.csv', row.names = FALSE)
total_16 = rbind(df_2017_16, df_2018_16, df_19)
write.csv(total_16, 'total_16.csv', row.names = FALSE)
total_17 = rbind(df_2017_17, df_2018_17, df_20)
write.csv(total_17, 'total_17.csv', row.names = FALSE)
total_18 = rbind(df_2017_18, df_2018_18, df_21)
write.csv(total_18, 'total_18.csv', row.names = FALSE)
total_19 = rbind(df_2017_19, df_2018_19, df_22)
write.csv(total_19, 'total_19.csv', row.names = FALSE)
total_20 = rbind(df_2017_20, df_2018_20, df_23)
write.csv(total_20, 'total_20.csv', row.names = FALSE)
total_21 = rbind(df_2017_21, df_2018_21, df_24)
write.csv(total_21, 'total_21.csv', row.names = FALSE)
total_22 = rbind(df_2017_22, df_2018_22, df_25)
write.csv(total_22, 'total_22.csv', row.names = FALSE)
total_23 = rbind(df_2017_23, df_2018_23, df_26)
write.csv(total_23, 'total_23.csv', row.names = FALSE)
total_24 = rbind(df_2017_24, df_2018_24, df_28)
write.csv(total_24, 'total_24.csv', row.names = FALSE)
total_25 = rbind(df_2017_25, df_2018_25, df_29)
write.csv(total_25, 'total_25.csv', row.names = FALSE)
total_26 = rbind(df_2017_26, df_2018_26, df_30)
write.csv(total_26, 'total_26.csv', row.names = FALSE)