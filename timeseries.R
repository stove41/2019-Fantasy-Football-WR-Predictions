library(forecast)
library(data.table)
library(tseries)


#This is just to get player name list.
nfl_df = read.csv('NFL_WR_data_wk1-10.csv')
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

#Use only players from cluser 1 which has the highest pts/game.
cluster_one = normed_player_stats[which(normed_player_stats$cluster == 1),]
str(cluster_one)

selected_players = cluster_one[,1]
selected_players

sel_play = as.character(selected_players)
sel_play
sel_play = sel_play[-5]
sel_play = sel_play[-11]
sel_play = sel_play[-15]
sel_play = sel_play[-27]
sel_play



#Create player data frames

df_1 = read.csv('total_1.csv')
df_2 = read.csv('total_2.csv')
df_3 = read.csv('total_3.csv')
df_4 = read.csv('total_4.csv')
df_5 = read.csv('total_5.csv')
df_6 = read.csv('total_6.csv')
df_7 = read.csv('total_7.csv')
df_8 = read.csv('total_8.csv')
df_9 = read.csv('total_9.csv')
df_10 = read.csv('total_10.csv')
df_11 = read.csv('total_11.csv')
df_12 = read.csv('total_12.csv')
df_13 = read.csv('total_13.csv')
df_14 = read.csv('total_14.csv')
df_15 = read.csv('total_15.csv')
df_16 = read.csv('total_16.csv')
df_17 = read.csv('total_17.csv')
df_18 = read.csv('total_18.csv')
df_19 = read.csv('total_19.csv')
df_20 = read.csv('total_20.csv')
df_21 = read.csv('total_21.csv')
df_22 = read.csv('total_22.csv')
df_23 = read.csv('total_23.csv')
df_24 = read.csv('total_24.csv')
df_25 = read.csv('total_25.csv')
df_25 = na.omit(df_25)
df_26 = read.csv('total_26.csv')



#create player time series
ts_1 = ts(df_1[3], frequency = 3)
ts_2 = ts(df_2[3], frequency = 3)
ts_3 = ts(df_3[3], frequency = 3)
ts_4 = ts(df_4[3], frequency = 3)
ts_5 = ts(df_5[3], frequency = 3)
ts_6 = ts(df_6[3], frequency = 3)
ts_7 = ts(df_7[3], frequency = 3)
ts_8 = ts(df_8[3], frequency = 3)
ts_9 = ts(df_9[3], frequency = 3)
ts_10 = ts(df_10[3], frequency = 3)
ts_11 = ts(df_11[3], frequency = 3)
ts_12 = ts(df_12[3], frequency = 3)
ts_13 = ts(df_13[3], frequency = 3)
ts_14 = ts(df_14[3], frequency = 3)
ts_15 = ts(df_15[3], frequency = 3)
ts_16 = ts(df_16[3], frequency = 3)
ts_17 = ts(df_17[3], frequency = 3)
ts_18 = ts(df_18[3], frequency = 3)
ts_19 = ts(df_19[3], frequency = 3)
ts_20 = ts(df_20[3], frequency = 3)
ts_21 = ts(df_21[3], frequency = 3)
ts_22 = ts(df_22[3], frequency = 3)
ts_23 = ts(df_23[3], frequency = 3)
ts_24 = ts(df_24[3], frequency = 3)
ts_25 = ts(df_25[3], frequency = 3)
ts_26 = ts(df_26[3], frequency = 3)


###################################################
#make predictions:
mean_preds = array(NA, dim = 26)
total_preds = array(NA, dim = 26)
mean_preds_3 = array(NA, dim = 26)
total_preds_3 = array(NA, dim = 26)

adf.test(ts_1)
model = hw(ts_1, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[1] = mean_prediction
total_preds[1] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[1] = mean_3
total_preds_3[1] = total_3

adf.test(ts_2)
model = hw(ts_2, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[2] = mean_prediction
total_preds[2] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[2] = mean_3
total_preds_3[2] = total_3

adf.test(ts_3)
model = hw(ts_3, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[3] = mean_prediction
total_preds[3] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[3] = mean_3
total_preds_3[3] = total_3

adf.test(ts_4)
model = hw(ts_4, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[4] = mean_prediction
total_preds[4] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[4] = mean_3
total_preds_3[4] = total_3

adf.test(ts_5)
model = hw(ts_5, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[5] = mean_prediction
total_preds[5] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[5] = mean_3
total_preds_3[5] = total_3

adf.test(ts_6)
model = hw(ts_6, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[6] = mean_prediction
total_preds[6] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[6] = mean_3
total_preds_3[6] = total_3

adf.test(ts_7)
model = hw(ts_7, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[7] = mean_prediction
total_preds[7] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[7] = mean_3
total_preds_3[7] = total_3

adf.test(ts_8)
model = hw(ts_8, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[8] = mean_prediction
total_preds[8] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[8] = mean_3
total_preds_3[8] = total_3

adf.test(ts_9)
model = hw(ts_9, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[9] = mean_prediction
total_preds[9] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[9] = mean_3
total_preds_3[9] = total_3

adf.test(ts_10)
model = hw(ts_10, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[10] = mean_prediction
total_preds[10] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[10] = mean_3
total_preds_3[10] = total_3

adf.test(ts_11)
model = hw(ts_11, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[11] = mean_prediction
total_preds[11] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[11] = mean_3
total_preds_3[11] = total_3

adf.test(ts_12)
model = hw(ts_12, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[12] = mean_prediction
total_preds[12] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[12] = mean_3
total_preds_3[12] = total_3

adf.test(ts_13)
model = hw(ts_13, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[13] = mean_prediction
total_preds[13] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[13] = mean_3
total_preds_3[13] = total_3

adf.test(ts_14)
model = hw(ts_14, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[14] = mean_prediction
total_preds[14] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[14] = mean_3
total_preds_3[14] = total_3

adf.test(ts_15)
model = hw(ts_15, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[15] = mean_prediction
total_preds[15] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[15] = mean_3
total_preds_3[15] = total_3

adf.test(ts_16)
model = hw(ts_16, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[16] = mean_prediction
total_preds[16] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[16] = mean_3
total_preds_3[16] = total_3

adf.test(ts_17)
model = hw(ts_17, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[17] = mean_prediction
total_preds[17] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[17] = mean_3
total_preds_3[17] = total_3

adf.test(ts_18)
model = hw(ts_18, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[18] = mean_prediction
total_preds[18] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[18] = mean_3
total_preds_3[18] = total_3

adf.test(ts_19)
model = hw(ts_19, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[19] = mean_prediction
total_preds[19] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[19] = mean_3
total_preds_3[19] = total_3

adf.test(ts_20)
model = hw(ts_20, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[20] = mean_prediction
total_preds[20] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[20] = mean_3
total_preds_3[20] = total_3

adf.test(ts_21)
model = hw(ts_21, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[21] = mean_prediction
total_preds[21] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[21] = mean_3
total_preds_3[21] = total_3

adf.test(ts_22)
model = hw(ts_22, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[22] = mean_prediction
total_preds[22] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[22] = mean_3
total_preds_3[22] = total_3

adf.test(ts_23)
model = hw(ts_23, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[23] = mean_prediction
total_preds[23] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[23] = mean_3
total_preds_3[23] = total_3

adf.test(ts_24)
model = hw(ts_24, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[24] = mean_prediction
total_preds[24] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[24] = mean_3
total_preds_3[24] = total_3

adf.test(ts_25)
model = hw(ts_25, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[25] = mean_prediction
total_preds[25] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[25] = mean_3
total_preds_3[25] = total_3

adf.test(ts_26)
model = hw(ts_26, h=6)
plot(model)
hwsummary = summary(model)
mean_prediction = mean(hwsummary$`Point Forecast`)
total_prediction = sum(hwsummary$`Point Forecast`)
mean_preds[26] = mean_prediction
total_preds[26] = total_prediction
mean_3 = mean(hwsummary$`Point Forecast`[1:3])
total_3 = sum(hwsummary$`Point Forecast`[1:3])
mean_preds_3[26] = mean_3
total_preds_3[26] = total_3




#Get test data:
test_df = read.csv('NFL_WR_data_2019_wk11-13.csv')

sel_play
str(test_df)
test_df$player = as.character(test_df$player)
player_indices = matrix(NA, nrow = 30, ncol = 10)
for(i in 1:length(sel_play)){
    indices = which(test_df[,1] == sel_play[i])
    for(j in 1:length(indices)){
        player_indices[i,j] = indices[j]
    }
}

player_indices
for(i in 1:26) { 
 nam <- paste("df", i, sep = "_")
 assign(nam, test_df[player_indices[i,],])
}

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}


df_1
df_1 = na.omit(df_1)
df_1
mean_df1 = mean(df_1[,3])
mean_df1



df_2
df_2 = na.omit(df_2)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_2 = insertRow(df_2, new_row, 2)
df_2$pts = as.numeric(df_2$pts)
mean_df2 = mean(df_2[,3])
str(df_2)
mean_df2


df_3 = na.omit(df_3)
mean_df3 = mean(df_3$pts)
mean_df3


df_4
df_4 = na.omit(df_4)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_4 = insertRow(df_4, new_row, 2)
df_4$pts = as.numeric(df_4$pts)
mean_df4 = mean(df_4$pts)
mean_df4



df_5
df_5 = na.omit(df_5)
mean_df5 = mean(df_5$pts)
mean_df5


df_6
df_6 = na.omit(df_6)
mean_df6 = mean(df_6$pts)
mean_df6


df_7
df_7 = na.omit(df_7)
mean_df7 = mean(df_7$pts)
mean_df7


df_8
df_8 = na.omit(df_8)
df_8
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11)
df_8 = insertRow(df_8, new_row, 1)
str(df_8)
df_8$pts = as.numeric(df_8$pts)
mean_df8 = mean(df_8$pts)
mean_df8


df_9
df_9 = na.omit(df_9)
mean_df9 = mean(df_9$pts)
mean_df9



df_10
df_10 = na.omit(df_10)
df_10
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11)
df_10 = insertRow(df_10, new_row, 1)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_10 = insertRow(df_10, new_row, 2)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13)
df_10 = insertRow(df_10, new_row, 3)
df_10$pts = as.numeric(df_10$pts)
mean_df10 = mean(df_10$pts)
mean_df10 = 0


df_11
df_11 = na.omit(df_11)
mean_df11 = mean(df_11$pts)
mean_df11



df_12
df_12 = na.omit(df_12)
mean_df12 = mean(df_12$pts)
mean_df12


df_13
mean_df13 = 0


df_14
df_14 = na.omit(df_14)
mean_df14 = mean(df_14$pts)
mean_df14



df_15
df_15 = na.omit(df_15)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_15 = insertRow(df_15, new_row, 2)
df_15$pts = as.numeric(df_15$pts)
mean_df15 = mean(df_15$pts)
mean_df15


df_16
df_16 = na.omit(df_16)
mean_df16 = mean(df_16$pts)
mean_df16


df_17
df_17 = na.omit(df_17)
mean_df17 = mean(df_17$pts)
mean_df17


df_18
df_18 = na.omit(df_18)
mean_df18 = mean(df_18$pts)
mean_df18



df_19
df_19 = na.omit(df_19)
mean_df19 = mean(df_19$pts)
mean_df19


df_20
df_20 = na.omit(df_20)
mean_df20 = mean(df_20$pts)
mean_df20


df_21
df_21 = na.omit(df_21)
mean_df21 = mean(df_21$pts)
mean_df21


df_22
df_22 = na.omit(df_22)
mean_df22 = 4/3
mean_df22


df_23
df_23 = na.omit(df_23)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12)
df_23 = insertRow(df_23, new_row, 2)
df_23$pts = as.numeric(df_23$pts)
mean_df23 = mean(df_23$pts)
mean_df23


df_24
df_24 = na.omit(df_24)
new_row = c('Player', 'Game', 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11)
df_24 = insertRow(df_24, new_row, 1)
df_24$pts = as.numeric(df_24$pts)
mean_df24 = mean(df_24$pts)
mean_df24


df_25
df_25 = na.omit(df_25)
mean_df25 = mean(df_25$pts)
mean_df25


df_26
df_26 = na.omit(df_26)
mean_df26 = 4/3
mean_df26




mean_preds
total_preds
mean_preds_3
total_preds_3

test_avg = array(c(mean_df1, mean_df2, mean_df3, mean_df4, mean_df5, mean_df6, mean_df7, mean_df8, mean_df9, mean_df10, mean_df11, mean_df12, mean_df13, mean_df14, mean_df15, mean_df16, mean_df17, mean_df18, mean_df19, mean_df20, mean_df21, mean_df22, mean_df23, mean_df24, mean_df25, mean_df26))
test_avg


final_df = data.frame(sel_play, mean_preds, total_preds, mean_preds_3, test_avg, total_preds_3)
final_df
cols = c('Player', 'avg_6', 'total_6', 'avg_3', 'test_avg', 'total_3')
colnames(final_df) = cols


sorted_by_average6 = final_df[order(-final_df$avg_6),]
sorted_by_total6 = final_df[order(-final_df$total_6),]
sorted_by_average3 = final_df[order(-final_df$avg_3),]
sorted_by_total3 = final_df[order(-final_df$total_3),]

sorted_by_average6


#Visualize predictions
plot(1:26, sorted_by_average3$avg_3, cex = 1, pch = 19, col = 'blue', ylim = c(0, 25), ylab = 'Points', xlab = 'Players', main = 'Predicted vs Actual Scores')
points(1:26, sorted_by_average3$test_avg, cex = 1, pch = 19, col = 'red')
legend('topright', legend=c("Predicted", "Actual"), col=c('blue', "red"), pch = 19, cex=0.8)

#Calculate MAD
resids = sorted_by_average3$test_avg - sorted_by_average3$avg_3
resids
MAD = mean(abs(resids))
MAD
#Calculate test_range
range(sorted_by_average3$test_avg)
