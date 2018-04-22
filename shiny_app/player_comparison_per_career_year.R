library(ggplot2)
library(dplyr)
library(htmltab)
library(bbr)


names <- get_players("A")

out=NULL
for (i in 1:2){
  a <- c(2, (i+10))
  b <- c((i+10), 5)
  c <- data.frame(a,b)
  out=rbind(out,c)
}
out


df_players = data.frame()
for (i in  LETTERS){
  
  # vector output
  players <- try(get_players(i))
  df_players <- rbind(df_players,players)
}

df_players$name <- paste0(sub("[a-zA-Z]+ ", "", df_players$player)," ",sub(" [a-zA-Z]+", "", df_players$player))


write.csv(df_players,"players.csv", row.names = F)
players <- read.csv("players.csv")



a_data <- htmltab(doc = "https://www.basketball-reference.com/players/j/jamesle01.html", which = 1, header = 1,rm_nodata_cols = F)
b_date <- htmltab(doc = "https://www.basketball-reference.com/players/b/bryanko01.html", which = 1, header = 1,rm_nodata_cols = F)

a_per <- a_data
b_per <- b_date

a_per$name <- "Lebron James"
b_per$name <- "Kobe Bryant"

a_per <- a_per %>%  mutate(no_year = 1:n(),name = "Lebron James") %>% filter (Lg == "NBA")
b_per <- b_per %>%  mutate(no_year = 1:n(),name = "Kobe Bryant") %>% filter (Lg == "NBA")

per <- rbind(a_per,b_per)

#function to remove multiple spaces
colClean <- function(x){ colnames(x) <- gsub("%", "_PER", colnames(x)); x } 

per <- colClean(per)
per[,6:30] <- lapply(per[,6:30], function (x) as.numeric(x))


param <- 'eFG%'
ytitle = 'year of career'
xtitle = param
testParam <- paste('',param,'',sep="")
min <- min(TOV,na.rm = T)
max <- max(TOV,na.rm = T)

ggplot(data=per, aes(x=no_year, y = TOV,group = name)) +
  geom_line(aes(color=name),size = 2)+
  geom_point(aes(color=name))+
  theme_light(base_size = 11, base_family = "")+
  scale_color_manual(values=c("darkorange","dodgerblue"))+
  #scale_linetype_manual(values=c("solid", "solid"))+
  scale_x_continuous(breaks = seq(0,100,by=1))+
  labs(y = xtitle,
       x = ytitle)



  





