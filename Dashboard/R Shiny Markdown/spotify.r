#install.packages("pacman")
library(pacman)
pacman::p_load(pacman,dplyr, ggplot2, plotly,tidyr, rio, gridExtra, scales, ggcorrplot, caret, e1071)

getwd()
setwd('C:\\Users\\hp\\Desktop\\AI\\R Programming\\Dashboard\\R Shiny Markdown')


# CSV
df <- import("./top2018.csv")
head(df)

# View in table format the dataset
View(df)

# List all columns names in the dataset
names(df)

# Inspect the number of observables, features and data types in the dataset
str(df)

# Checking for Missing values
missing_values <- df %>% summarize_all(funs(sum(is.na(.))/n()))
missing_values
missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  
  geom_bar(stat="identity",fill="red")+
  
  coord_flip()+theme_bw()

corr <- round(cor(df[,4:16]), 8)
#head(df[,4:16])
ggcorrplot(corr)

df$hit <- with(df, paste(artists,",", name))
head(df)

df$key <- as.factor(df$key)
df$mode <- as.factor(df$mode)
df$time_signature <- as.factor(df$time_signature)

#renderPlotly({
ggplot(df, aes(x=key, fill=key)) +
  geom_bar() +
  theme(legend.position='none') +
  labs(title = "Sound Keys Counts (Pitch Class notation)") +
  scale_x_discrete(labels = c('C','C#','D','D#','E','F','F#','G','G#','A','A#','B'))
#})

c <- sum(df$key==0)
c2 <- sum(df$key==1)
d <- sum(df$key==2)
d2 <- sum(df$key==3)
e <- sum(df$key==4)
f <- sum(df$key==5)
f2 <- sum(df$key==6)
g <- sum(df$key==7)
g2 <- sum(df$key==8)
a <- sum(df$key==9)
a2 <- sum(df$key==10)
b <- sum(df$key==11)

scale = c(c,c2,d,d2,e,f,f2,g,g2,a,a2,b)

plot_ly(
  x = c('C','C#','D','D#','E','F','F#','G','G#','A','A#','B'),
  y = scale,
  type = "bar"
)
  
ggplot(df, aes(x=mode, fill=mode)) +
  geom_bar() +
  theme(legend.position='none') +
  labs(title = "Trak Modality") +
  scale_x_discrete(labels = c('Minor','Major'))

m1 <- sum(df$mode==0)
m2 <- sum(df$mode==1)

plot_ly(
  x = c('Minor','Major'),
  y = c(m1,m2),
  type = "bar"
)

ggplot(df, aes(x=time_signature, fill=time_signature)) +
  geom_bar() +
  theme(legend.position='none') +
  labs(title = "Time Signature (how many beats are in each bar)")

t1 <- sum(df$time_signature==3)
t2 <- sum(df$time_signature==4)
t3 <- sum(df$time_signature==5)

plot_ly(
  x = c('3','4','5'),
  y = c(t1,t2,t3),
  type = "bar"
)

df %>%
  group_by(artists) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  slice(1) %>%
  d <- artists
  df$artists[3]

df %>%
  group_by(artists) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  slice(1:10) %>%
  ggplot(., aes(reorder(artists, +freq), freq))+
  geom_bar(stat = "identity", fill = "royalblue1", col = "grey10")+
  coord_flip()+
  labs(x = "" ,y = "Top 10 2018 Artists", title = "Artists with most hits in Top 100")+
  geom_text(aes(label = freq, y = freq/2))

df %>%
  arrange(desc(danceability)) %>%
  slice(1:5) %>%
  ggplot(., aes(reorder(hit, +danceability), danceability))+
  geom_bar(stat = "identity", fill = "royalblue1", col = "grey10")+
  coord_flip()+
  labs(x = "" ,y = "Score", title = "Danceability")+
  geom_text(aes(label = danceability, y = danceability/2))

df %>%
  arrange(desc(energy)) %>%
  slice(1:5) %>%
  ggplot(., aes(reorder(hit, + energy), energy))+
  geom_bar(stat = "identity", fill = "royalblue1", col = "grey10")+
  coord_flip()+
  labs(x = "" ,y = "Score", title = "Energy")+
  geom_text(aes(label = energy, y = energy/2))

df %>%
  arrange(desc(loudness)) %>%
  slice(1:5) %>%
  ggplot(., aes(reorder(hit, + loudness), loudness))+
  geom_bar(stat = "identity", fill = "royalblue1", col = "grey10")+
  coord_flip()+
  labs(x = "" ,y = "Score", title = "Loudness")+
  geom_text(aes(label = loudness, y = loudness/2))

df %>%
  arrange(desc(duration_ms)) %>%
  slice(1:5) %>%
  ggplot(., aes(reorder(hit, + duration_ms), duration_ms))+
  geom_bar(stat = "identity", fill = "royalblue1", col = "grey10")+
  coord_flip()+
  labs(x = "" ,y = "Score", title = "Duration")+
  geom_text(aes(label = format(as.POSIXct(Sys.Date())+duration_ms/1000, "%M:%S"), y = duration_ms/2))


# Length: very slow (20 bpm)
# Adagio: slow and majestic (66 to 76 bpm)
# Andante: at the pace, quiet, a little vivacious (76 to 108 bpm)
# Allegro: animated and fast. (110 to 168 bpm).
# Presto: very fast (168 to 200 bpm).

q <- sum(df$tempo > 168)
w <- sum(df$tempo >= 110 & df$tempo <= 168)
e <- sum(df$tempo >= 76 & df$tempo <= 108)
r <- sum(df$tempo >= 66 & df$tempo <= 76)
t <- sum(df$tempo < 65)

presto <- integer(7) 
allegro <- integer(47) + 1
andante <- integer(44) + 2
adagio <- integer(1) + 3
lenght <- integer(1) + 4 
 
rhythm = c(presto, allegro, andante, adagio, lenght)
rhythm <- as.factor(rhythm)
genres <- data.frame(rhythm)

ggplot(genres,aes(x=rhythm, fill= rhythm)) +
  geom_bar() +
  theme(legend.position='none') +
  labs(title = "Musical Tempo Bands") +
  scale_x_discrete(labels = c('Presto', 'Allegro', 'Andante','Adagio', 'Lenght'))

plot_ly(
  x = c('Presto', 'Allegro', 'Andante','Adagio', 'Lenght'),
  y = c(q,w,e,r,t),
  type = "bar"
)
