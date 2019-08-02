batting.team <- read_csv(file.choose())
batting.ind <- read_csv(file.choose())
pitching.ind <- read_csv(file.choose())
pitching.team <- read_csv(file.choose())
batting.team.2019 <- read_csv(file.choose())


#get team stats so that it contains wins and losses
pitching.ind.3 <- pitching.ind %>%
  group_by(team) %>%
  summarize(total.wins = sum(w),
         total.losses = sum(l))

#team stats with records
pitching.team4 <- left_join(pitching.team,
                            pitching.ind.3,
                            by = "team")
pitching.team4 <- pitching.team4 %>%
  mutate(win.perc = total.wins/(total.wins+total.losses))
#individual stats with team record
pitching.ind1 <- left_join(pitching.ind,
                            pitching.ind.3,
                            by = "team")


#team hitting stats with record
batting.team1 <- left_join(batting.team,
                           pitching.ind.3,
                           by = "team")
batting.team1 <- batting.team1 %>%
  mutate(win.perc = total.wins/(total.wins+total.losses))
#individual stats with team record
batting.ind1 <- left_join(batting.ind,
                           pitching.ind.3,
                           by = "team")
#regressions: what variables are related to wins?
#runs created
model1 <- lm(win.perc ~ rc,
             data = batting.team1)
summary(model1)
model1$coefficients #intercept and slope
#what's the predicted wining percentage for a team with 200 runs created
predict.lm(model1,
           data.frame(rc = 200))

batting.team1 %>%
  ggplot(aes(x = bb,
             y = win.perc)) +
  geom_point() +
  geom_abline(slope = .0021, 
              intercept = .0537,
              color = "red",
              size = 3)
batting.ind %>%
  filter(team == "Middlebury") %>%
  summarise(total.rc = sum(rc))
#looking at different graphs
batting.team1 %>%
  ggplot(aes(x = `k%`,
             y = win.perc)) +
  geom_point() 

geom_text(aes(label = team))

#all
#remove.categoricalvariables
bat.team2 <- batting.team1 %>%
  select(-team,
         -conference,
         -total.wins,
         -total.losses)
model2 <- lm(win.perc ~ .,
             data = bat.team2)
step(model2)
#for pitchers
pitch.team2 <- pitching.team4 %>%
  select(-team,
         -conference,
         -total.wins,
         -total.losses)
model2 <- lm(win.perc ~ .,
             data = pitch.team2)
step(model2)
pitch.team2 %>%
  ggplot(aes(x = go,
             y = win.perc)) +
  geom_point() +
  ylab("Winning Percentage")

#individual data
#clustering
players.subset <- batting.ind %>%
  select(-team,
         -conference,
         -class,
         -no,
         -split,
         -name) %>%
  na.omit()

hittingplayers.kmeans <- kmeans(players.subset,4)


###All Aericans
first.team <- read_csv(file.choose())

first.team.scaled <- scale(first.team %>%
                             select(-`Name, School`, 
                                    -`Yr.`,
                                    -`Pos.`))
#dustance matrix
distance.matrix <- dist(first.team.scaled)

hc1 <- hclust(distance.matrix)
plot(hc1)

d1 <- as.dendrogram(hc1)
labels <- first.team$`Name, School`[order.dendrogram(d1)] #try and get names of players
plot(d1)

#make three clusters
clusters <- cutree(hc1, k = 3)
#draw rectangles around clusters
rect.hclust(hc1,
            k = 3,
            border = 1:3)
#choosing colors
d1 <- color_branches(d1, k = 3)

d1 %>%
  set("labels_col",
      c("Red", "Red", "Blue", "Green", "Green", "Green")) %>%
  plot()

#Make 3 clusters
clusters <- cutree(d1, k = 3)

#Choose my colors
colors <- c("Red", "Blue", "Green")

colors[clusters[labels(d1)]]

d1 %>%
  set("labels_col",
      colors[clusters[labels(d1)]]) %>%
  set("labels_cex", 2) %>%
  circlize_dendrogram()
###all hitters
allhitters.scaled <- scale(batting.ind %>%
                             select(-name, 
                                    -`split`,
                                    -`class`,
                                    -no,
                                    -team,
                                    -conference,
                                    -pos))

distance.matrix <- dist(allhitters.scaled)

hc1 <- hclust(distance.matrix)
plot(hc1)

d1 <- as.dendrogram(hc1)
labels <- first.team$`Name, School`[order.dendrogram(d1)] #try and get names of players
plot(d1)

#make three clusters
clusters <- cutree(hc1, k = 3)
#draw rectangles around clusters
rect.hclust(hc1,
            k = 3,
            border = 1:3)
#choosing colors
d1 <- color_branches(d1, k = 3)

d1 %>%
  set("labels_col",
      c("Red", "Red", "Blue", "Green", "Green", "Green")) %>%
  plot()

#Make 3 clusters
clusters <- cutree(d1, k = 3)

#Choose my colors
colors <- c("Red", "Blue", "Green")

colors[clusters[labels(d1)]]

d1 %>%
  set("labels_col",
      colors[clusters[labels(d1)]]) %>%
  set("labels_cex", 2) %>%
  circlize_dendrogram()

###nescac
nescac <- batting.ind %>%
  filter(conference == "NESCAC",
         ab > 20)
#scale the data
nescac.scaled <- scale(nescac %>%
                             select(-name, 
                                    -`split`,
                                    -`class`,
                                    -no,
                                    -team,
                                    -conference,
                                    -pos))

distance.matrix <- dist(nescac.scaled)

hc1 <- hclust(distance.matrix)
plot(hc1)


View(nescac[188,])

cluster <- cutree(hc1, 
       k = 2)
#rectangels around them
rect.hclust(hc1,
            k = 4,
            border = 1:3)
#starting to see why people are grouped together
#at bats and woba
#clusters 6, 2 are good
nescac %>%
  mutate(cluster = cluster) %>%
  ggplot(aes(x = ab,
             y = woba)) +
  geom_jitter(aes(color = factor(cluster)),
              size = 3)
#at bats and obp
nescac %>%
  mutate(cluster = cluster) %>%
  filter(cluster == 4 |
           cluster == 6 |
           cluster == 7) %>%
  ggplot(aes(x = ab,
             y = babip)) +
  geom_jitter(aes(color = factor(cluster)),
              size = 3) +
  geom_text(aes(label = name))
#without names and filtered clsuters
nescac %>%
  mutate(cluster = cluster) %>%
  ggplot(aes(x = ab,
             y = babip)) +
  geom_jitter(aes(color = factor(cluster)),
              size = 3)

nescac.top.hitters <- nescac %>%
  mutate(cluster = cluster) %>%
  filter(cluster == 4 |
           cluster == 6 |
           cluster == 7)

all.nescac2018 <- read_csv(file.choose())
all.nescac <- all.nescac2018 %>%
  filter(Position != "P") 
#joined together only top hitters
colnames(all.nescac)[2] <- "name"
all.nescac.joined <- left_join(nescac.top.hitters,
                               all.nescac,
                               by = "name") 
#nescac <- nescac %>%
  mutate(cluster = cluster)

all.nescac.joined %>%
  mutate(win.all.nescac = !is.na(Institution)) %>%
  select(name,
         cluster,
         win.all.nescac)

#looking at all hitters now
all.nescac.joined2 <- left_join(nescac,
                                all.nescac,
                                by = "name")
head(all.nescac.joined2 %>%
  mutate(win.all.nescac = !is.na(Institution)) %>%
  select(name,
         pos,
         cluster,
         win.all.nescac) %>%
  arrange(-win.all.nescac), 17) 

head(x, 17)


nescac.top.hitters$name[5] == all.nescac$Name[6]
###2019 data
hitters.2019 <- read_csv(file.choose())
###nescac
nescac.2019 <- hitters.2019 %>%
  filter(conference == "NESCAC")
nescac2019.scaled <- scale(nescac.2019 %>%
                         select(-name, 
                                -`split`,
                                -`class`,
                                -no,
                                -team,
                                -conference,
                                -pos))

distance.matrix <- dist(nescac2019.scaled)
hc1 <- hclust(distance.matrix)
#see how many clusters I think theyre are
plot(hc1)
cluster <- cutree(hc1, 
                  k = 5)
#draw bozes around
rect.hclust(hc1,
            k = 7,
            border = 1:3)
#woba and ab's
#with 7 clusters, kind of separates it up into people with injuries who did well with limited at bats, and those who did well with lots of at bats
#clustees 3 and 7
nescac.2019 %>%
  mutate(cluster = cluster) %>%
  filter(cluster == 7) %>%
  ggplot(aes(x = ab,
             y = woba)) +
  geom_jitter(aes(color = factor(cluster)),
              size = 3) +
  geom_text(aes(label = name))  
#runs created and woba
#koperniack seems to be the best player honestly
cluster <- cutree(hc1, 
                  k = 5)
nescac.2019 %>%
  mutate(cluster = cluster) %>%
  filter(ab > 20) %>%
  ggplot(aes(x = woba,
             y = rc)) +
  geom_jitter(aes(color = factor(cluster)),
              size = 3) 

#eliminating people with under 50 AB's
#this way we get rid of the data points with good stats but those who just got injured, or those who just got very littel playing time
nescac.filtered <- nescac.2019 %>%
  filter(ab > 50) %>%
  mutate(rc.per.pa = rc/pa)
nescac.filtered.scaled <- scale(nescac.filtered %>%
                             select(-name, 
                                    -`split`,
                                    -`class`,
                                    -no,
                                    -team,
                                    -conference,
                                    -pos))

distance.matrix <- dist(nescac.filtered.scaled)
hc1 <- hclust(distance.matrix)
plot(hc1)
cluster <- cutree(hc1, 
                  k = 6)
rect.hclust(hc1,
            k = 6,
            border = 1:3)
#now here's some pretty good data;
#tells about those who stayed healthy
nescac.filtered %>%
  mutate(cluster = cluster) %>%
  filter(ab > 20) %>%
  ggplot(aes(x = woba,
             y = rc)) +
  geom_jitter(aes(color = factor(cluster)),
              size = 3) +
  geom_text(aes(label = name))

#for those still with not a lot of AB's:
#create rate stat for rc/pa
#do the same, now just with the new rate stat
nescac.filtered <- nescac.2019 %>%
  filter(ab > 50) %>%
  mutate(rc.per.pa = rc/pa)
nescac.filtered.scaled <- scale(nescac.filtered %>%
                                  select(-name, 
                                         -`split`,
                                         -`class`,
                                         -no,
                                         -team,
                                         -conference,
                                         -pos))

distance.matrix <- dist(nescac.filtered.scaled)
hc1 <- hclust(distance.matrix)
plot(hc1)
cluster <- cutree(hc1, 
                  k = 8)
rect.hclust(hc1,
            k = 8,
            border = 1:3)
#no surprise here now that this is a rate stat
#grouings are fairly distinct
#can also label by pos to see INF vs outfield
nescac.filtered %>%
  mutate(cluster = cluster) %>%
  filter(ab > 75) %>%
  ggplot(aes(x = woba,
             y = rc.per.pa)) +
  geom_jitter(aes(color = factor(cluster)),
              size = 3) +
  geom_text(aes(label = name))

#pitchers for 2019
pitchers.2019 <- read_csv(file.choose())
#for 2018 nescac
pitchers.nescac <- pitching.ind1 %>%
  filter(conference == "NESCAC", 
         ip > 15)
nescacpitchers.scaled <- scale(pitchers.nescac %>%
                         select(-name, 
                                -`split`,
                                -class,
                                -no,
                                -team,
                                -conference
                                ))

distance.matrix <- dist(nescacpitchers.scaled)

hc1 <- hclust(distance.matrix)
plot(hc1)
cluster <- cutree(hc1, 
                  k = 8)

rect.hclust(hc1,
            k = 8,
            border = 1:3)
#ip vs r
pitchers.nescac %>%
  mutate(cluster = cluster) %>%

  ggplot(aes(x = ip,
             y = r)) +
  geom_jitter(aes(color = factor(cluster)),
              size = 3) +
  geom_text(aes(label = name))

#superteam
model3 <- lm(win.perc ~ woba,
             data = batting.team1)
model3
model3$coefficients #intercept and slope
#need to know average obp of teammates
x <- all.nescac.joined2 %>%
  mutate(win.all.nescac = !is.na(Institution)) %>%
  filter(win.all.nescac == TRUE) 
#for obp: predicts winning percentage of .79
#for babip: .65
#for runs: .93
#sac flies: .84
#woba: .78
x %>%
  summarise(av.babip = mean(woba))
predict.lm(model3,
           data.frame(woba = .415))
predicted.hitting <- matrix(c(.79,.65,.78),ncol=5,byrow=TRUE)
colnames(predicted.hitting) <- c("OBP","BABIP",  "WOBA")
rownames(predicted.hitting) <- c("Predicted Winning Percentage")
predicted.hitting1 <- as.table(predicted.hitting)

#preditcted pitching

award.pitchers <- all.nescac2018 %>%
  filter(Position == "P")


#looking at all pitchers now
all.pitchers.joined <- left_join(pitching.ind1,
                                award.pitchers,
                                by = "name")
all.pitchers.joined2 <- all.pitchers.joined %>%
       mutate(win.all.nescac = !is.na(Institution)) %>%
       arrange(-win.all.nescac)
award.pitchers.stats <- all.pitchers.joined2 %>%
  filter(win.all.nescac == TRUE)

#runs: 
#babip: .77
#bf
#sh
#obp: .85
model3 <- lm(win.perc ~ obp,
             data = pitching.team4)
award.pitchers.stats %>%
  summarise(stat = mean(obp))
predict.lm(model3,
           data.frame(obp = .279))
predicted.pitching <- matrix(c(.85,.77),ncol=2,byrow=TRUE)
colnames(predicted.pitching) <- c("OBP","BABIP")
rownames(predicted.pitching) <- c("Predicted Winning Percentage")
