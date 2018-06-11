# Load the Libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(RColorBrewer)
library(treemap)
library(tidyr)
library(grid)
library(gridExtra)
library(corrplot)
library(cluster)
library(NbClust)
library(factoextra)
library(FactoMineR)
library(ellipse)
library(MASS)


# Load dataset
pkmon <- read.csv('pokemon.csv')

# Understand dataset
str(pkmon)

pkmon$Legendary = as.logical(pkmon$Legendary)
pkmon$Generation = as.factor(pkmon$Generation)

levels(pkmon$Generation) <- c('Gen 1','Gen 2','Gen 3','Gen 4','Gen 5','Gen 6')

# Types

# Let's understand how the Pokemon world is divided based on Type
p1 <- pkmon %>% group_by(Type.1) %>% summarise(nr = length(Type.1)) %>% ungroup() %>%
  ggplot(aes(reorder(Type.1,-nr), y = nr, fill = factor(Type.1))) +
  geom_bar(stat = 'identity') +
  labs(x = 'Type 1', y = 'Count', title = 'Pokemon Count Based on Type 1') +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

p1

# Water and Normal Type Pokemon are the most common, while flyng is the less common

p2 <- pkmon %>% group_by(Type.2) %>% summarise(nr = length(Type.2)) %>% ungroup() %>%
  ggplot(aes(reorder(Type.2,-nr), y = nr, fill = factor(Type.2))) +
  geom_bar(stat = 'identity') +
  labs(x = 'Type 2', y = 'Count', title = 'Pokemon Count Based on Type 2')
  theme_bw() + theme(plot.title = element_text(hjust=0.5))

p2

# Awesome, so in the Pokemon World we don't that many with more than 1 type
# What really caught my interest is that there aren't many Pokemon with flying as their Type 1, but 
# most Pokemon have Flying as Type 2

# Plot Types Relation
types <- pkmon %>% group_by(Type.1, Type.2) %>% summarise(count = n())

ggplot(types, aes(x= Type.1, y= Type.2)) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count)) +
  labs(x = 'Type 1', y = 'Type 2') +
  scale_fill_gradient(low = 'white', high = 'red') +
  theme_bw()

# So we now know that pure Normal and Water are the most common, which is not surprising since they're the most frequent
# Ah hah, so There are many type 2 flying, being common in Normal and Bug pokemon

# Finally, what's the distribution of types in each gen?
pkmon %>% ggplot(aes(x = Type.1, fill = Generation)) +
  geom_bar() +
  labs(title = 'Pokemons of Each Gen per Type 1', x = 'Type 1', y = 'Pokemons') +
  theme_few() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

pkmon %>% ggplot(aes(x = Type.2, fill = Generation)) +
  geom_bar() +
  labs(title = 'Pokemons of Each Gen per Type 2', x = 'Type 2', y = 'Pokemons') +
  theme_few() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

# Stats

# Let's add a Total column to get to know the total stat of every Pokemon
pkmon$total <- rowSums(pkmon[,5:10])

# Create a variables that only takes the stats 
poke <- pkmon[,5:10]

corrplot(cor(poke),method = "circle", type = "full")

# From what we got from this we can say that all stats are positively correlated.

# We want to test out how do stats behave in relations to other variables
pkmon_stats <- pkmon %>% 
  gather(attribute, value, -X., -Name, -Type.1, -Type.2, -Legendary, -Generation, -total) %>% 
  mutate(generation = as.factor(Generation)) %>% 
  mutate(dual_type = Type.2 != "")

ggplot(pkmon_stats, aes(fill = factor(attribute))) +
  geom_boxplot(aes(x = attribute, y = value)) +
  labs(title = "Pokemon Stats", x = "Attribute", y = "Values") + 
  theme_bw() + theme(plot.title = element_text(hjust=0.5))

# So the highest stat is Attack, but HP has the highest number of outliers, which are the famous Walls: Pokemon who are resistant

ggplot(pkmon_stats) +
  geom_boxplot(aes(x = attribute, y = value, fill = Legendary)) +
  labs(title = "Legendary vs Non-Legendary Stats", x = "Attribute", y = "Values") +
  theme_bw() + theme(plot.title = element_text(hjust=0.5)) 

# Suprisingly, Legendary Pokemon are enhanced in Special Attack stat

# Now it's time to plot the density of types
p1 <- ggplot(pkmon, aes(x = total, fill = Type.1)) + geom_density(alpha = 0.5) +
  facet_wrap(~Type.1) +
  labs(title = 'Pokemon Stats Distribution by Type 1', x = 'Type', y = 'Density') +
  theme_bw() + theme(plot.title = element_text(hjust=0.5))

p1

# Now let's see this relationship by pokemon type in a specific stat
p1 <- pkmon %>% group_by(Type.1) %>% mutate(median_ = median(HP)) %>%
  ggplot(aes(x = reorder(Type.1, HP, FUN =  median), y = HP)) +
  geom_boxplot(aes(fill = median_)) +
  labs(title = "Pokemon Stats on HP", x = "Type 1", y = "HP") +
  scale_fill_gradient(low = 'white', high = 'red') +
  theme_bw(base_size = 12) + theme(plot.title = element_text(hjust=0.5))

p2 <- pkmon %>% group_by(Type.1) %>% mutate(median_ = median(Speed)) %>%
  ggplot(aes(x = reorder(Type.1, Speed, FUN =  median), y = Speed)) +
  geom_boxplot(aes(fill = median_)) +
  labs(title = "Pokemon Stats on Speed", x = "Type 1", y = "Speed") +
  scale_fill_gradient(low = 'white', high = 'red') +
  theme_bw(base_size = 12) + theme(plot.title = element_text(hjust=0.5))

p3 <- pkmon %>% group_by(Type.1) %>% mutate(median_ = median(Attack)) %>%
  ggplot(aes(x = reorder(Type.1, Attack, FUN =  median), y = Attack)) +
  geom_boxplot(aes(fill = median_)) +
  labs(title = "Pokemon Stats on Attack", x = "Type 1", y = "Attack") +
  scale_fill_gradient(low = 'white', high = 'red') +
  theme_bw(base_size = 12) + theme(plot.title = element_text(hjust=0.5))

p4 <- pkmon %>% group_by(Type.1) %>% mutate(median_ = median(Defense)) %>%
  ggplot(aes(x = reorder(Type.1, Defense, FUN =  median), y = Defense)) +
  geom_boxplot(aes(fill = median_)) +
  labs(title = "Pokemon Stats on Defense", x = "Type 1", y = "Defense") +
  scale_fill_gradient(low = 'white', high = 'red') +
  theme_bw(base_size = 12) + theme(plot.title = element_text(hjust=0.5))

p5 <- pkmon %>% group_by(Type.1) %>% mutate(median_ = median(Sp..Atk)) %>%
  ggplot(aes(x = reorder(Type.1, Sp..Atk, FUN =  median), y = Sp..Atk)) +
  geom_boxplot(aes(fill = median_)) +
  labs(title = "Pokemon Stats on Sp. Attack", x = "Type 1", y = "Special Attack") +
  scale_fill_gradient(low = 'white', high = 'red') +
  theme_bw(base_size = 12) + theme(plot.title = element_text(hjust=0.5))

p6 <- pkmon %>% group_by(Type.1) %>% mutate(median_ = median(Sp..Def)) %>%
  ggplot(aes(x = reorder(Type.1, Sp..Def, FUN =  median), y = Sp..Def)) +
  geom_boxplot(aes(fill = median_)) +
  labs(title = "Pokemon Stats on Sp. Defense", x = "Type 1", y = "Special Defense") +
  scale_fill_gradient(low = 'white', high = 'red') +
  theme_bw(base_size = 12) + theme(plot.title = element_text(hjust=0.5))

grid.arrange(p1,p2, ncol = 1)
grid.arrange(p3,p4, ncol = 1)
grid.arrange(p5,p6, ncol = 1)

# Then who are the toughest Pokemon out there? 
# We know that legendary Pokemon are gonna top the list, so we're going to divide into 2 plots.
pkmon %>% filter(pkmon$Legendary == TRUE) %>% arrange(desc(total)) %>% slice(1:10) %>%
  ggplot(aes(reorder(Name,total), y = total)) +
  geom_bar(stat = 'identity', fill = '#FF5a5a') +
  coord_flip() + theme_few(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Top 10 Legendary Pokemon', x = 'Pokemon', y = 'Total Score')

# Now let's do the Non-Legendary

pkmon %>% filter(pkmon$Legendary == FALSE) %>% arrange(desc(total)) %>% slice(1:10) %>%
  ggplot(aes(reorder(Name,total), y = total)) +
  geom_bar(stat = 'identity', fill = '#87CEEB') +
  coord_flip() + theme_few(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Top 10 Non-Legendary Pokemon', x = 'Pokemon', y = 'Total Score')

# Mega Evolutions dominate both plots. Let's check the top Pokemon per stat.

pkmon %>% arrange(desc(HP)) %>% slice(1:10) %>%
  ggplot(aes(reorder(Name,HP), y = HP)) +
  geom_bar(stat = 'identity', fill = '#32CD32') +
  coord_flip() + theme_few(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Top 10 Pokemon based on HP', x = 'Pokemon', y = 'Total HP')

pkmon %>% arrange(desc(Speed)) %>% slice(1:10) %>%
  ggplot(aes(reorder(Name,Speed), y = Speed)) +
  geom_bar(stat = 'identity', fill = 'gold') +
  coord_flip() + theme_few(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Top 10 Pokemon based on Speed ', x = 'Pokemon', y = 'Total Speed')

pkmon %>% arrange(desc(Attack)) %>% slice(1:10) %>%
  ggplot(aes(reorder(Name,Attack), y = Attack)) +
  geom_bar(stat = 'identity', fill = '#FF4C4C') +
  coord_flip() + theme_few(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Top 10 Pokemon based on Attack', x = 'Pokemon', y = 'Total Attack')

pkmon %>% arrange(desc(Defense)) %>% slice(1:10) %>%
  ggplot(aes(reorder(Name,Defense), y = Defense)) +
  geom_bar(stat = 'identity', fill = '#87CEEB') +
  coord_flip() + theme_few(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Top 10 Pokemon based on Defense', x = 'Pokemon', y = 'Total Defense')

pkmon %>% arrange(desc(Sp..Atk)) %>% slice(1:10) %>%
  ggplot(aes(reorder(Name,Sp..Atk), y = Sp..Atk)) +
  geom_bar(stat = 'identity', fill = '#FFA500') +
  coord_flip() + theme_few(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Top 10 Pokemon based on Sp. Attack', x = 'Pokemon', y = 'Total Sp. Attack')

pkmon %>% arrange(desc(Sp..Def)) %>% slice(1:10) %>%
  ggplot(aes(reorder(Name,Sp..Def), y = Sp..Def)) +
  geom_bar(stat = 'identity', fill = '#4C4CFF') +
  coord_flip() + theme_few(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Top 10 Pokemon based on Sp. Defense', x = 'Pokemon', y = 'Total Sp. Defense')

# Generations
# So, which gen has the strongest pokemon in average? 

ggplot(pkmon %>% group_by(Generation), aes(fill = factor(Generation))) +
  geom_boxplot(aes(x = Generation, y = total)) +
  labs(title = "Pokemon Stats", x = "Generation", y = "Total", fill = "Legend") + 
  theme_bw() + theme(plot.title = element_text(hjust=0.5))

# So it appears that Gen 4 has the best pokemon. Is it because it has more legendary?

# First, let's check the distribution

p1 <- ggplot(pkmon %>% group_by(Generation), aes(fill = factor(Generation))) +
  geom_boxplot(aes(x = Generation, y = HP)) +
  labs(title = "HP by Gen", x = "Generation", y = "Total", fill = "Legend") + 
  theme_few() + theme(plot.title = element_text(hjust=0.5))

p2 <- ggplot(pkmon %>% group_by(Generation), aes(fill = factor(Generation))) +
  geom_boxplot(aes(x = Generation, y = Speed)) +
  labs(title = "Speed by Gen", x = "Generation", y = "Total", fill = "Legend") + 
  theme_few() + theme(plot.title = element_text(hjust=0.5))

p3 <- ggplot(pkmon %>% group_by(Generation), aes(fill = factor(Generation))) +
  geom_boxplot(aes(x = Generation, y = Attack)) +
  labs(title = "Attack by Gen", x = "Generation", y = "Total", fill = "Legend") + 
  theme_few() + theme(plot.title = element_text(hjust=0.5))

p4 <- ggplot(pkmon %>% group_by(Generation), aes(fill = factor(Generation))) +
  geom_boxplot(aes(x = Generation, y = Defense)) +
  labs(title = "Defense by Gen", x = "Generation", y = "Total", fill = "Legend") + 
  theme_few() + theme(plot.title = element_text(hjust=0.5))

p5 <- ggplot(pkmon %>% group_by(Generation), aes(fill = factor(Generation))) +
  geom_boxplot(aes(x = Generation, y = pkmon$Sp..Atk)) +
  labs(title = "Sp. Attack by Gen", x = "Generation", y = "Total", fill = "Legend") + 
  theme_few() + theme(plot.title = element_text(hjust=0.5))

p6 <- ggplot(pkmon %>% group_by(Generation), aes(fill = factor(Generation))) +
  geom_boxplot(aes(x = Generation, y = pkmon$Sp..Def)) +
  labs(title = "Sp. Defense by Gen", x = "Generation", y = "Total", fill = "Legend") +
  theme_few() + theme(plot.title = element_text(hjust=0.5)) 

grid.arrange(p1,p2,p3,p4,p5,p6, ncol = 2)

# Hmm, so there is a fare distribution between gens, but we can see that Gen 4 has better stat pokemon. 
# Let's count how many legendaries are per gen.

pkmon %>% group_by(Generation) %>% filter(Legendary == TRUE) %>% summarise(nr = length(Legendary)) %>% ungroup() %>%
  ggplot(aes(x = reorder(Generation,nr), y = nr, fill = factor(Generation))) +
  geom_bar(stat = 'identity') + coord_flip() +
  labs(title = 'Number of Legendary Pokemon per Gen', x = 'Generation', y = 'Count', fill = 'Legend') +
  theme_few() + theme(plot.title = element_text(hjust = 0.5))

# Hmm, so Gen 3 has the most Legendary Pokemon, and Gen 4 is in 3rd place. Let's see which Gen has the most pokemon.

pkmon %>% group_by(Generation) %>% summarise(nr = length(Generation)) %>% ungroup() %>%
  treemap(
    index = "Generation",
    type = "value",
    vSize = "nr",
    vColor = "nr",
    palette = "RdYlBu",
    title = sprintf("Pokemon per Generation"),
    title.legend = "Number of Legendary Pokemon",
    fontsize.title = 14
  )

# Gen 4 seems to have less pokemon, a decent number of legendary, and overall great stats. Let's do a standard deviation to double-check.

sd(pkmon$total)
sd(pkmon$total[pkmon$Generation == 'Gen 1'])
sd(pkmon$total[pkmon$Generation == 'Gen 2'])
sd(pkmon$total[pkmon$Generation == 'Gen 3'])
sd(pkmon$total[pkmon$Generation == 'Gen 4'])
sd(pkmon$total[pkmon$Generation == 'Gen 5'])
sd(pkmon$total[pkmon$Generation == 'Gen 6'])

# So Gen 4 Pokemon Stats are o most the same as the entire Pokemon standard deviation. We could just think that Gen 4 Pokemon
# have better stats in general, although that doesn't define eveything in Pokemon since the move pool has great influence. 

# Making the Model
# Let's hit it with a good old PCA

# poke.pca <- PCA(poke)

poke.stat.type <- aggregate(pkmon[,5:9],list(pkmon$Type.1),mean)
poke.stat.type <- data.frame(poke.stat.type[,-1], row.names=poke.stat.type[,1])
# poke.stat.type.pca <- PCA(poke.stat.type)


#Discriminant Analysis
# poke.lda <- lda(as.matrix(pkmon[c("HP","Attack","Defense","Sp..Atk","Sp..Def","Speed")]), grouping = pkmon$Type.1)
# poke.lda

poke.type <- pkmon[c("HP","Attack","Defense","Sp..Atk","Sp..Def","Speed","Type.1")]
poke.type$Type.1 <- "Other"
poke.type$Type.1[(pkmon$Type.1 == "Rock") | (pkmon$Type.1 == "Steel")] <- "Tough"
poke.type$Type.1[(pkmon$Type.1 == "Poison") | (pkmon$Type.1 == "Normal")  | (pkmon$Type.1 == "Bug")  | (pkmon$Type.1 == "Fighting")  | (pkmon$Type.1 == "Ground")] <- "City-Forest"
poke.type$Type.1[(pkmon$Type.1 == "Dragon")] <- "Dragon"

set.seed(0)
poke.kmeans <- kmeans(poke,centers = 3)
poke.kmeans$centers
autoplot(poke.kmeans, data=pkmon)

# Yes! Now that we know in which clsuter does every pokemon belong, let's add that to out table
pokemon <- pkmon
pokecluster <- as.data.frame(poke.kmeans$cluster)
pokemon$PokeCluster <- pokecluster
colnames(pokemon$PokeCluster) <- "PokeCluster"

# Now on to the Type Cluster
set.seed(9)
poke.kmeans <- kmeans(poke.stat.type,centers =  4)
poke.kmeans$centers
autoplot(poke.kmeans, data=poke.stat.type,label = TRUE, label.size = 3)

pokemon$TypeCluster <- "1"
pokemon$TypeCluster[pokemon$Type.1 == 'Bug' | 
                      pokemon$Type.1 == 'Fighting' | 
                      pokemon$Type.1 == 'Ground' | 
                      pokemon$Type.1 == 'Normal' | 
                      pokemon$Type.1 == 'Poison'] <- "2"
pokemon$TypeCluster[pokemon$Type.1 == 'Dark' |
                      pokemon$Type.1 == 'Electric' |
                      pokemon$Type.1 == 'Fairy' |
                      pokemon$Type.1 == 'Ghost' |
                      pokemon$Type.1 == 'Grass' |
                      pokemon$Type.1 == 'Ice' |
                      pokemon$Type.1 == 'Water'] <- "3"
pokemon$TypeCluster[pokemon$Type.1 == 'Rock' |
                      pokemon$Type.1 == 'Steel'] <- "4"
