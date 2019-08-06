library(tidyverse)

bobross <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

bobross <- bobross %>% 
  rename(ID = EPISODE) %>%
  mutate(SEASON = str_extract(string = ID, pattern = "^S.."),
         EPISODE = str_extract(string = ID, pattern = "E..$"))

bob.rows <- seq(1, nrow(bobross))
frames <- colnames(bobross)[contains(match = "_FRAME", vars = colnames(bobross))]
Elements <- c("FIRE", "ROCKS", "SNOW")
Landscapes <- c("BEACH", "CLIFF", "FOG", "HILLS", "LAKE", "LAKES", "MOUNTAIN",
                "MOUNTAINS", "OCEAN", "PATH", "RIVER", "SNOWY_MOUNTAIN",
                "WATERFALL", "WAVES")
Sky <- c("AURORA_BOREALIS", "CIRRUS", "CLOUDS", "CUMULUS", "MOON", "SUN")
Plants <- c("BUSHES", "CACTUS", "CONIFER", "DECIDUOUS", "FLOWERS", "GRASS",
            "PALM_TREES", "TREE", "TREES")
Constructions <- c("BARN", "BRIDGE", "BUILDING", "CABIN", "FARM", "FENCE",
                   "LIGHTHOUSE", "MILL", "STRUCTURE", "WINDMILL")
Subjects <- c("DIANE_ANDRE", "GUEST", "PERSON", "PORTRAIT", "STEVE_ROSS")

new_col <- function(vect){
  map(.x = bob.rows, .f = function(x){sum(bobross[x, vect])})
}

bobross <- bobross %>%
  mutate(FRAMES = map(.x = seq(1, nrow(bobross)), .f = function(x){
    which(bobross[x, frames] == 1)}) %>% as.numeric(),
    FRAMES = ifelse(is.na(FRAMES), 0, frames[FRAMES]),
    FRAMES = str_remove_all(string = FRAMES, pattern = "_FRAME$"),
    FRAME = ifelse(FRAMES == 0, "NO", "YES"),
    Elements = new_col(Elements),
    Landscapes = new_col(Landscapes),
    Sky = new_col(Sky),
    Plants = new_col(Plants),
    Constructions = new_col(Constructions),
    Subjects = new_col(Subjects))

frame_type <- bobross %>% count(FRAMES) %>% filter(FRAMES != 0) %>%
  ggplot(aes(x = reorder(FRAMES, -n), y = n)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() + theme(axis.text.x = element_text(hjust = 1, angle = 90)) +
  labs(x = "", y = "Count", title = "Most used frames")

frame_yn <- bobross %>%
  ggplot(aes(x = EPISODE, y = SEASON, fill = FRAMES)) +
  geom_tile() + labs(title = "Utilization of Frames over time") +
  theme(legend.text = element_text(size = 3))

theme_frameyn <- bobross %>%
  gather(key = Themes, value = N, c(Elements, Landscapes, Sky, Plants,
                                   Constructions, Subjects)) %>%
  ggplot(aes(x = Themes, y = N, fill = FRAME)) +
  geom_bar(stat = "identity") + theme_bw() +
  labs(title = "Themes Pictured")

P1 <- gridExtra::arrangeGrob(theme_frameyn, frame_yn, frame_type)

plot(P1)

ggsave(filename = "bobross.png", plot = P1, path = "2019-08-06")
