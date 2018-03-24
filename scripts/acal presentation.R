library(tidyverse)
library(plot3D)
library(xaringan)
library(dbplyr)
library(readr)



pitchmodiu <- read_tsv("../data/modiu_Rec.means.tsv")
pitchyussif <- read_tsv("../data/yussif_Rec.means.tsv")
dur <- read_tsv("../data/dur.tsv")

#Pitch for both participants
##Modiu pitch data

my_modiupitch <- pitchmodiu %>% 
  select(., rowLabel, minf0) %>%  
  separate(rowLabel, into = c("p", "syll", "rep"), sep = "_", remove = F) %>% 
  separate(., rowLabel, into = c("key1", "key2m"), sep = 3) %>% 
  separate(., rep, into = c("tone", "rep"), sep = 1) %>%
  select(., -key1 ) %>% 
  select(., key2m, p, minf0) %>% 
  spread(., key = p, value = minf0) %>% 
  transform(., pitchratiom = p2 / p1) %>% 
  select(., key2m, pitchratiom) %>% 
  separate(., key2m, into = c("mkey", "repe"), sep = "_") %>% 
  separate(., repe, into = c("tone", "rep"), sep = 1) %>% 
  unite(., "tonetype", c("mkey", "tone")) %>% 
  spread(., key = rep, value = pitchratiom) %>% 
  select(., -H1, -H2, -L1, -L2, first = 2, second = 3) %>% 
  transform(., meanpitchm = (first + second) / 2)
  
mpitch <- my_modiupitch[c(-1,-14,-15,-16,-30),] %>% 
    select(., tonetype, p1 = meanpitchm)
    
##Yussif pitch data

my_yussifpitch <- pitchyussif %>% 
select(., rowLabel, minf0) %>%
  separate(rowLabel, into = c("p", "syll", "rep"), sep = "_", remove = F) %>% 
  separate(., rowLabel, into = c("key1", "key2y"), sep = 3) %>% 
  separate(., rep, into = c("tone", "rep"), sep = 1) %>%
  select(., -key1 ) %>% 
  select(., key2y, p, minf0) %>% 
  spread(., key = p, value = minf0) %>% 
  transform(., pitchratioy = p2 / p1) %>% 
  select(., key2y, pitchratioy) %>% 
  separate(., key2y, into = c("mkey", "repe"), sep = "_") %>% 
  separate(., repe, into = c("tone", "rep"), sep = 1) %>% 
  unite(., "tonetype", c("mkey", "tone")) %>% 
  spread(., key = rep, value = pitchratioy) %>% 
  select(., tonetype, -H1, -H2, -L1, -L2, first = 2, second = 3) %>% 
  transform(., meanpitchy = (first + second) / 2)

ypitch <- my_yussifpitch[c(-1,-14,-15,-16,-29),]
  
ypitchy <- ypitch %>%  
select(., tonetype, p2 = meanpitchy) 

##Mean pitch/p1/p2

pitch_p1_p2 <- left_join(mpitch, ypitchy, by = "tonetype")

p <- as.data.frame(pitch_p1_p2)
p[is.na(p)] <- 0.879 

pitch <- p %>% 
  select(., words = tonetype, pitch_m = p1, pitch_y = p2) 




##plot tone participant by pitch

meanp <- pitch %>% 
  transform(., mean_t_pitch = (pitch_m + pitch_y)/2) %>% 
  select(., -pitch_m, -pitch_y) %>%
  separate(., words, into = c("syll", "tone"), remove = FALSE) %>%
  select(., -syll)

##Extra
meanpp <- meanp %>%
  select(., -tone)

##pitch plot (Important)
meanp %>% 
  select(., -words) %>% 
  ggplot(., aes(x = tone, y = mean_t_pitch)) +
  geom_boxplot()

# Vowel duration
dur_p1_p2 <- dur %>% 
  separate(., word, into = c("word", "rep")) %>% 
  separate(., rep, into = c("tone", "rep"), sep = 1) %>% 
  unite(., "words", c("word", "tone")) %>% 
  gather(., key = participant, value = duration, -words, -rep) %>% 
  spread(., key = rep, value = duration) %>% 
  select(., words, participant, first = 3, second = 4) %>% 
  transform(., meandur = (first + second)/2) %>% 
  select(., -first, -second) %>% 
  separate(., words, into = c("word", "tone"), remove = FALSE) %>% 
  select(., -word)  
  
##plot tone participant by vowel duration
meand <- dur_p1_p2 %>% 
  spread(., key = participant, value = meandur) %>% 
  transform(., mean_v_duration = (dur_m + dur_y)/2) %>% 
  select(., -dur_m, -dur_y)

##by tone {error}
meand %>%
  select(., -words) %>%
  spread(., key = tone, value = mean_v_duration)

##duration plot (Important)
meand %>% 
  ggplot(., aes(x = tone, y = mean_v_duration)) +
  geom_boxplot()

left_join(meand, meanpp, by = "words")

#All together Duration vs Pitch
dp1 <- left_join(meand, meanpp, by = "words") %>%
  separate(., words, into = c("syllable", "t")) %>%
  separate(., syllable, into = c("consonant", "vowel"), sep = 1) %>%
  select(., -t)

dp <- dp1 %>% 
  select(., tone, mean_v_duration, 
         mean_t_pitch)

##plot with means
dp1 %>%
  ggplot(., aes(x = mean_v_duration, y = mean_t_pitch, color = tone, label = vowel)) +
  geom_point() +
  geom_label()

dp1 %>%
  ggplot(., aes(x = mean_v_duration, y = mean_t_pitch, color = tone)) +
  geom_point() +
  geom_raster(aes(), interpolate = FALSE)
#STATS

  
dp1 %>%
  group_by(., tone) %>%
  summarize(., mean_dura = mean(mean_v_duration), sd_dura = sd(mean_v_duration), 
            mean_pi = mean(mean_t_pitch), sd_pi = sd(mean_t_pitch)) %>%
  knitr::kable(., format = "html", digits = 2)

dp1 %>%
  group_by(., vowel) %>%
  summarize(., mean_dura = mean(mean_v_duration), 
            mean_pi = mean(mean_t_pitch))
dp1 %>%
  group_by(., consonant) %>%
  summarize(., mean_dura = mean(mean_v_duration), sd_dura = sd(mean_v_duration), 
            mean_pi = mean(mean_t_pitch), sd_pi = sd(mean_t_pitch))
#Models
##Mod null
mod_0 <- lm(mean_v_duration ~ 1, data = dp1)
summary(mod_0)

mod_1 <- lm(mean_v_duration ~ mean_t_pitch, data = dp1)
  summary(mod_1)

mod_2 <- lm(mean_v_duration ~ mean_t_pitch + vowel, data = dp1)
  summary(mod_2)

mod_3 <- lm(mean_v_duration ~ mean_t_pitch:vowel, data = dp1)
  summary(mod_3)

mod_4 <- lm(mean_v_duration ~ mean_t_pitch:consonant, data = dp1)
  summary(mod_4)
mod_5 <- lm(mean_v_duration ~ tone, data = dp1)
  summary(mod_5)
  
anova(mod_0, mod_1, mod_2, mod_3, mod_4, mod_5)
  
tone_Mod <- dp1 %>%
  group_by(., tone) %>%
  summarize(., mean_dura = mean(mean_v_duration), sd_dura = sd(mean_v_duration), 
            mean_pi = mean(mean_t_pitch), sd_pi = sd(mean_t_pitch))

mod_tone <- lm(mean_pi ~ mean_dura , data = tone_Mod)
summary(mod_tone)


  
# separate pitch/dur/p1/p2 (useless)

  left_join(pitch, durations, by = "word") 
  
plot %>%
  select(., pitch_m, dur_m, tone = tone.x) %>% 
    ggplot(., aes(x = dur_m, y = pitch_m , color = tone)) +
    geom_point() 

plot %>%
  select(., pitch_y, dur_y, tone = tone.x) %>% 
  ggplot(., aes(x = dur_y, y = pitch_y , color = tone)) +
  geom_point() 

    
  
  
  
  

  



by_tone_y <- y %>%
  separate(., tonetype, into = c("syllable", "tonetype"), sep = "_") %>%  
  spread(., key = tonetype, value = meanpitch_s1) %>% View


mutate(., logArea = log(area),
       logPop = log(pop))
d <- 
  ?Arithmetic

msleep %>% 
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())

log


