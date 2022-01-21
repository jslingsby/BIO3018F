
library(tidyverse)
library(readxl)
library(vegan)

###Get data
species66 <- read_excel("data/pnas.1619014114.sd01.xlsx", sheet = "veg1966")
species96 <- read_excel("data/pnas.1619014114.sd01.xlsx", sheet = "veg1996")
# species10 <- read_excel("data/pnas.1619014114.sd01.xlsx", sheet = "veg2010")

species66 <- species66 %>% mutate_at(-1, round, 0)
species96 <- species96 %>% mutate_at(-1, round, 0)

# Get env data
env <- read_excel("data/pnas.1619014114.sd01.xlsx", sheet = "enviroment", na = "NA")
env$...1 <- paste("CP_", env$Plot, sep = "")


### Species - Abundance

## Plots
ggplot(data.frame(species_number = rowSums(species66[,-1]>0),
                  abundance = rowSums(species66[,-1])),
       aes(abundance, species_number)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ log(x)) +
  ylab("Species Count") +
  xlab("Number of Individuals")

ggsave("images/spp_abund_CP.png", width = 5, height = 4)

## Individuals
species66l <- species66 %>% pivot_longer(cols = -1, 
                                  names_to = "Species", 
                                  values_to = "Abundance") 

#custom function from https://per48.co/blog/r-cumulative-unique-count-dplyr/  
cumulative_n_distinct = function(x){
  require(purrr)
  require(dplyr)
  y <- x %>%
    accumulate(append) %>%
    map(.,n_distinct) %>%
    as.numeric
  return(y)
}

sadded <- species66l %>%
  filter(Abundance > 0) %>% 
  mutate(`Species Count` = cumulative_n_distinct(Species),
         `Number of Plots Sampled` = cumulative_n_distinct(`...1`),
         `Number of Individuals Sampled` = cumsum(Abundance))

sadded %>%
  ggplot(aes(`Number of Individuals Sampled`)) +
  geom_line(aes(y = `Species Count`))

ggsave("images/spp_ind_accum_CP.png", width = 5, height = 4)

sadded %>%
  ggplot(aes(`Number of Plots Sampled`)) +
  geom_line(aes(y = `Species Count`))

ggsave("images/spp_sites_accum_CP.png", width = 5, height = 4)

### Species Accumulation
sinds <- specaccum(species66[,-1], method = "rarefaction", permutations = 100)
sinds <- bind_cols(sinds[c(3,4,5,7)]) 

sinds%>%
  ggplot(aes(individuals)) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd), fill = "grey70") +
  geom_line(aes(y = richness)) +
  ylab("Species Richness") +
  xlab("Number of Individuals")

ggsave("images/spec_ind_raref_CP.png", width = 5, height = 4)

sinds%>%
  ggplot(aes(sites)) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd), fill = "grey70") +
  geom_line(aes(y = richness)) +
  ylab("Species Richness") +
  xlab("Number of Plots")

ggsave("images/spec_sites_raref_CP.png", width = 5, height = 4)

### Comparison between 1966 and 1996 surveys

## Curves
sinds96 <- specaccum(species96[,-1], method = "rarefaction", permutations = 100)
sinds96 <- bind_cols(sinds96[c(3,4,5,7)]) 

sindall <- rbind(sinds, sinds96)
sindall$Year <- c(rep(1966, 81), rep(1996, 81))

sindall %>%
  ggplot(aes(individuals, group = Year)) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd), fill = "grey70") +
  geom_line(aes(y = richness)) +
  ylab("Species Richness") +
  xlab("Number of Individuals") +
  geom_text(aes(x = 45000, y = 440, label = "1966")) +
  geom_text(aes(x = 60000, y = 420, label = "1996"))

ggsave("images/spec_ind_raref_CP_comp.png", width = 5, height = 4)

sindall %>%
  ggplot(aes(sites, group = Year)) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd), fill = "grey70") +
  geom_line(aes(y = richness)) +
  ylab("Species Richness") +
  xlab("Number of Plots") +
  geom_text(aes(x = 75, y = 435, label = "1966")) +
  geom_text(aes(x = 75, y = 415, label = "1996"))

ggsave("images/spec_sites_raref_CP_comp.png", width = 5, height = 4)

##Plot-level counts
ggplot(data.frame(species_number66 = rowSums(species66[,-1]>0), 
                  species_number96 = rowSums(species96[,-1]>0)), 
       aes(species_number66, species_number96)) +
  geom_point() + 
  geom_smooth(method = "lm", formula=y~x-1) +
  ylab("Species Count 1996") +
  xlab("Species Count 1966") +
  ylim(c(0,80)) + xlim(c(0,80)) +
  ggpubr::stat_cor(label.y = 80) +
  ggpubr::stat_regline_equation(label.y = 75, formula=y~x-1)

ggsave("images/spec_comp_CP.png", width = 4, height = 4)

##Plot-level rarefaction
ggplot(data.frame(species_number66 = rarefy(species66[,-1], 10), 
                  species_number96 = rarefy(species96[,-1], 10)), 
       aes(species_number66, species_number96)) +
  geom_point() + 
  geom_smooth(method = "lm", formula=y~x-1) +
  ylab("Species Richness 1996") +
  xlab("Species Richness 1966") +
  ylim(c(2,9)) + xlim(c(2,9)) +
  ggpubr::stat_cor(label.y = 9) +
  ggpubr::stat_regline_equation(label.y = 8.5, formula=y~x-1)

ggsave("images/sper_comp_CP.png", width = 4, height = 4)

##T-tests
#Species count
delta <- tibble(delta = rowSums(species66[,-1]>0) - rowSums(species96[,-1]>0))

delta %>% ggplot(aes(delta)) + 
  geom_histogram() +
  xlab("Change in species count")

t.test(rowSums(species66[,-1]>0), rowSums(species96[,-1]>0), paired = T)

ggsave("images/spec_comp_hist_CP.png", width = 4, height = 4)

#Species richness
delta <- tibble(delta = rarefy(species66[,-1], 10) - rarefy(species96[,-1], 10))

delta %>% ggplot(aes(delta)) + 
  geom_histogram() +
  xlab("Change in species richness")

t.test(rarefy(species66[,-1], 10), rarefy(species96[,-1], 10), paired = T)

ggsave("images/sper_comp_hist_CP.png", width = 4, height = 4)

##Beta diversity

# Variation

# par(mfrow = c(1,2))
# hist(designdist(species66[,-1]), main = "", xlab = "Distance")
# hist(designdist(species96[,-1]), main = "", xlab = "Distance")

var6696 <- tibble(Year = c(rep(1966, 3240), rep(1996, 3240)), `Bray-Curtis Distance` = c(as.vector(designdist(species66[,-1])), as.vector(designdist(species96[,-1]))))

var6696 %>% ggplot(aes(`Bray-Curtis Distance`)) + 
  geom_histogram(col="grey") +
  facet_wrap(~Year)

ggsave("images/beta_variation_CP.png", width = 6, height = 3)


#NMDS

env <- left_join(species66[,1], env)

x <- metaMDS(species66[,-1], distance = "bray")

scores(x) %>%
  cbind(env) %>%
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Age1966)) +
  stat_ellipse(geom = "polygon", aes(group = Age1966, color = Age1966, fill = Age1966), alpha = 0.3) +
  annotate("text", x = -2, y = 0.95, label = paste0("stress: ", format(x$stress, digits = 4)), hjust = 0) +
  theme_bw()

ggsave("images/NMDS.png", width = 5, height = 4)

ggdendro::ggdendrogram(hclust(1-designdist(species66[,-1])))

ggsave("images/hclust.png", width = 7, height = 6)
