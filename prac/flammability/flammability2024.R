library(tidyverse)
library(googlesheets4)
library(googledrive)
library(readxl)
library(GGally)

## Download data from Googlesheets
#if (Sys.getenv("USER") == "jasper") {gmail = "jasper.slingsby@uct.ac.za"}

# Authenticate and access the Google Sheet
#drive_auth(email = gmail)
#gs4_auth(token = drive_token())

drive_deauth()
gs4_deauth()

# Download
sheet <- "https://docs.google.com/spreadsheets/d/1VYMvHk7b_GcVFxCLYt4GUyUIMfrgWasQT7uy6aE0_ms/edit#gid=1570965423"
drive_download(sheet, path = "prac/flammability/flammabilitytraits2024.xlsx", overwrite = TRUE)

## Read in data
traits <- read_xlsx("prac/flammability/flammabilitytraits2024.xlsx","TraitData")
fire <- read_xlsx("prac/flammability/flammabilitytraits2024.xlsx","Flammability")

## Summarise traits by species (by either taking the means or the majority class)

# Leaf
traits <- traits %>%
  group_by(Taxon) %>%
  summarise(
    SppCode = first(names(sort(table(SppCode), decreasing = TRUE))),
    Site = first(names(sort(table(SiteName), decreasing = TRUE))),
    `Cover at site (%)` = mean(`Cover at site (%)`, na.rm = T),
    `Growth form` = factor(first(names(sort(table(`Growth form`), decreasing = TRUE))), levels = c("Graminoid", "Shrub", "Tree")),
    `Height (cm)` = mean(`Height (cm)`, na.rm = T),
    `Leaf length (mm)` = mean(`Mean leaf length (mm)`, na.rm = T),
    `Leaf width (mm)` = mean(`Mean leaf width (mm)`, na.rm = T),
    LMA = mean(`Dry weight (g)`, na.rm = T)/mean(`Leaf area (cm2)`),
    LDMC = mean(`Dry weight (g)`, na.rm = T)/mean(`Wet weight (g)`),
    `Dead material (%)` = mean(`Dead material (%)`, na.rm = T),
    Caginess = factor(toupper(first(names(sort(table(Cageyness), decreasing = TRUE)))), levels = c("L", "M", "H")),
    Herbivory = factor(toupper(first(names(sort(table(`Herbivore damage`), decreasing = TRUE)))), levels = c("L", "M")),
    `Leaf strength` = factor(first(names(sort(table(`Leaf strength`), decreasing = TRUE))), levels = c("L", "M", "H")),
    `Phenolics` = factor(first(names(sort(table(`Phenolics`), decreasing = TRUE))), levels = c("A", "L", "M", "H"))
    )

# Flammability
fire <- fire %>%
  group_by(SppCode) %>%
  summarise(
    `Max T (C)` = mean(`Max T (C)`, na.rm = T),
    `Burning time BT (s)` = mean(`Burning time BT (s)`, na.rm = T),
    `Length burnt BL (cm)` = mean(`Length burnt BL (cm)`, na.rm = T),
    `Biomass Burnt BB (%)` = mean(`Biomass Burnt BB (%)`, na.rm = T)
  )

# Join traits into one
data <- left_join(traits, fire)


## Create categories of variables for comparisons
cont_trts <- c("Height (cm)", "Leaf length (mm)", "Leaf width (mm)", "LMA", "LDMC", "Dead material (%)")
disc_trts <- c("Growth form", "Caginess", "Herbivory", "Leaf strength", "Phenolics")
flammability <- c("Max T (C)", "Burning time BT (s)", "Length burnt BL (cm)", "Biomass Burnt BB (%)")


## Plant traits 
# Within continuous traits
data %>%
  ggpairs(cont_trts)

# Continuous vs discrete traits
data %>%
  ggduo(disc_trts, cont_trts)

# Continuous traits by site
data %>%
  ggduo("Site", cont_trts)

# Continuous traits by site, weighted by % cover of species
data %>%
  ggduo("Site", cont_trts,
        mapping = ggplot2::aes(weight = `Cover at site (%)`))

# Discrete traits by site
data %>%
  ggduo("Site", disc_trts)

# Discrete traits by site, weighted by % cover of species
data %>%
  ggduo("Site", disc_trts,
        mapping = ggplot2::aes(weight = `Cover at site (%)`))

## Flammability traits
# Within flammability traits
data %>%
  ggpairs(flammability,
          mapping = ggplot2::aes(colour = Site))

# Flammability data by site
data %>%
  ggduo("Site", flammability)

# Flammability data by species
data %>%
  select(all_of(c("Taxon", "Site", flammability))) %>%
  pivot_longer(cols = flammability) %>%
  ggplot(aes(y = value, x = fct_reorder(Taxon, as.numeric(as.factor(Site))), fill = Site)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(vars(name), scales = "free") +
    xlab("Species")
    

#  ggduo("Taxon", flammability, cardinality_threshold = 25)

# Flammability data by site, weighted by % cover of species
data %>%
  ggduo("Site", flammability,
        mapping = ggplot2::aes(weight = `Cover at site (%)`))


## Plant vs flammability traits

# Flammability vs discrete traits
data %>%
  ggduo(disc_trts, flammability)

# Flammability vs discrete traits, weighted by % cover of species
data %>%
  ggduo(disc_trts, flammability,
        mapping = ggplot2::aes(weight = `Cover at site (%)`))

# Flammability vs continuous traits
data %>%
  ggduo(cont_trts, flammability)

data %>%
  ggpairs(c(cont_trts, flammability))


