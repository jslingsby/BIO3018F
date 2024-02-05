library(tidyverse)
library(googlesheets4)
library(googledrive)
library(readxl)

if (Sys.getenv("USER") == "jasper") {gmail = "jasper.slingsby@uct.ac.za"}

# Authenticate and access the Google Sheet
drive_auth(email = gmail)
gs4_auth(token = drive_token())

# Download
sheet <- "https://docs.google.com/spreadsheets/d/1VYMvHk7b_GcVFxCLYt4GUyUIMfrgWasQT7uy6aE0_ms/edit#gid=1570965423"
drive_download(sheet, path = "prac/flammability/flammabilitytraits2024.xlsx", overwrite = TRUE)

# Read in data
leaf_trts <- read_xlsx("prac/flammability/flammabilitytraits2024.xlsx","TraitData")
fire_trts <- read_xlsx("prac/flammability/flammabilitytraits2024.xlsx","Flammability")

# Match species names to flammability data by code
nms <- leaf_trts %>% select(Taxon, SppCode) %>% unique()
flam_trts <- left_join(fire_trts, nms)


flam_trts %>% group_by(Taxon) %>% summarise(samplesN = n()) %>% print(n = 25)
