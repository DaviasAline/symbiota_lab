# 0_source_data_reading_cleaning
# A.Davias
# 08/03/2023

# Chargement des packages ----
library(readxl)
library(haven)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(questionr)
library(mice)
library(expss)

# Data reading ----
## Neuro ----
bdd_neuro <-                                                 
  read_dta("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/2018-01-04 Bayley Cognitive 1 and 2 year.dta") %>%
  rename(ident = SubjectNumber) %>%
  mutate(ident = as.character(ident))

## Covariables ----
bdd_microbiote_covariates <-                                 
  read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/CHILD data for Aline_Edmonton site.xlsx", 
             na = c("NA", "8888", "999")) %>%
  rename(ident = CHILD_Study_ID) %>%
  mutate(ident = as.character(ident))

bdd_bmi <- 
  read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/child_general_cohort J Forbes.xlsx") %>%
  select(SubjectNumber, mom_bmi_best) %>%
  rename(ident = SubjectNumber) %>%
  mutate(ident = as.character(ident))

bdd_age_sampling <- 
  read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/age_sampling (all infants with stool collected).xlsx") %>%
  rename(ident = SubjectNumber) %>%
  mutate(ident = as.character(ident)) %>%
  select(-ChildActualDOB)

## Microbiote 3M ----
taxa_3M <- 
  read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/3months_Taxonomic Abundances L2-L6.xlsx") %>%
  mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
  rename(ident = CHILD_ID) %>%
  mutate_if(is.numeric, ~ . * 100)

# phyla_3M <-
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/3months_Taxonomic Abundances L2-L6.xlsx") %>%
#   select(CHILD_ID, 2:10)%>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   rename(ident = CHILD_ID) %>%
#   mutate_if(is.numeric, ~ . * 100)
# phyla_3M_vec <- colnames(phyla_3M[2:10])
# 
# class_3M <-
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/3months_Taxonomic Abundances L2-L6.xlsx") %>%
#   select(CHILD_ID, 11:29)%>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   rename(ident = CHILD_ID) %>%
#   mutate_if(is.numeric, ~ . * 100)
# class_3M_vec <- colnames(class_3M[2:20])
# 
# order_3M <-
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/3months_Taxonomic Abundances L2-L6.xlsx") %>%
#   select(CHILD_ID, 30:60)%>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   rename(ident = CHILD_ID) %>%
#   mutate_if(is.numeric, ~ . * 100)
# order_3M_vec <- colnames(order_3M[2:32])
# 
# family_3M <-
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/3months_Taxonomic Abundances L2-L6.xlsx") %>%
#   select(CHILD_ID, 61:123)%>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   rename(ident = CHILD_ID) %>%
#   mutate_if(is.numeric, ~ . * 100)
# family_3M_vec <- colnames(family_3M[2:64])
# 
# genera_3M <-
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/3months_Taxonomic Abundances L2-L6.xlsx") %>%
#   select(CHILD_ID, 124:255) %>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   rename(ident = CHILD_ID) %>%
#   mutate_if(is.numeric, ~ . * 100)
# genera_3M_vec <- colnames(genera_3M[2:133])

bdd_taxa_3M <- taxa_3M %>% 
  select(ident, 
         "k__Bacteria;p__Bacteroidetes",
         "k__Bacteria;p__Proteobacteria", 
         "k__Bacteria;p__Firmicutes", 
         "k__Bacteria;p__Actinobacteria", 
         
         "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae;g__", 
         "k__Bacteria;p__Bacteroidetes;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Bacteroides", 
         "k__Bacteria;p__Actinobacteria;c__Actinobacteria;o__Bifidobacteriales;f__Bifidobacteriaceae;g__Bifidobacterium",
         "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Veillonellaceae;g__Veillonella",
         
         "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Streptococcaceae;g__Streptococcus", 
         "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Lactobacillaceae;g__Lactobacillus",          
         "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Enterococcaceae;g__Enterococcus",
         
         "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales"
         
         
  ) %>% 
  rename(p__Bacteroidetes_3M = "k__Bacteria;p__Bacteroidetes", 
         p__Proteobacteria_3M = "k__Bacteria;p__Proteobacteria", 
         p__Firmicutes_3M = "k__Bacteria;p__Firmicutes", 
         p__Actinobacteria_3M = "k__Bacteria;p__Actinobacteria",
         
         f__Enterobacteriaceae_g__3M = "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae;g__",
         g__Bacteroides_3M = "k__Bacteria;p__Bacteroidetes;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Bacteroides", 
         g__Bifidobacterium_3M = "k__Bacteria;p__Actinobacteria;c__Actinobacteria;o__Bifidobacteriales;f__Bifidobacteriaceae;g__Bifidobacterium",
         g__Veillonella_3M = "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Veillonellaceae;g__Veillonella", 
         
         g__Streptococcus_3M = "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Streptococcaceae;g__Streptococcus", 
         g__Lactobacillus_3M = "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Lactobacillaceae;g__Lactobacillus",          
         g__Enterococcus_3M = "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Enterococcaceae;g__Enterococcus",      
         
         o__Lactobacillales_3M = "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales"
  ) %>%
  mutate(
    p__Bacteroidetes_3M_ln = log(p__Bacteroidetes_3M), 
    p__Bacteroidetes_3M_cat3 = cut(p__Bacteroidetes_3M,
                                   include.lowest = TRUE,
                                   right = FALSE,
                                   dig.lab = 4,
                                   breaks = c(0, 0.246153846154, 52.9538461538, 99.2230769231)),
    p__Proteobacteria_3M_ln = log(p__Proteobacteria_3M),
    p__Firmicutes_3M_ln = log(p__Firmicutes_3M),
    p__Actinobacteria_3M_ln = log(p__Actinobacteria_3M),
    
    f__Enterobacteriaceae_g__3M_ln = log(f__Enterobacteriaceae_g__3M),
    g__Bacteroides_3M_ln = log(g__Bacteroides_3M), 
    g__Bacteroides_3M_cat3 = cut(g__Bacteroides_3M,
                                 include.lowest = TRUE,
                                 right = TRUE,
                                 dig.lab = 4,
                                 breaks = 3), 
    g__Bifidobacterium_3M_ln = log(g__Bifidobacterium_3M),
    g__Veillonella_3M_ln = log(g__Veillonella_3M),
    
    g__Streptococcus_3M_ln = log(g__Streptococcus_3M), 
    g__Streptococcus_3M_cat = ifelse(g__Streptococcus_3M > 0, "Yes", "No"),
    g__Lactobacillus_3M_ln = log(g__Lactobacillus_3M), 
    g__Lactobacillus_3M_cat = ifelse(g__Lactobacillus_3M > 0, "Yes", "No"),
    g__Enterococcus_3M_ln = log(g__Enterococcus_3M),  
    g__Enterococcus_3M_cat = ifelse(g__Enterococcus_3M > 0, "Yes", "No"))

bdd_Hein_3M <- read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/K3_cognitive_3mths-vairables.xlsx") %>%
  rename(ident = CHILD_ID, 
         Clusters_Hein_3M = Clusters) %>%
  mutate(
    ident = as.character(ident),
    Clusters_Hein_3M = as.character(Clusters_Hein_3M),
    Clusters_Hein_3M = fct_recode(Clusters_Hein_3M, 
                               "1: Bacteroidetes" = "1",
                               "2: Proteobacteria" = "2",
                               "3: Firmicutes" = "3"), 
    Clusters_Hein_3M = fct_relevel(Clusters_Hein_3M, 
                                "2: Proteobacteria", 
                                "3: Firmicutes", 
                                "1: Bacteroidetes"))

C_difficile_new_3M <- 
  read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/C difficile qPCR results-normalized LOD fix SB June 6 2023.xlsx") %>%
  filter(Type == "3-month Stool") %>%
  filter(is.na(Flag)) %>%
  select(ident = "CHILD ID", 
         Cdiff_bact_genomes_3M = "Cdiff_bact genomes", 
         C_diff_genomes_per_g_3M = "C diff genomes/g") %>%
  mutate(ident = as.character(ident), 
         Cdiff_num_3M = Cdiff_bact_genomes_3M, 
         Cdiff_cat_3M = ifelse(C_diff_genomes_per_g_3M == "< LOD", "No", "Yes"), 
         Cdiff_num_3M_ln = log(Cdiff_num_3M)) %>%
  select(ident, 
         Cdiff_num_3M,
         Cdiff_cat_3M,
         Cdiff_num_3M_ln)

## Microbiote 1Y ----
taxa_Y1 <- 
  read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/12months_Taxonomic Abundances L2-L6.xlsx") %>%
  mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
  rename(ident = CHILD_ID) %>%
  mutate_if(is.numeric, ~ . * 100)

# phyla_Y1 <- 
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/12months_Taxonomic Abundances L2-L6.xlsx") %>%
#   select(CHILD_ID, contains("p__"))%>%
#   select(CHILD_ID, !contains("c__"))%>%
#   select(CHILD_ID, !contains("o__")) %>%
#   select(CHILD_ID, !contains("f__")) %>%
#   select(CHILD_ID, !contains("g__")) %>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   rename(ident = CHILD_ID) %>%
#   mutate_if(is.numeric, ~ . * 100)
# phyla_Y1_vec <- colnames(phyla_Y1[2:10])
# 
# class_Y1 <- 
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/12months_Taxonomic Abundances L2-L6.xlsx") %>%
#   select(CHILD_ID, contains("c__"))%>%
#   select(CHILD_ID, !contains("o__")) %>%
#   select(CHILD_ID, !contains("f__")) %>%
#   select(CHILD_ID, !contains("g__")) %>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   rename(ident = CHILD_ID) %>%
#   mutate_if(is.numeric, ~ . * 100)
# class_Y1_vec <- colnames(class_Y1[2:20])
# 
# order_Y1 <- 
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/12months_Taxonomic Abundances L2-L6.xlsx") %>%
#   select(CHILD_ID, contains("o__")) %>%
#   select(CHILD_ID, !contains("f__")) %>%
#   select(CHILD_ID, !contains("g__")) %>%
#   select(CHILD_ID, !contains("Other")) %>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   rename(ident = CHILD_ID) %>%
#   mutate_if(is.numeric, ~ . * 100)
# order_Y1_vec <- colnames(order_Y1[2:32])
# 
# family_Y1 <- 
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/12months_Taxonomic Abundances L2-L6.xlsx") %>%
#   
#   #select(CHILD_ID, !contains("Other")) %>%
#   select(CHILD_ID, contains("f__")) %>%
#   select(CHILD_ID, !contains("g__")) %>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   rename(ident = CHILD_ID) %>%
#   mutate_if(is.numeric, ~ . * 100)
# family_Y1_vec <- colnames(family_Y1[2:62])
# 
# genera_Y1 <-
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/12months_Taxonomic Abundances L2-L6.xlsx") %>%
#   select(CHILD_ID, contains("g__"), contains("Other")) %>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   rename(ident = CHILD_ID) %>%
#   mutate_if(is.numeric, ~ . * 100)
# genera_Y1_vec <- colnames(genera_Y1[2:135])

bdd_taxa_Y1 <- taxa_Y1 %>% 
  select(ident, 
         "k__Bacteria;p__Bacteroidetes",
         "k__Bacteria;p__Proteobacteria", 
         "k__Bacteria;p__Firmicutes", 
         "k__Bacteria;p__Actinobacteria", 
         "k__Bacteria;p__Bacteroidetes;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Bacteroides", 
         "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Lachnospiraceae;g__",
         "k__Bacteria;p__Actinobacteria;c__Actinobacteria;o__Bifidobacteriales;f__Bifidobacteriaceae;g__Bifidobacterium",
         "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Veillonellaceae;g__Veillonella",                   
         "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Ruminococcaceae;g__Faecalibacterium", 
         
         "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Lactobacillaceae;g__Lactobacillus",           
         "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Enterococcaceae;g__Enterococcus",             
         "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Streptococcaceae;g__Streptococcus") %>% 
  rename(p__Bacteroidetes_Y1 = "k__Bacteria;p__Bacteroidetes", 
         p__Proteobacteria_Y1 = "k__Bacteria;p__Proteobacteria", 
         p__Firmicutes_Y1 = "k__Bacteria;p__Firmicutes", 
         p__Actinobacteria_Y1 = "k__Bacteria;p__Actinobacteria", 
         g__Bacteroides_Y1 = "k__Bacteria;p__Bacteroidetes;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Bacteroides", 
         f__Lachnospiraceae_g___Y1 = "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Lachnospiraceae;g__",
         g__Bifidobacterium_Y1 = "k__Bacteria;p__Actinobacteria;c__Actinobacteria;o__Bifidobacteriales;f__Bifidobacteriaceae;g__Bifidobacterium",
         g__Veillonella_Y1 = "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Veillonellaceae;g__Veillonella",                   
         g__Faecalibacterium_Y1 = "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Ruminococcaceae;g__Faecalibacterium", 
         g__Lactobacillus_Y1 = "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Lactobacillaceae;g__Lactobacillus",
         g__Enterococcus_Y1 = "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Enterococcaceae;g__Enterococcus",             
         g__Streptococcus_Y1 = "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Streptococcaceae;g__Streptococcus" 
  ) %>%
  mutate(
    p__Proteobacteria_Y1_ln = log(p__Proteobacteria_Y1),
    p__Firmicutes_Y1_ln = log(p__Firmicutes_Y1),
    p__Actinobacteria_Y1_ln = log(p__Actinobacteria_Y1),
    p__Bacteroidetes_Y1_cat3 = cut(p__Bacteroidetes_Y1,
                                   include.lowest = TRUE,
                                   right = TRUE,
                                   dig.lab = 4,
                                   breaks = 3), 
    g__Bacteroides_Y1_ln = log(g__Bacteroides_Y1), 
    f__Lachnospiraceae_g___Y1_ln = log(f__Lachnospiraceae_g___Y1), 
    g__Bifidobacterium_Y1_ln = log(g__Bifidobacterium_Y1), 
    g__Veillonella_Y1_ln = log(g__Veillonella_Y1), 
    g__Faecalibacterium_Y1_ln = log(g__Faecalibacterium_Y1), 
    
    g__Lactobacillus_Y1_ln = log(g__Lactobacillus_Y1), 
    g__Enterococcus_Y1_ln = log(g__Enterococcus_Y1), 
    g__Streptococcus_Y1_ln = log(g__Streptococcus_Y1), 
    g__Lactobacillus_Y1_cat = ifelse(g__Lactobacillus_Y1 > 0, "Yes", "No"), 
    g__Enterococcus_Y1_cat = ifelse(g__Enterococcus_Y1 > 0, "Yes", "No"), 
    g__Streptococcus_Y1_cat = ifelse(g__Streptococcus_Y1 > 0, "Yes", "No"))

#bdd_taxa_Y1 %>% filter(ident =="20664") ## ident 20664 est présent 2 fois: que faire ? 


C_difficile_new_Y1 <- 
  read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/C difficile qPCR results-normalized LOD fix SB June 6 2023.xlsx") %>%
  filter(Type == "1-year Stool") %>%
  filter(is.na(Flag)) %>%
  select(ident = "CHILD ID", 
         Cdiff_bact_genomes_12M = "Cdiff_bact genomes", 
         C_diff_genomes_per_g_12M = "C diff genomes/g") %>%
  mutate(ident = as.character(ident), 
         Cdiff_num_Y1 = Cdiff_bact_genomes_12M, 
         Cdiff_num_Y1_ln = log(Cdiff_num_Y1),
         Cdiff_cat_Y1 = ifelse(C_diff_genomes_per_g_12M == "< LOD", "No", "Yes"), 
         to_include = ifelse(ident == "20664" & Cdiff_cat_Y1 == "No", "No", "Yes")) %>%  # there is a problem: child 20664 is duplicated at one year
  filter(to_include == "Yes") %>%        # we exclude the 20664 row where Cdiff is not detected
  select(
    ident, 
    Cdiff_num_Y1, 
    Cdiff_cat_Y1, 
    Cdiff_num_Y1_ln)

bdd_Hein_Y1 <- 
  read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/12m_Microbiotaclusters_ND.xlsx") %>%
  rename(
    ident = CHILD_ID, 
    Clusters_Hein_Y1 = Clusters) %>%
  mutate(
    ident = as.character(ident))

## Metabolites 3M ----
bdd_metabolites <- read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/Copy of NMR_N705_Combined data Oct302019.xlsx") %>%
  mutate(
    SubjectNumber = as.factor(as.character(SubjectNumber)),
    Sex = case_when(Sex == 1 ~ "Male", 
                    Sex == 2 ~ "Female", 
                    .default = NA), 
    Sex = as.factor(Sex)) %>%
  select(-SubjectType, -StudyCentre, -SampleID, -Group, -Diaper, -ChildActualDOB) %>%
  rename_with(~paste("metabolite", ., sep = "_")) %>%
  rename(ident = metabolite_SubjectNumber, 
         Sex = metabolite_Sex) %>%
  mutate(
    metabolite_Acetate_ln = log(metabolite_Acetate),
    metabolite_Propionate_ln = log(metabolite_Propionate), 
    metabolite_Butyrate_ln = log(metabolite_Butyrate), 
    metabolite_Tryptophan_ln = log(metabolite_Tryptophan), 
    metabolite_Taurine_ln = log(metabolite_Taurine), 
    metabolite_Lactate_ln = log(metabolite_Lactate), 
    metabolite_Formate_ln = log(metabolite_Formate), 
    metabolite_Tyrosine_ln = log(metabolite_Tyrosine), 
    metabolite_Valerate_ln = log(metabolite_Valerate)) 


# Data merging ----
bdd <-
  list(
    bdd_neuro,
    bdd_microbiote_covariates,
    bdd_bmi,
    bdd_age_sampling,
    
    bdd_Hein_3M,
    bdd_taxa_3M,
    C_difficile_new_3M,
    
    bdd_Hein_Y1,
    bdd_taxa_Y1,
    C_difficile_new_Y1) %>%
  reduce(full_join, by = "ident")

bdd <- full_join(bdd, bdd_metabolites, by = c("ident", "Sex"))

# Data cleaning ----
bdd <- bdd %>%
    mutate(
      ident = as.character(ident),
      Patient_ID_num = as.character(Patient_ID_num),
      StudyCenter = as.factor(StudyCenter),
      site = as.factor(site),
      EnrolmentDate = ymd(EnrolmentDate), 
      ChildActualDOB = ymd(ChildActualDOB),
      Sex = as.factor(Sex),            
      Birthwtgrams = as.numeric(Birthwtgrams),
      BirthwtCat = as.factor(BirthwtCat),
      GA = as.numeric(GA), 
      GACat = as.factor(GACat),
      Ethnicity = as.factor(Ethnicity),
      Marital_status = as.factor(Marital_status),
      Mat_education = as.factor(Mat_education),
      MotherDOB = ymd(MotherDOB),
      Mom_age = as.numeric(Mom_age),
      Smoke_preg = as.factor(Smoke_preg),
      BFStatus3m = as.factor(BFStatus3m),
      Birthmode3 = as.factor(Birthmode3),
      BirthmodeIAP = as.factor(BirthmodeIAP),
      sIgA_3m = as.numeric(sIgA_3m),
      sIgA_12mo = as.numeric(sIgA_12mo),
      Cluster_3M = as.factor(Cluster_3M),
      Cluster_12M = as.factor(Cluster_12M), 
      trajectory = as.factor(trajectory),
      mom_bmi_best = as.numeric(mom_bmi_best)) %>%
  mutate(
    BirthwtCat = fct_recode(BirthwtCat,
                            "<3000" = "<3000g",
                            ">= 4000" = "> or equal to 4000",
                            "3000-3499" = "3000 to <3500",
                            "3500-3999" = "3500 to <4000"),
    Cluster_3M = fct_recode(Cluster_3M,
                            "C1 (Enterobacteriaceae)" = "C1",
                            "C2 (Bacteroides)" = "C2"),
    Cluster_12M = fct_recode(Cluster_12M,
                            "C1 (Enterobacteriaceae)" = "C1",
                            "C2 (Bacteroides)" = "C2"),
    BirthwtCat = fct_relevel(BirthwtCat,
                             "<3000", "3000-3499", "3500-3999", ">= 4000"),
    GACat = fct_recode(GACat,
                       "Early term (37-38)" = "Early term (37-38wks)",
                       "Full term (39-40)" = "Full term (39-40wks)",
                       "Late term (41+)" = "Late term (41+ wks)",
                       "Preterm (34-36)" = "Preterm (34-36wks)"),
    GACat = fct_relevel(GACat,
                        "Preterm (34-36)", "Early term (37-38)", "Full term (39-40)", "Late term (41+)"),
    Marital_status = fct_relevel(Marital_status,
                                 "Married or common law", "Divorced or seperated", "Single"),
    Birthmode3 = fct_relevel(Birthmode3,
                             "Vaginal", "Emergency CS", "Elective CS"),
    BirthmodeIAP = fct_relevel(BirthmodeIAP,
                               "Vaginal noIAP", "Vaginal IAP", "Emergency CS", "Elective CS"), 
    Marital_status_2 =   fct_recode(Marital_status, 
                                    "Divorced, seperated or single" = "Divorced or seperated",
                                    "Divorced, seperated or single" = "Single"),
    SES18WKQ9 = as.character(SES18WKQ9),
    family_income = fct_recode(SES18WKQ9,
                               "$0 - $9,999" = "1",
                               "$10,000 - $19,999" = "2",
                               "$20,000 - $29,999" = "3",
                               "$30,000 - $39,999" = "4",
                               "$40,000 - $49,999" = "5",
                               "$50,000 - $59,999" = "6",
                               "$60,000 - $79,999" = "7",
                               "$80,000 - $99,999" = "8",
                               "$100,000 - $149,999" = "9",
                               "$150,000 or over" = "10",
                               NULL = "11"),
    family_income = fct_relevel(family_income,
                                "$0 - $9,999", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999",
                                "$40,000 - $49,999", "$50,000 - $59,999", "$60,000 - $79,999",
                                "$80,000 - $99,999", "$100,000 - $149,999", "$150,000 or over"), 
    Mat_education = fct_relevel(Mat_education,
                                "High school or less", "Some post secondary", "Postgrad degree", "University degree"), 
    Ethnicity = fct_relevel(Ethnicity, "White", "Asian", "First nation", "Other"), 
    BFStatus3m = fct_relevel(BFStatus3m, "None", "Partial", "Exclusive"))

# Variable creation ----
bdd <- bdd %>%
  mutate(
    BirthwtKg = Birthwtgrams / 1000,
    
    family_income_3cat = fct_recode(family_income,
                                    "<$40,000" = "$0 - $9,999",
                                    "<$40,000" = "$10,000 - $19,999",
                                    "<$40,000" = "$20,000 - $29,999",
                                    "<$40,000" = "$30,000 - $39,999",
                                    "$40,000 - $79,999" = "$40,000 - $49,999",
                                    "$40,000 - $79,999" = "$50,000 - $59,999",
                                    "$40,000 - $79,999" = "$60,000 - $79,999",
                                    "$80,000 and over" = "$80,000 - $99,999",
                                    "$80,000 and over" = "$100,000 - $149,999",
                                    "$80,000 and over" = "$150,000 or over"), 
    Birthmode_2cat =  fct_recode(Birthmode3, 
                                 "C-section" = "Emergency CS",
                                 "C-section" = "Elective CS"), 
    sIgA_3m_ln = log(sIgA_3m),                      # ici on log transforme les variables sIgA
    sIgA_3m_cat_CutOff7 = cut(sIgA_3m,                      # ici on catégorise les variables sIgA avec un cut off à 7ng/dL
                              include.lowest = TRUE,
                              right = FALSE,
                              dig.lab = 4,
                              breaks = c(0, 7, 50)),
    sIgA_3m_cat_CutOff7 = fct_recode(sIgA_3m_cat_CutOff7,
                                     "<7 ng/dL" = "[0,7)",
                                     ">=7 ng/dL" = "[7,50]"), 
    sIgA_3m_cat_CutOff5 = cut(sIgA_3m,                      # ici on catégorise les variables sIgA avec un cut off à 5ng/dL
                              include.lowest = TRUE,
                              right = FALSE,
                              dig.lab = 4,
                              breaks = c(0, 5, 50)),
    sIgA_3m_cat_CutOff5 = fct_recode(sIgA_3m_cat_CutOff5,
                                     "<5 ng/dL" = "[0,5)",
                                     ">=5 ng/dL" = "[5,50]"), 
    sIgA_3m_cat_CutOff4 = cut(sIgA_3m,                      # ici on catégorise les variables sIgA avec un cut off à 4ng/dL
                              include.lowest = TRUE,
                              right = FALSE,
                              dig.lab = 4,
                              breaks = c(0, 4, 50)),
    sIgA_3m_cat_CutOff4 = fct_recode(sIgA_3m_cat_CutOff4,
                                     "<4 ng/dL" = "[0,4)",
                                     ">=4 ng/dL" = "[4,50]"), 
    
    
    sIgA_12mo_ln = log(sIgA_12mo),
    sIgA_12mo_cat_CutOff7 = cut(sIgA_12mo,
                        include.lowest = TRUE,
                        right = FALSE,
                        dig.lab = 4,
                        breaks = c(0, 7, 50)), 
    sIgA_12mo_cat_CutOff7 = fct_recode(sIgA_12mo_cat_CutOff7,
                               "<7 ng/dL" = "[0,7)",
                               ">=7 ng/dL" = "[7,50]"), 
    sIgA_12mo_cat_CutOff5 = cut(sIgA_12mo,
                        include.lowest = TRUE,
                        right = FALSE,
                        dig.lab = 4,
                        breaks = c(0, 5, 50)), 
    sIgA_12mo_cat_CutOff5 = fct_recode(sIgA_12mo_cat_CutOff5,
                               "<5 ng/dL" = "[0,5)",
                               ">=5 ng/dL" = "[5,50]"), 

    BFStatus3m_sIgA_3m_CutOff7 = case_when(
      
      BFStatus3m == "Exclusive" & sIgA_3m_cat_CutOff7 == "<7 ng/dL" ~ "Low sIgA levels at 3 months in breastfed children",
      BFStatus3m == "Partial" & sIgA_3m_cat_CutOff7 == "<7 ng/dL" ~ "Low sIgA levels at 3 months in breastfed children",
                                           
      BFStatus3m == "None" & sIgA_3m_cat_CutOff7 == "<7 ng/dL" ~ "Low sIgA levels at 3 months in not breastfed children",
                                           
      BFStatus3m == "Exclusive" & sIgA_3m_cat_CutOff7 == ">=7 ng/dL" ~ "High sIgA levels at 3 months in breastfed children",
      BFStatus3m == "Partial" & sIgA_3m_cat_CutOff7 == ">=7 ng/dL" ~ "High sIgA levels at 3 months in breastfed children",
       
      BFStatus3m == "None" & sIgA_3m_cat_CutOff7 == ">=7 ng/dL" ~ "High sIgA levels at 3 months in not breastfed children"), 
    
    BFStatus3m_sIgA_12m_CutOff7 = case_when(
      
      BFStatus3m == "Exclusive" & sIgA_12mo_cat_CutOff7 == "<7 ng/dL" ~ "Breastfed at 3 months / low sIgA levels at 12 months",
      BFStatus3m == "Partial" & sIgA_12mo_cat_CutOff7 == "<7 ng/dL" ~ "Breastfed at 3 months / low sIgA levels at 12 months",
     
      BFStatus3m == "None" & sIgA_12mo_cat_CutOff7 == "<7 ng/dL" ~ "Not breastfed at 3 months / low sIgA levels at 12 months",
     
      BFStatus3m == "Exclusive" & sIgA_12mo_cat_CutOff7 == ">=7 ng/dL" ~ "Breastfed at 3 months / high sIgA levels at 12 months",
      BFStatus3m == "Partial" & sIgA_12mo_cat_CutOff7 == ">=7 ng/dL" ~ "Breastfed at 3 months / high sIgA levels at 12 months",
     
      BFStatus3m == "None" & sIgA_12mo_cat_CutOff7 == ">=7 ng/dL" ~ "Not breastfed at 3 months / high sIgA levels at 12 months"), 
    
    BFStatus3m_sIgA_3m_CutOff5 = case_when(
      
      BFStatus3m == "Exclusive" & sIgA_3m_cat_CutOff5 == "<5 ng/dL" ~ "Low sIgA levels at 3 months in breastfed children",
      BFStatus3m == "Partial" & sIgA_3m_cat_CutOff5 == "<5 ng/dL" ~ "Low sIgA levels at 3 months in breastfed children",
     
      BFStatus3m == "None" & sIgA_3m_cat_CutOff5 == "<5 ng/dL" ~ "Low sIgA levels at 3 months in not breastfed children",
     
      BFStatus3m == "Exclusive" & sIgA_3m_cat_CutOff5 == ">=5 ng/dL" ~ "High sIgA levels at 3 months in breastfed children",
      BFStatus3m == "Partial" & sIgA_3m_cat_CutOff5 == ">=5 ng/dL" ~ "High sIgA levels at 3 months in breastfed children",
     
      BFStatus3m == "None" & sIgA_3m_cat_CutOff5 == ">=5 ng/dL" ~ "High sIgA levels at 3 months in not breastfed children"), 
    
    BFStatus3m_sIgA_12m_CutOff5 = case_when(
      
      BFStatus3m == "Exclusive" & sIgA_12mo_cat_CutOff5 == "<5 ng/dL" ~ "Breastfed at 3 months / low sIgA levels at 12 months",
      BFStatus3m == "Partial" & sIgA_12mo_cat_CutOff5 == "<5 ng/dL" ~ "Breastfed at 3 months / low sIgA levels at 12 months",
      
      BFStatus3m == "None" & sIgA_12mo_cat_CutOff5 == "<5 ng/dL" ~ "Not breastfed at 3 months / low sIgA levels at 12 months",
      
      BFStatus3m == "Exclusive" & sIgA_12mo_cat_CutOff5 == ">=5 ng/dL" ~ "Breastfed at 3 months / high sIgA levels at 12 months",
      BFStatus3m == "Partial" & sIgA_12mo_cat_CutOff5 == ">=5 ng/dL" ~ "Breastfed at 3 months / high sIgA levels at 12 months",
      
      BFStatus3m == "None" & sIgA_12mo_cat_CutOff5 == ">=5 ng/dL" ~ "Not breastfed at 3 months / high sIgA levels at 12 months"), 
    
    BFStatus3m_sIgA_3m_CutOff7 =  fct_relevel(BFStatus3m_sIgA_3m_CutOff7,
                                              "Low sIgA levels at 3 months in not breastfed children", 
                                              "Low sIgA levels at 3 months in breastfed children",
                                              "High sIgA levels at 3 months in not breastfed children", 
                                              "High sIgA levels at 3 months in breastfed children"), 
    BFStatus3m_sIgA_3m_CutOff5 = fct_relevel(BFStatus3m_sIgA_3m_CutOff5,
                                             "Low sIgA levels at 3 months in not breastfed children", 
                                             "Low sIgA levels at 3 months in breastfed children",
                                             "High sIgA levels at 3 months in not breastfed children",
                                             "High sIgA levels at 3 months in breastfed children"),
    BFStatus3m_sIgA_12m_CutOff7 = fct_relevel(BFStatus3m_sIgA_12m_CutOff7,
                                              "Not breastfed at 3 months / low sIgA levels at 12 months",
                                              "Breastfed at 3 months / low sIgA levels at 12 months", 
                                              "Breastfed at 3 months / high sIgA levels at 12 months"),
    BFStatus3m_sIgA_12m_CutOff5 =  fct_relevel(BFStatus3m_sIgA_12m_CutOff5,
                                               "Not breastfed at 3 months / low sIgA levels at 12 months",
                                               "Breastfed at 3 months / low sIgA levels at 12 months", 
                                               "Breastfed at 3 months / high sIgA levels at 12 months"), 
    BFStatus3m_2cat = fct_recode(BFStatus3m,
                                 "No" = "None",
                                 "Yes" = "Partial",
                                 "Yes" = "Exclusive"),
    BFStatus3m_2cat_bis = fct_recode(BFStatus3m,
                                 "No" = "None",
                                 "No" = "Partial",
                                 "Yes" = "Exclusive"))


# Choix des labels ----
bdd = modify(bdd, {
  var_lab(EnrolmentDate) = "Date of enrolment"
  var_lab(ChildActualDOB) = "Child date of birth"
  var_lab(Sex) = "Child sex"            
  var_lab(Birthwtgrams) = "Weight at birth (g)"
  var_lab(BirthwtKg) = "Weight at birth (Kg)"
  var_lab(BirthwtCat) = "Weight at birth (g)"
  var_lab(GA) = "Gestational age (weeks)"
  var_lab(GACat) = "Gestational age (weeks)"
  var_lab(Ethnicity) = "Maternal ethnicity"
  var_lab(Marital_status) = "Maternal marital status"
  var_lab(Marital_status_2) = "Maternal marital status"
  var_lab(Mat_education) = "Maternal education"
  var_lab(MotherDOB) = "Maternal birth date"
  var_lab(Mom_age) = "Maternal age"
  var_lab(Smoke_preg) = "Smoking status during pregnancy"
  var_lab(BFStatus3m) = "Breastfeeding status at 3 months"
  var_lab(Birthmode3) = "Delivery mode"
  var_lab(BirthmodeIAP) = "Delivery mode"
  var_lab(Birthmode_2cat) = "Delivery mode"
  var_lab(sIgA_3m) = "Secretory IgA at 3 months (mg/dL)"
  var_lab(sIgA_12mo) = "Secretory IgA at 12 months (mg/dL)"
  var_lab(sIgA_3m_ln) = "Secretory IgA at 3 months (mg/dL)"
  var_lab(sIgA_12mo_ln) = "Secretory IgA at 12 months (mg/dL)"
  var_lab(sIgA_3m_cat_CutOff5) = "Secretory IgA at 3 months"
  var_lab(sIgA_12mo_cat_CutOff5) = "Secretory IgA at 12 months"
  var_lab(sIgA_3m_cat_CutOff7) = "Secretory IgA at 3 months"
  var_lab(sIgA_12mo_cat_CutOff7) = "Secretory IgA at 12 months"
  var_lab(BFStatus3m_sIgA_3m_CutOff7) = "Breastfeeding status at 3 months and secretory level IgA at 3 months"
  var_lab(BFStatus3m_sIgA_3m_CutOff5) = "Breastfeeding status at 3 months and secretory level IgA at 3 months"
  var_lab(BFStatus3m_sIgA_12m_CutOff7) = "Breastfeeding status at 3 months and secretory level IgA at 12 months"
  var_lab(BFStatus3m_sIgA_12m_CutOff5) = "Breastfeeding status at 3 months and secretory level IgA at 12 months"
  var_lab(Cluster_3M) = "Gut microbiota enterotypes at 3 months"
  var_lab(Cluster_12M) = "Gut microbiota enterotypes at 12 months"
  var_lab(trajectory) = "Enteropype evolution between 3 and 12 months"
  var_lab(PRNMH18WQ3_1a) = "Caucasian (Y/N)"
  var_lab(PRNMH18WQ3_1b) = "Indo_canadian (Y/N)"
  var_lab(PRNMH18WQ3_1c) = "Black (Y/N)"
  var_lab(PRNMH18WQ3_1d) = "Japanese (Y/N)"
  var_lab(PRNMH18WQ3_1e) = "Filipino (Y/N)"
  var_lab(PRNMH18WQ3_1f) = "Middle Eastern (Y/N)" 
  var_lab(PRNMH18WQ3_1g) = "Chinese (HongKong/Taiwan/Mainland China)(Y/N)"
  var_lab(PRNMH18WQ3_1h) = "South East Asian (Cambodia/Vietnam/Laos/Malaysia/Thai) (Y/N)"
  var_lab(PRNMH18WQ3_1i) = "First Nations (Y/N)"
  var_lab(PRNMH18WQ3_1j) = "Unknown (Y/N)"
  var_lab(PRNMH18WQ3_1k) = "Hispanic (Y/N)" 
  var_lab(PRNMH18WQ3_1l) = "None of these groups (specify below) (Y/N)" 
  var_lab(PRNMH18WQ3_1m) = "South Asian (Indian continent) (Y/N)"
  var_lab(PRNMH18WQ3_1n) = "open question ethnicity"
  var_lab(PRNMH18WQ1) = "Born in Canada (Y/N)"
  var_lab(PRNMH18WQ1_1) = "If no at PRNMH18WQ1, what country were you born in?"
  var_lab(PRNMH18WQ2) = "How long have you lived in Canada?"
  var_lab(PRNMH18WQ38) = "What is your marital status?"
  var_lab(SES18WKQ1) = "What is your highest educational level achieved?"      
  var_lab(SES18WKQ2) = "How many years of education have you had?"
  var_lab(SES18WKQ9) = "Best estimate of the total income, usehold members, in the past 12 months."
  var_lab(family_income) = "Family income, in the past 12 months"
  var_lab(family_income_3cat) = "Family income, in the past 12 months"
  var_lab(mom_bmi_best) = "Maternal BMI before pregnancy"
  var_lab(BFStatus3m_2cat) = "Breastfeeding status (Exclusive and partial VS not)"
  var_lab(BFStatus3m_2cat_bis) = "Breastfeeding status (Exclusive VS partial and not)"
})

bdd = modify(bdd, {
  var_lab(Clusters_Hein_3M) = "Gut microbiota enterotypes at 3 months"
  
  var_lab(p__Proteobacteria_3M) = "Phylum Proteobacteria at 3 months"
  var_lab(p__Proteobacteria_3M_ln) = "Phylum Proteobacteria at 3 months"
  var_lab(p__Firmicutes_3M) = "Phylum Firmicutes at 3 months"            
  var_lab(p__Firmicutes_3M_ln) = "Phylum Firmicutes at 3 months"  
  var_lab(p__Actinobacteria_3M) = "Phylum Actinobacteria at 3 months"
  var_lab(p__Actinobacteria_3M_ln) = "Phylum Actinobacteria at 3 months"
  var_lab(p__Bacteroidetes_3M) = "Phylum Bacteroidetes at 3 months"
  var_lab(p__Bacteroidetes_3M_cat3) = "Phylum Bacteroidetes at 3 months"
  
  var_lab(f__Enterobacteriaceae_g__3M) = "Family Enterobacteriaceae (unamed genus) at 3 months"
  var_lab(f__Enterobacteriaceae_g__3M_ln) = "Family Enterobacteriaceae (unamed genus) at 3 months"
  var_lab(g__Bacteroides_3M) = "Genus Bacteroides at 3 months"
  var_lab(g__Bacteroides_3M_ln) = "Genus Bacteroides at 3 months"
  var_lab(g__Bacteroides_3M_cat3) = "Genus Bacteroides at 3 months"
  var_lab(g__Bifidobacterium_3M) = "Genus Bifidobacterium at 3 months"
  var_lab(g__Bifidobacterium_3M_ln) = "Genus Bifidobacterium at 3 months"
  var_lab(g__Veillonella_3M) = "Genus Veillonella at 3 months"
  var_lab(g__Veillonella_3M_ln) = "Genus Veillonella at 3 months"
  
  var_lab(g__Streptococcus_3M) = "Genus Streptococcus at 3 months"
  var_lab(g__Streptococcus_3M_ln) = "Genus Streptococcus at 3 months"
  var_lab(g__Lactobacillus_3M) = "Genus Lactobacillus at 3 months"
  var_lab(g__Lactobacillus_3M_ln) = "Genus Lactobacillus at 3 months"
  var_lab(g__Enterococcus_3M) = "Genus Enterococcus at 3 months"
  var_lab(g__Enterococcus_3M_ln) = "Genus Enterococcus at 3 months"
  
  var_lab(Cdiff_num_3M) = "Clostridium difficile at 3 months" 
  var_lab(Cdiff_cat_3M) = "Clostridium difficile at 3 months" 
  var_lab(Cdiff_num_3M_ln) = "Clostridium difficile at 3 months"
  
  var_lab(metabolite_Acetate) = "Acetate metabolite at 3 months"
  var_lab(metabolite_Propionate) = "Propionate metabolite at 3 months"
  var_lab(metabolite_Butyrate) = "Butyrate metabolite at 3 months"
  
  var_lab(metabolite_Acetate_ln) = "Acetate metabolite at 3 months"
  var_lab(metabolite_Propionate_ln) = "Propionate metabolite at 3 months"
  var_lab(metabolite_Butyrate_ln) = "Butyrate metabolite at 3 months"
  
  var_lab(metabolite_Tryptophan) = "Tryptophan metabolite at 3 months"
  var_lab(metabolite_Taurine) = "Taurine metabolite at 3 months"
  var_lab(metabolite_Lactate) = "Lactate metabolite at 3 months"
  var_lab(metabolite_Formate) = "Formate metabolite at 3 months"
  
  var_lab(metabolite_Tryptophan_ln) = "Tryptophan metabolite at 3 months"
  var_lab(metabolite_Taurine_ln) = "Taurine metabolite at 3 months"
  var_lab(metabolite_Lactate_ln) = "Lactate metabolite at 3 months"
  var_lab(metabolite_Formate_ln) = "Formate metabolite at 3 months"
  
  var_lab(Clusters_Hein_Y1) = "Gut microbiota enterotypes at 12 months"
  var_lab(p__Proteobacteria_Y1) = "Phylum Proteobacteria at 12 months"
  var_lab(p__Proteobacteria_Y1_ln) = "Phylum Proteobacteria at 12 months"
  var_lab(p__Firmicutes_Y1) = "Phylum Firmicutes at 12 months"    
  var_lab(p__Firmicutes_Y1_ln) = "Phylum Firmicutes at 12 months"          
  var_lab(p__Actinobacteria_Y1) = "Phylum Actinobacteria at 12 months"  
  var_lab(p__Actinobacteria_Y1_ln) = "Phylum Actinobacteria at 12 months"
  var_lab(p__Bacteroidetes_Y1) = "Phylum Bacteroidetes at 12 months"
  var_lab(p__Bacteroidetes_Y1_cat3) = "Phylum Bacteroidetes at 12 months"
  
  var_lab(g__Bacteroides_Y1) = "Genus Bacteroides at 12 months"
  var_lab(g__Bacteroides_Y1_ln) = "Genus Bacteroides at 12 months"
  var_lab(f__Lachnospiraceae_g___Y1) = "Family Lachnospiraceae (unamed genus) at 12 months"
  var_lab(f__Lachnospiraceae_g___Y1_ln) = "Family Lachnospiraceae (unamed genus) at 12 months"
  var_lab(g__Bifidobacterium_Y1) = "Genus Bifidobacterium at 12 months"
  var_lab(g__Bifidobacterium_Y1_ln) = "Genus Bifidobacterium at 12 months"
  var_lab(g__Veillonella_Y1) = "Genus Veillonella at 12 months"
  var_lab(g__Veillonella_Y1_ln) = "Genus Veillonella at 12 months"
  var_lab(g__Faecalibacterium_Y1) = "Genus Faecalibacterium at 12 months"
  var_lab(g__Faecalibacterium_Y1_ln) = "Genus Faecalibacterium at 12 months"
  var_lab(g__Streptococcus_Y1) = "Genus Streptococcus at 12 months"
  var_lab(g__Streptococcus_Y1_ln) = "Genus Streptococcus at 12 months"
  var_lab(g__Lactobacillus_Y1) = "Genus Lactobacillus at 12 months"
  var_lab(g__Lactobacillus_Y1_ln) = "Genus Lactobacillus at 12 months"
  var_lab(g__Enterococcus_Y1) = "Genus Enterococcus at 12 months"
  var_lab(g__Enterococcus_Y1_ln) = "Genus Enterococcus at 12 months"
  var_lab(Cdiff_num_Y1) = "Clostridium difficile at 12 months" 
  var_lab(Cdiff_cat_Y1) = "Clostridium difficile at 12 months"  
  var_lab(Cdiff_num_Y1_ln) = "Clostridium difficile at 12 months" 
})

# Choix des vecteurs ----
vec_outcome <- bdd %>% select( "BAYLEYQ4_3_12", "BAYLEYQ4_3_24") %>% colnames()
vec_covar_cat <- bdd %>% select("Sex", 
                                "BirthwtCat", 
                                "GACat", 
                                "Ethnicity", 
                                "Marital_status_2", 
                                "Mat_education", 
                                "Smoke_preg", 
                                "BFStatus3m", 
                                "Birthmode3", 
                                "Birthmode_2cat", 
                                "family_income_3cat")  %>% colnames()
vec_covar_num <- bdd %>% select("BirthwtKg","GA", "Mom_age") %>% colnames()
