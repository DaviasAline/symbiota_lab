# Aline Davias 
# 09 avril 2024

source("~/5. R projects/symbiota_lab/3_programs/1_functions.R", echo=TRUE)
load("2_final_data/final_results_18_03_2024.RData")


explanatory_vars <- 
  c("Clusters_Hein_3M", 
    "p__Proteobacteria_3M_ln", 
    "p__Firmicutes_3M_ln", 
    "p__Bacteroidetes_3M_cat3", 
    "p__Actinobacteria_3M_ln", 
    "f__Enterobacteriaceae_g__3M_ln", 
    "g__Bacteroides_3M_cat3", 
    "g__Bifidobacterium_3M_ln", 
    "g__Veillonella_3M_ln", 
    "g__Streptococcus_3M_ln", 
     "g__Lactobacillus_3M_cat", 
    "g__Enterococcus_3M_cat", 
    "Cdiff_num_3M_ln", 
    #"Cdiff_cat_3M", 
    "metabolite_Acetate_ln", 
    "metabolite_Propionate_ln", 
    "metabolite_Butyrate_ln", 
    "metabolite_Tryptophan_ln", 
    "metabolite_Lactate_ln", 
    "metabolite_Formate_ln", 
    "metabolite_Tyrosine_ln", 
    "metabolite_Valerate_ln", 
    
    "Clusters_Hein_Y1", 
    "p__Proteobacteria_Y1_ln", 
    "p__Firmicutes_Y1_ln", 
    "p__Bacteroidetes_Y1_cat3", 
    "p__Actinobacteria_Y1_ln", 
    "g__Bacteroides_Y1", 
    "f__Lachnospiraceae_g___Y1_ln", 
    "g__Bifidobacterium_Y1_ln", 
    "g__Veillonella_Y1_ln", 
    "g__Faecalibacterium_Y1_ln", 
    "g__Streptococcus_Y1_ln", 
    "g__Lactobacillus_Y1_cat", 
    "g__Enterococcus_Y1_cat", 
    "Cdiff_num_Y1_ln" 
    #"Cdiff_cat_Y1"
    )

outcomes <- c("BAYLEYQ4_3_12", "BAYLEYQ4_3_24")

bdd_direct_paths <- bdd_filtered %>%
  select(all_of(explanatory_vars),
         all_of(outcomes),
         sIgA_3m_cat_CutOff5,
         sIgA_3m_ln,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  mutate(across(where(is.numeric), ~na_if(.x, -Inf)))

var_lab(bdd_direct_paths$g__Lactobacillus_3M_cat) <- "Genus Lactobacillus at 3 months"
var_lab(bdd_direct_paths$g__Enterococcus_3M_cat) <- "Genus Enterococcus at 3 months"
var_lab(bdd_direct_paths$metabolite_Tyrosine_ln) <- "Tyrosine metabolite at 3 months"
var_lab(bdd_direct_paths$metabolite_Valerate_ln) <- "Valerate metabolite at 3 months"
var_lab(bdd_direct_paths$g__Lactobacillus_Y1_cat) <-  "Genus Lactobacillus at 12 months"
var_lab(bdd_direct_paths$g__Enterococcus_Y1_cat) <- "Genus Enterococcus at 12 months"


# direct path A : GM 3M --> sIgA ----
## version régression logistique ----
logistic_reg <- function(explanatory_var) {
  results <- tbl_regression(
    glm(sIgA_3m_cat_CutOff5 ~ explanatory_var, data = bdd_direct_paths, family = binomial(logit)), 
    exponentiate = TRUE, 
    estimate_fun = scales::label_number(accuracy = .01, decimal.mark = "."),
    pvalue_fun = custom_pvalue_fun) %>%
    bold_labels() %>%
    bold_p(t=0.1) %>%
    add_n() %>%
    add_global_p(keep = TRUE) 
  return(results)
} 


path_a_logistic <- tbl_stack(
  tbls = list(
    logistic_reg(bdd_direct_paths$Clusters_Hein_3M), 
    logistic_reg(bdd_direct_paths$p__Proteobacteria_3M_ln), 
    logistic_reg(bdd_direct_paths$p__Firmicutes_3M_ln),
    logistic_reg(bdd_direct_paths$p__Bacteroidetes_3M_cat3),
    logistic_reg(bdd_direct_paths$p__Actinobacteria_3M_ln),
    logistic_reg(bdd_direct_paths$f__Enterobacteriaceae_g__3M_ln),
    logistic_reg(bdd_direct_paths$g__Bacteroides_3M_cat3),
    logistic_reg(bdd_direct_paths$g__Bifidobacterium_3M_ln), 
    logistic_reg(bdd_direct_paths$g__Veillonella_3M_ln),       
    logistic_reg(bdd_direct_paths$g__Streptococcus_3M_ln),
    logistic_reg(bdd_direct_paths$g__Lactobacillus_3M_cat),
    logistic_reg(bdd_direct_paths$g__Enterococcus_3M_cat),
    logistic_reg(bdd_direct_paths$Cdiff_num_3M_ln),
    logistic_reg(bdd_direct_paths$metabolite_Acetate_ln),
    logistic_reg(bdd_direct_paths$metabolite_Propionate_ln),
    logistic_reg(bdd_direct_paths$metabolite_Butyrate_ln),
    logistic_reg(bdd_direct_paths$metabolite_Tryptophan_ln),
    logistic_reg(bdd_direct_paths$metabolite_Lactate_ln),
    logistic_reg(bdd_direct_paths$metabolite_Formate_ln),
    logistic_reg(bdd_direct_paths$metabolite_Tyrosine_ln),
    logistic_reg(bdd_direct_paths$metabolite_Valerate_ln)))

path_a_logistic

## version régression linéaire (table S2) ----
linear_reg <- function(explanatory_var) {
  results <- 
    tbl_regression(
      lm(sIgA_3m_ln ~ explanatory_var, 
         data = bdd_direct_paths), 
      estimate_fun = scales::label_number(accuracy = .01, decimal.mark = "."),
      pvalue_fun = custom_pvalue_fun) %>%
    bold_labels() %>%
    bold_p(t=0.1) %>%
    add_n(location = "level") %>%
    add_global_p(keep = TRUE) %>%
    add_glance_table(include = r.squared)
  
  return(results)
} 

path_a_linear <- tbl_stack(
  tbls = list(
    linear_reg(bdd_direct_paths$Clusters_Hein_3M), 
    linear_reg(bdd_direct_paths$p__Proteobacteria_3M_ln), 
    linear_reg(bdd_direct_paths$p__Firmicutes_3M_ln),
    linear_reg(bdd_direct_paths$p__Bacteroidetes_3M_cat3),
    linear_reg(bdd_direct_paths$p__Actinobacteria_3M_ln),
    linear_reg(bdd_direct_paths$f__Enterobacteriaceae_g__3M_ln),
    linear_reg(bdd_direct_paths$g__Bacteroides_3M_cat3),
    linear_reg(bdd_direct_paths$g__Bifidobacterium_3M_ln), 
    linear_reg(bdd_direct_paths$g__Veillonella_3M_ln),       
    linear_reg(bdd_direct_paths$g__Streptococcus_3M_ln),
    linear_reg(bdd_direct_paths$g__Lactobacillus_3M_cat),
    linear_reg(bdd_direct_paths$g__Enterococcus_3M_cat),
    linear_reg(bdd_direct_paths$Cdiff_num_3M_ln),
    linear_reg(bdd_direct_paths$metabolite_Acetate_ln),
    linear_reg(bdd_direct_paths$metabolite_Propionate_ln),
    linear_reg(bdd_direct_paths$metabolite_Butyrate_ln),
    linear_reg(bdd_direct_paths$metabolite_Tryptophan_ln),
    linear_reg(bdd_direct_paths$metabolite_Lactate_ln),
    linear_reg(bdd_direct_paths$metabolite_Formate_ln),
    linear_reg(bdd_direct_paths$metabolite_Tyrosine_ln),
    linear_reg(bdd_direct_paths$metabolite_Valerate_ln)))


# direct path B : sIgA --> neuro (table S3) ----
path_b <- tbl_merge(
  tbls = list(
    tbl_regression(lm(BAYLEYQ4_3_12 ~ sIgA_3m_ln, data = bdd_filtered), 
                   estimate_fun = scales::label_number(accuracy = .01, decimal.mark = "."),
                   pvalue_fun = custom_pvalue_fun) %>%
      bold_labels() %>%
      bold_p(t=0.1) %>%
      add_n() %>%
      add_glance_table(include = r.squared),
    tbl_regression(lm(BAYLEYQ4_3_24 ~ sIgA_3m_ln, data = bdd_filtered), 
                   estimate_fun = scales::label_number(accuracy = .01, decimal.mark = "."),
                   pvalue_fun = custom_pvalue_fun) %>%
      bold_labels() %>%
      bold_p(t=0.1) %>%
      add_n()%>%
      add_glance_table(include = r.squared)),
  c("**Bayley cognitive score at 1 year**", "**Bayley cognitive score at 2 years**"))
path_b

# direct path C : GM 3M --> neuro (table 2) ----
prep_path_c <- list()

# Boucle sur les outcomes
for (outcome in outcomes) {
  # Initialisation de la liste pour cet outcome
  liste_tbl_regressions <- list()
  
  # Boucle sur les variables explicatives
  for (explanatory_var in explanatory_vars) {
    # Construction du modèle
    formule <- as.formula(paste(outcome, "~", explanatory_var, "+ Sex + GA + Birthmode_2cat + BFStatus3m"))
    modele <- lm(formule, data = bdd_direct_paths)
    
    # Ajout du résultat à la liste pour cet outcome
    liste_tbl_regressions[[explanatory_var]] <- 
      tbl_regression(modele, 
                     include = explanatory_var, 
                     estimate_fun = scales::label_number(accuracy = .01, decimal.mark = "."),
                     pvalue_fun = custom_pvalue_fun) %>%
      add_n(location = "level") %>%
      add_global_p(keep = FALSE) %>%
      bold_labels() %>%
      bold_p() %>% 
      add_glance_table(include = r.squared)
  }
  
  prep_path_c[[outcome]] <- liste_tbl_regressions
}


path_c <- vector("list", length = 35)  
for(i in 1:35) {
  path_c[[i]] <- tbl_merge(
    tbls = list(prep_path_c$BAYLEYQ4_3_12[[i]], prep_path_c$BAYLEYQ4_3_24[[i]]),
    tab_spanner = c("**Bayley cognitive score at 1 year**", "**Bayley cognitive score at 2 years**")) %>%
    modify_table_body(~.x %>% arrange(row_type == "glance_statistic"))
}

path_c <- tbl_stack(path_c)  
rm(liste_tbl_regressions, prep_path_c)
path_c


# Effects des covariables (table S4)----
effet_covar <- tbl_merge(
  tbls = list(
    tbl_regression(
      lm(sIgA_3m_ln ~ Sex + GA + Birthmode_2cat + BFStatus3m, data = bdd_direct_paths), 
      estimate_fun = scales::label_number(accuracy = .01, decimal.mark = "."),
      pvalue_fun = custom_pvalue_fun) %>%
      add_global_p() %>%
      add_n() %>%
      bold_labels() %>%
      bold_p(), 
    tbl_regression(
      lm(BAYLEYQ4_3_12 ~ Sex + GA + Birthmode_2cat + BFStatus3m, data = bdd_direct_paths), 
      estimate_fun = scales::label_number(accuracy = .01, decimal.mark = "."),
      pvalue_fun = custom_pvalue_fun) %>%
      add_global_p() %>%
      add_n() %>%
      bold_labels() %>%
      bold_p(), 
    tbl_regression(
      lm(BAYLEYQ4_3_24 ~ Sex + GA + Birthmode_2cat + BFStatus3m, data = bdd_direct_paths), 
      estimate_fun = scales::label_number(accuracy = .01, decimal.mark = "."),
      pvalue_fun = custom_pvalue_fun) %>%
      add_global_p() %>%
      add_n() %>%
      bold_labels() %>%
      bold_p()), 
  tab_spanner = c("**SIgA at 3-4 months (mg/g feces)**", 
                  "**Bayley cognitive score at 12 months**", 
                  "**Bayley cognitive score at 24 months**"))
