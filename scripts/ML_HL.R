
rm(list = ls())

library(bibliometrix)
library(ggplot2)
library(data.table)
library(tidyverse)
library(readxl)
library("multiwayvcov")
library("lfe")
library(stargazer)
library(lmtest)
library(recipes)
library(sandwich)
library(parallel)
library(doParallel)
library(caret)
library("lfe")
library(text)
library(textTinyR)
library(tm)

hlph <- read_excel("Bibliometrix-Export-File-2023-05-31.xlsx")


# Read data in ------------------------------------------------------------

#---Modifica diret?rio

# Duvida : onde foi obtido o arquivo WOS ?
#D1 <- "./savedrecs_fim.bib"

# Convert to data.frame ---------------------------------------------------

#M1 <- convert2df(D1, dbsource = "isi", format = "bibtex")
M1 = hlph
# Convert to data.frame ---------------------------------------------------

#M1 <- convert2df(D1, dbsource = "isi", format = "bibtex")
#M2 <- convert2df(D1, dbsource = "scopus", format = "bibtex")


# Merge -------------------------------------------------------------------

#Z <- mergeDbSources(M1, M2, remove.duplicated=TRUE)

# Remove uncorrelated papers ----------------------------------------------

# uncorrelated_papers = fread("./data/uncorrelated_papers_list.csv")

#identify by authors and paper's title
# M =
#   Z %>%
#     anti_join(uncorrelated_papers %>%
#                 select(AU, TI))


M = M1






# Do processing -----------------------------------------------------------
#Optamos por ID por ter menos NA
sum(is.na(M$DE))
sum(is.na(M$ID))

M1 <- subset(M, !is.na(ID))


keywords = as.data.table(M1$ID)

# Bibliometric analysis ---------------------------------------------------

results <- biblioAnalysis(M1, sep = ";")

# Do processing -----------------------------------------------------------


data =
  keywords %>%
  mutate(id = row_number()) %>%
  # separate(V1, c("_1"),
  #          sep = ";")
  # separate(V1, c("_1", "_2", "_3", "_4", "_5", "_6"),
  #          sep = ";")
  separate(V1, c("_1", "_2", "_3", "_4", "_5",
                 "_6", "_7", "_8", "_9", "_10", "_11",
                 "_12", "_13", "_14", "_15", "_16", "_17",
                 "_18", "_19"),
           sep = ";")


tidied = data %>%
  pivot_longer(-id) %>%
  mutate(value = toupper(trimws(value, which = "both"))) %>%
  mutate(value = case_when(
    
    value %in% c("COVID-19 PANDEMIC", "CORONAVIRUS INFECTION", "CORONAVIRUS", "CORONAVIRUS INFECTIONS", "CORONAVIRUS DISEASE 2019", "SEVERE ACUTE RESPIRATORY SYNDROME CORONAVIRUS 2", "COVID19", "COVID-19 EFFECT", "SARS-COV-2") ~ "COVID-19",
    value %in% c("MEN", "MAN", "MALES") ~ "MALE",
    value %in% c("PANDEMIC", "PANDEMICS", "EPIDEMIC") ~ "PANDEMIC",
    value %in% c("WOMEN", "WOMAN", "FEMALES") ~ "FEMALE",
    value %in% c("HUMAN", "HUMANS") ~ "HUMAN",
    value %in% c("CHILDREN") ~ "CHILD",
    value %in% c("ADOLESCENTS", "TEEN", "TEENS", "TEENAGER", "TEENAGERS", "ADOLESCENCE") ~ "ADOLESCENT",
    value %in% c("ADULTS") ~ "ADULT",
    
    
    
    value %in% c("ACCESS", "ACCESS TO", "ACCESS TO CARE", "ACCESS TO DENTAL CARE", "ACCESS TO HEALTH CARE", "ACCESS TO HEALTH INFORMATION", "ACCESS TO HEALTH SERVICES", "ACCESS TO HEALTHCARE", "ACCESS TO INFORMATION", "ACCESS TO NURSING CARE") ~ "ACCESS",
    value %in% c("ADHERENCE", "ADHERENCE BEHAVIOR", "ADHERENCE TO TREATMENT", "THERAPEUTIC ADHERENCE") ~ "ADHERENCE",
    value %in% c("ADOLESCENT", "ADOLESCENT AND CHILDREN", "ADOLESCENT FRIENDLY HEALTH SERVICES", "ADOLESCENT HEALTH", "ADOLESCENT HEALTH LITERACY", "ADOLESCENT NUTRITION", "ADOLESCENT OBESITY", "ADOLESCENT PREGNANCY", "ADOLESCENT VOICE", "ADOLESCENTS AND YOUTH") ~ "ADOLESCENT",
    value %in% c("ALCOHOLISM", "ALCOHOL USE DISORDER", "ALCOHOL BEHAVIOR MODIFICATION PROGRAM", "ALCOHOL ADDICTION", "ALCOHOL ABUSE", "ALCOHOL", "ALCOHOL BEHAVIOR MODIFICATION PROGRAM") ~ "ALCOHOL ABUSE",
    value %in% c("ALZHEIMER DISEASE", "ALZHEIMER'S DISEASE", "ALZHEIMER'S") ~ "ALZHEIMER'S",
    value %in% c("AMERICAN INDIAN", "AMERICAN INDIAN HEALTH", "AMERICAN INDIANS") ~ "AMERICAN INDIAN",
  
    
    value %in% c("ANTI-STIGMA", "ANTI-STIGMA CAMPAIGN", "ANTI-STIGMA INTERVENTION") ~ "ANTI-STIGMA",
    value %in% c("ANTIBIOTIC", "ANTIBIOTIC INTERACTION", "ANTIBIOTIC RESISTANCE", "ANTIBIOTIC STEWARDSHIP", "ANTIBIOTIC SUSCEPTIBILITY PATTERNS", "ANTIBIOTICS") ~ "ANTIBIOTICS",
    value %in% c("ANTI-VACCINE MOVEMENT", "ANTIVACCINE", "VACCINE HESITANCY", "", "", "", "", "", "", "") ~ "ANTIVACINE",
    value %in% c("ANXIETY", "ANXIETY DISORDERS", "ANXIETY LITERACY", "ANXIETY SYMPTOM") ~ "MENTAL HEALTH",
    value %in% c("ASIAN-AMERICAN", "ASIAN AMERICAN", "ASIAN AMERICANS", "", "", "", "", "", "", "") ~ "ASIAN-AMERICAN",
    value %in% c("ASTHMA", "ASTHMA ACTION PLAN", "ASTHMA IN CHILDREN", "ASTHMA SELF-MANAGEMENT") ~ "ASTHMA",
    value %in% c("ATTITUDE", "ATTITUDE AND PRACTICE", "ATTITUDE OF HEALTH PERSONNEL", "ATTITUDE TO", "ATTITUDE TOWARD", "ATTITUDE TOWARD SUN EXPOSURE", "ATTITUDES", "ATTITUDES AND BELIEFS", "ATTITUDES AND PRACTICES", "ATTITUDES OF GENERAL PUBLIC") ~ "ATTITUDE",
    value %in% c("", "", "", "", "", "", "", "", "", "") ~ "",
    value %in% c("CHILD", "CHILD-CENTRED", "CHILD CARE", "CHILD CENTRED", "CHILD DAYCARE CENTRES", "CHILD DEVELOPMENT", "CHILD FEEDING GUIDELINES", "CHILD HEALTH", "CHILD HEALTH CLINIC", "CHILD HEALTH SERVICES", "CHILD RESTRAINTS", "CHILD STUNTING", "CHILD THERAPY", "CHILD WELL-BEING", "CHILDCARE PROVIDERS", "CHILDHOOD", "CHILDHOOD ASTHMA", "CHILDHOOD DISEASES", "CHILDREN'S", "CHILDREN'S ENVIRONMENTAL HEALTH", "CHILDREN'S HEALTH", "CHILDREN'S HEALTH LITERACY", "CHILDREN 7-12 YEARS", "CHILDREN AGED 5 YEARS","CHILDREN AGES 9-10 YEARS OLD", "CHILDREN’S HEALTH", "CHILDREN’S LITERATURE") ~ "CHILD",
    value %in% c("WORRY", "WORRIES") ~ "WORRY",
    value %in% c("TB") ~ "TUBERCULOSIS",
    value %in% c("AIDS") ~ "HIV",
    
    value %in% c("", "", "", "", "", "", "", "", "", "") ~ "",
    value %in% c("", "", "", "", "", "", "", "", "", "") ~ "",
    value %in% c("", "", "", "", "", "", "", "", "", "") ~ "",
    value %in% c("", "", "", "", "", "", "", "", "", "") ~ "",
    value %in% c("", "", "", "", "", "", "", "", "", "") ~ "",
    value %in% c("", "", "", "", "", "", "", "", "", "") ~ "",
    value %in% c("", "", "", "", "", "", "", "", "", "") ~ "",
    
    
    
    value %in% c("AFRICAN-AMERICAN", "AFRICAN-AMERICANS", "AFRICAN–AMERICAN CHURCHES", "AFRICAN AMERICAN", "AFRICAN AMERICAN OR BLACK WOMEN", "AFRICAN AMERICAN WOMEN", "AFRICAN AMERICANS") ~ "AFRICAN-AMERICAN",
    
    
    str_detect(value, "BEHAVIO*") ~ "BEHAVIOR",
    str_detect(value, "BELIEF*") ~ "BELIEF",
    str_detect(value, "BIBLIOMETRI*") ~ "BIBLIOMETRIC",
    str_detect(value, "BREAST CANCER") ~ "BREAST CANCER",
    str_detect(value, "BREASTFEEDING") ~ "BREASTFEEDING",
    str_detect(value, "CANCER") ~ "CANCER",
    str_detect(value, "HABITS") ~ "HABITS",
    str_detect(value, "VACCINE") ~ "VACCINE",
    str_detect(value, "VACCINATION") ~ "VACCINE",
    str_detect(value, "CHRONIC") ~ "CHRONIC",
    str_detect(value, "MENTAL HEALTH") ~ "MENTAL HEALTH",
    str_detect(value, "MENTAL ILLNESS") ~ "MENTAL HEALTH",
    str_detect(value, "HIV") ~ "HIV",
    str_detect(value, "ZIKA VIRUS") ~ "ZIKA-VIRUS",
    str_detect(value, "ZIKA") ~ "ZIKA-VIRUS",
    str_detect(value, "WORK") ~ "WORK",
    str_detect(value, "VULNERABLE") ~ "VULNERABILITY",
    str_detect(value, "VULNERABILITY") ~ "VULNERABILITY",
    str_detect(value, "URBAN") ~ "URBAN",
    str_detect(value, "UNIVERSITY") ~ "UNIVERSITY",
    str_detect(value, "UNIVERSITARY") ~ "UNIVERSITY",
    str_detect(value, "UNCERTAINTY") ~ "UNCERTAINTY",
    str_detect(value, "DIABETES") ~ "DIABETES",
    str_detect(value, "TUBERCULOSIS") ~ "TUBERCULOSIS",
    str_detect(value, "TOBACCO") ~ "TOBACCO",
    str_detect(value, "TOOTH") ~ "ORAL HEALTH",
    str_detect(value, "TEETH") ~ "ORAL HEALTH",
    str_detect(value, "ORAL") ~ "ORAL HEALTH",

    str_detect(value, "DEPRESSION") ~ "MENTAL HEALTH",
    str_detect(value, "ANXIETY") ~ "MENTAL HEALTH",
    str_detect(value, "SUICIDE") ~ "MENTAL HEALTH",
    str_detect(value, "ADDICT*") ~ "ADDICTION",
    
    str_detect(value, "ABORIGI*") ~ "ABORIGINAL",
    str_detect(value, "AGEING") ~ "AGING",
    str_detect(value, "AGING") ~ "AGING",
    str_detect(value, "AWARENESS") ~ "AWARENESS",
    str_detect(value, "OBESITY") ~ "OBESITY",
    str_detect(value, "NUTRITION") ~ "NUTRITION",
    str_detect(value, "WOM*") ~ "FEMALE",
    str_detect(value, "VITAMIN") ~ "VITAMIN",
    str_detect(value, "ADHERENCE") ~ "ADHERENCE",
    str_detect(value, "HEALTH LITERACY") ~ "HEALTH LITERACY",
    str_detect(value, "STROKE") ~ "STROKE",
    str_detect(value, "SEXUALLY TRANSMITTED") ~ "STD",
    str_detect(value, "STD") ~ "STD",
    str_detect(value, "SEXUALLY TRANSMITTED") ~ "STD",

    str_detect(value, "HEALTH EDUCATION") ~ "HEALTH EDUCATION",
    str_detect(value, "PUBLIC HEALTH") ~ "PUBLIC HEALTH",
    str_detect(value, "HEARING") ~ "HEARING",
    str_detect(value, "HEART") ~ "CARDIAC",
    str_detect(value, "CARDIAC") ~ "CARDIAC",
    
    str_detect(value, "GENDER") ~ "GENDER",
    
    
    str_detect(value, "FOOD") ~ "FOOD",
    str_detect(value, "ETHNIC") ~ "ETHNIC GROUPS",
    str_detect(value, "EPIDEMI*") ~ "EPIDEMIOLOGY",
    str_detect(value, "DIET") ~ "NUTRITION",
    str_detect(value, "DENTAL") ~ "ORAL HEALTH",
    str_detect(value, "DEMENTIA") ~ "DEMENTIA",
    str_detect(value, "DECISION") ~ "DECISION MAKING",
    str_detect(value, "COVID*") ~ "COVID-19",
    str_detect(value, "CARDIOVASCULAR") ~ "CARDIAC",
    str_detect(value, "CANCER") ~ "CANCER",
    str_detect(value, "CARE-GIV*") ~ "CARE-GIVER",
    str_detect(value, "DEATH") ~ "DEATH",
    str_detect(value, "DIAGNO*") ~ "DIAGNOSIS",
    str_detect(value, "PREVENT*") ~ "PREVENTION",
    str_detect(value, "ELDER*") ~ "AGED",
    
    str_detect(value, "HEALTH-CARE") ~ "HEALTHCARE",
    str_detect(value, "HEALTH CARE") ~ "HEALTHCARE",
    str_detect(value, "HEALTH LITERACY") ~ "HEALTH LITERACY",
    str_detect(value, "HEALTH-LITERACY") ~ "HEALTH LITERACY",
    
    str_detect(value, "HEALTHCARE") ~ "HEALTHCARE",
    str_detect(value, "INTERVENTION") ~ "INTERVENTION",
    str_detect(value, "RISK FACTORS") ~ "RISK FACTOR",
    str_detect(value, "RISK-FACTORS") ~ "RISK FACTOR",
    str_detect(value, "RISK-FACTORS") ~ "RISK FACTOR",
    str_detect(value, "RISK FACTORS") ~ "RISK FACTOR",
    str_detect(value, "RISK PERCEPTIONS") ~ "RISK PERCEPTION",
    str_detect(value, "RISK ASSESSMENT") ~ "RISK ASSESSMENT",
    str_detect(value, "RISK-ASSESSMENT") ~ "RISK ASSESSMENT",
    str_detect(value, "80 AND OVER") ~ "AGED",
    str_detect(value, "HIV") ~ "HIV",
    str_detect(value, "HIV") ~ "HIV",
    
    
    
    TRUE ~ value
    
      ))

tidied$value = gsub("ANTIVACINE", "ANTI-VACCINE", tidied$value)
tidied$value = gsub("-", " ", tidied$value)

contar = tidied %>%
  count(value) 
 
library(xlsx)

write.xlsx(contar, "contar.xlsx")

final =
  tidied %>%
  #filter(!is.null(value) & !is.na(value)) %>%
  pivot_wider(id_cols = id) %>%
  unite("keywords", -id, sep = ";") %>%
  mutate(keywords = str_replace_all(string = keywords, pattern = ";NA", replacement = "")) %>%
  mutate(keywords = ifelse(keywords == "NA", NA, keywords))


# M$ID = final$keywords

# save(M, file = "benjamin_keywords_corrigido.RData")



# Regression --------------------------------------------------------------

panel_format =
  tidied %>%
  #filter(!is.na(value)) %>%
  select(-name) %>%
  mutate(value = ifelse(is.na(value) | value == "", "NO_KEYWORD", value)) %>%
  mutate(frequency = 1) %>%
  pivot_wider(id_cols = id,
              names_from = value,
              values_from = frequency,
              values_fn = max,
              values_fill = 0) %>%
  janitor::clean_names() #%>%
  #select(-market, -model, -exposures, -markets)

panel = data.table(
  total_citations = results$TotalCitation,
  citation_per_year = results$TCperYear,
  qty_authors  = results$nAUperPaper,
  year_publication = results$Years,
  country = results$CO,
  panel_format
) %>%
  mutate(total_citations = coalesce(total_citations, 0),
         citation_per_year = coalesce(citation_per_year, 0.0)) %>%
  mutate(is_single_author = qty_authors == 1) %>%
  mutate(country = coalesce(country, "not informed"))




# Data splitting
index = createDataPartition(panel$country,
                            p = 0.8,
                            list = FALSE)

train_data = panel[index, ]
test_data  = panel[-index, ]



# for speeding up the process
ex_ante_recipe =
  recipes::recipe(~ ., data = train_data %>% as.data.table()) %>%
  recipes::step_rm(total_citations, id) %>%
  # recipes::step_impute_mode(country) %>%
  #recipes::step_dummy(country, one_hot = TRUE, keep_original_cols = FALSE) %>%
  recipes::step_nzv(recipes::all_predictors(), -country,-citation_per_year, freq_cut=98/2) %>% #, -starts_with("country")) %>%
  recipes::step_corr(recipes::all_numeric(), threshold = .99) %>%
  recipes::step_impute_knn(all_predictors())

prepared_recipe = recipes::prep(ex_ante_recipe)

processed_train_data = recipes::juice(prepared_recipe)
processed_test_data  = recipes::bake(prepared_recipe, test_data)

# Calculate the number of cores
no_cores <- detectCores() - 1

# create the cluster for caret to use
cl <- makePSOCKcluster(no_cores); registerDoParallel(cl);


# train the model
model_random_forests <- caret::train(
  #citation_per_year ~ .,
  x = processed_train_data %>% select(-citation_per_year, -country),
  y = log(1+processed_train_data$citation_per_year),
  data = processed_train_data %>% as.data.table(),
  method = "rf", #"ctree", #"rpart",  #"xgbTree",# 'glmnet',
  tuneLength = 30,
  metric = "RMSE",
  trControl= trainControl(
    allowParallel = T,
    number = 5,
    repeats = 5,
    method = "repeatedcv", #"cv",
    verbose = F,
  )

)

model_random_forests

# number of trees
model_random_forests$finalModel$ntree

ggplot(model_random_forests) +
  geom_line(size = 1.3) +
  geom_point(size = 5) +
  geom_vline(xintercept = model_random_forests$bestTune$mtry, linetype = "dashed", alpha = 0.5, size = 1) +
  theme_bw(base_size = 22) +
  theme(aspect.ratio = 1)

ggsave(
       filename = "cross_validation_result.pdf",
       device = "pdf",
       width = 9,
       height = 7)

fitted_variables = predict(model_random_forests,
                           newdata = processed_test_data %>% select(-citation_per_year))

caret::RMSE(fitted_variables, processed_test_data$citation_per_year)
caret::R2(fitted_variables, processed_test_data$citation_per_year)

# estimate variable importance
importance <- varImp(model_random_forests, scale=TRUE)
# summarize importance
print(importance)

importanceToPlot =
  data.table(original_label = rownames(importance$importance),
             importance = importance$importance$Overall) %>%
  filter(!(original_label %in% c("no_keyword", "country_not.informed"))) %>%
  arrange(-importance) %>%
  mutate(rank = row_number()) %>%
  mutate(original_label = str_replace_all(original_label, "^country_", "")) %>%
  mutate(formatted_label = factor(original_label, ordered = TRUE, levels = rev(.$original_label[rank])))


importanceToPlot %>%
  filter(rank <= 25) %>%
  ggplot(aes(y=formatted_label, x=importance)) +
  geom_vline(xintercept=0,
             size=1, alpha = 0.6, color = "black", linetype = "dashed") +
  geom_segment( aes(yend=formatted_label, x=0, xend=importance)) +
  geom_point( size=4, color="red", fill=ggplot2::alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
  guides(color = guide_legend(title=""),
         fill = guide_legend(title = "")) +
  labs(x = "Avg. Importance (%)",
       y = ""
       #title = "Variable importance (%)"
  ) +
  theme_bw(base_size = 22) +
  theme(panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.position="bottom")

ggsave(
       filename = "feature_importance_citation_per_year.pdf",
       device = "pdf",
       width = 10,
       height = 7)

# Unsupervised learning ---------------------------------------------------

panel = data.table(
  total_citations = results$TotalCitation,
  citation_per_year = results$TCperYear,
  qty_authors  = results$nAUperPaper,
  year_publication = results$Years,
  country = results$CO,
  panel_format
) %>%
  mutate(is_single_author = qty_authors == 1) %>%
  as.data.frame()

# for speeding up the process
ex_ante_recipe =
  recipes::recipe(~ ., data = panel %>% as.data.table()) %>%
  recipes::step_rm(total_citations, id) %>%
  # recipes::step_impute_mode(country) %>%
  recipes::step_dummy(country, one_hot = TRUE, keep_original_cols = FALSE) %>%
  recipes::step_nzv(recipes::all_predictors(), -citation_per_year, -starts_with("country")) %>%
  recipes::step_corr(recipes::all_numeric(), threshold = .99) %>%
  recipes::step_impute_knn(recipes::all_predictors())

prepared_recipe = recipes::prep(ex_ante_recipe)

processed_data = recipes::juice(prepared_recipe)


panel %>%
  mutate(paper_age = 2023 - year_publication) %>%
  mutate(citation_per_year = coalesce(citation_per_year, 0)) %>%
  mutate(mean_age = mean(paper_age)) %>%
  mutate(paper_age = coalesce(paper_age, mean_age)) %>%
  filter(is.na(paper_age))

strings = paste0(importanceToPlot$formatted_label, sep = " +", collapse = " ")
       
fileConn<-file("formula.txt")
writeLines(strings, fileConn)
close(fileConn)
panel_zscore <- panel %>%
  mutate_if(is.numeric, scale)

panel_model = panel %>%
  mutate(paper_age = 2023 - year_publication) %>%
  mutate(publicado_na_covid = year_publication >= 2020) %>%
  mutate(citation_per_year = coalesce(total_citations, 0)) %>%
  mutate(mean_age = mean(paper_age, na.rm = T)) %>%
  mutate(paper_age = coalesce(paper_age, mean_age)) %>%
  mutate(country = coalesce(country, "No country")) %>%
  mutate(id = row_number())


check = data = panel %>%
  mutate(paper_age = 2023 - year_publication) %>%
  mutate(publicado_na_covid = year_publication >= 2020) %>%
  mutate(citation_per_year = coalesce(total_citations, 0)) %>%
  mutate(mean_age = mean(paper_age, na.rm = T)) %>%
  mutate(paper_age = coalesce(paper_age, mean_age)) %>%
  mutate(country = coalesce(country, "No country")) %>%
  mutate(id = row_number())

panel$paper_age = 2023 - panel$year_publication

model1 = felm(
  data = panel %>%
    mutate(publicado_na_covid = year_publication >= 2020) %>%
    mutate(citation_per_year = coalesce(total_citations, 0)) %>%
    mutate(mean_age = mean(paper_age, na.rm = T)) %>%
    mutate(paper_age = coalesce(paper_age, mean_age)) %>%
    mutate(country = coalesce(country, "No country")) %>%
    mutate(id = row_number()),
  formula = citation_per_year ~ paper_age + qty_authors + is_single_author  + human + article + health_knowledge_attitudes_practice + anti_vaccine + cross_sectional_study + female + health_literacy + questionnaire + healthcare + male + surveys_and_questionnaires + health_promotion + behavior + adult + covid_19 + public_health + controlled_study + mental_health + education + aged
  | 0 | 0 | 0
)


stargazer(model1, type = "text", out = "reg_idade.tex",  add.lines = list(
            c("Fixed Effect", "ID of Article")))


model2 = felm(
  data = panel %>%
    mutate(paper_age = 2023 - year_publication) %>%
    mutate(publicado_na_covid = year_publication >= 2020) %>%
    mutate(total_citations = coalesce(total_citations, 0)) %>%
    mutate(mean_age = mean(paper_age, na.rm = T)) %>%
    mutate(paper_age = coalesce(paper_age, mean_age)) %>%
    mutate(country = coalesce(country, "No country")) %>%
    mutate(id = row_number()),
  formula = citation_per_year ~ paper_age + qty_authors + is_single_author  + human + article + health_knowledge_attitudes_practice + anti_vaccine + cross_sectional_study + female + health_literacy + questionnaire + healthcare + male + surveys_and_questionnaires + health_promotion + behavior + adult + covid_19 + public_health + controlled_study + mental_health + education + aged
  | paper_age | 0 | 0
)

model3 = felm(
  data = panel %>%
    mutate(paper_age = 2023 - year_publication) %>%
    mutate(publicado_na_covid = year_publication >= 2020) %>%
    mutate(total_citations = coalesce(total_citations, 0)) %>%
    mutate(mean_age = mean(paper_age, na.rm = T)) %>%
    mutate(paper_age = coalesce(paper_age, mean_age)) %>%
    mutate(country = coalesce(country, "No country")) %>%
    mutate(id = row_number()),
  formula = citation_per_year ~ paper_age + qty_authors + is_single_author  + human + article + health_knowledge_attitudes_practice + anti_vaccine + cross_sectional_study + female + health_literacy + questionnaire + healthcare + male + surveys_and_questionnaires + health_promotion + behavior + adult + covid_19 + public_health + controlled_study + mental_health + education + aged
   | 0 | 0
)


model1b = felm(
  data = panel %>%
    mutate(paper_age = 2023 - year_publication) %>%
    mutate(publicado_na_covid = year_publication >= 2020) %>%
    mutate(citation_per_year = coalesce(citation_per_year, 0)) %>%
    mutate(mean_age = mean(paper_age, na.rm = T)) %>%
    mutate(paper_age = coalesce(paper_age, mean_age)) %>%
    mutate(country = coalesce(country, "No country")) %>%
    mutate(id = row_number()),
  formula = citation_per_year ~ paper_age + qty_authors + is_single_author  + human + article + health_knowledge_attitudes_practice + anti_vaccine + cross_sectional_study + female + health_literacy + questionnaire + healthcare + male + surveys_and_questionnaires + health_promotion + behavior + adult + covid_19 + public_health + controlled_study + mental_health + education + aged
  | 0 | 0 | id
)


model2b = felm(
  data = panel %>%
    mutate(paper_age = 2023 - year_publication) %>%
    mutate(publicado_na_covid = year_publication >= 2020) %>%
    mutate(total_citations = coalesce(total_citations, 0)) %>%
    mutate(mean_age = mean(paper_age, na.rm = T)) %>%
    mutate(paper_age = coalesce(paper_age, mean_age)) %>%
    mutate(country = coalesce(country, "No country")) %>%
    mutate(id = row_number()),
  formula = citation_per_year ~ paper_age + qty_authors + is_single_author  + human + article + health_knowledge_attitudes_practice + anti_vaccine + cross_sectional_study + female + health_literacy + questionnaire + healthcare + male + surveys_and_questionnaires + health_promotion + behavior + adult + covid_19 + public_health + controlled_study + mental_health + education + aged
  | paper_age | 0 | id
)



model3b = felm(
  data = panel %>%
    mutate(paper_age = 2023 - year_publication) %>%
    mutate(publicado_na_covid = year_publication >= 2020) %>%
    mutate(total_citations = coalesce(total_citations, 0)) %>%
    mutate(mean_age = mean(paper_age, na.rm = T)) %>%
    mutate(paper_age = coalesce(paper_age, mean_age)) %>%
    mutate(country = coalesce(country, "No country")) %>%
    mutate(id = row_number()),
  formula = citation_per_year ~ paper_age + qty_authors + is_single_author  + human + article + health_knowledge_attitudes_practice + anti_vaccine + cross_sectional_study + female + health_literacy + questionnaire + healthcare + male + surveys_and_questionnaires + health_promotion + behavior + adult + covid_19 + public_health + controlled_study + mental_health + education + aged
  | factor(paper_age):factor(country) | 0 | id
)


stargazer(model1, model2, type = "text")
stargazer(model1b, model2b,  type = "latex", out = "regs_keywords_clusterid.tex",
          add.lines = list(
            c("Fixed Effect", "None", "Paper Age"), c("Clustered S.E.", "Paper ID","Paper ID")))
stargazer(model1, model2, model3, model1b,model2b, model3b, type = "text")
stargazer(model1, model2, type = "html", out = "regression.html")
stargazer(model1, model2, type = "latex", out = "regression.tex")


model = felm(
  data = panel %>%
    mutate(paper_age = 2023 - year_publication) %>%
    mutate(id = row_number()),
  formula = citation_per_year ~ country
  | 1 | 0 | id
)


stargazer(model, type = "text")
stargazer(model, type = "html", out = "INST_regression_country.html")
stargazer(model, type = "latex", out = "regression.tex")


