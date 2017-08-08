library(tidyr)
library(dplyr)
##Clean up brand names
refine_original <- refine_original %>%
  mutate(company = ifelse(grepl("^phil|^fil|^phl", company, ignore.case = TRUE), "phillips", company)) %>%
  mutate(company = ifelse(grepl("^ak", company, ignore.case = TRUE), "akzo", company)) %>%
  mutate(company = ifelse(grepl("^van", company, ignore.case = TRUE), "van houten", company)) %>%
  mutate(company = ifelse(grepl("^uni", company, ignore.case = TRUE), "unilever", company))
#Separate product code and number
refine_original <- refine_original %>%
  separate('Product code / number', into = c("product_code", "product_number"), sep = "-")
#Add product categories
refine_original <- refine_original %>%
  mutate("product_category" = ifelse(product_code == "p", "Smartphone", " ")) %>%
  mutate("product_category" = ifelse(product_code == "v", "TV", product_category)) %>%
  mutate("product_category" = ifelse(product_code == "x", "Laptop", product_category)) %>%
  mutate("product_category" = ifelse(product_code == "q", "Tablet", product_category))
#Add full address for geocoding
refine_original <- refine_original %>%
  unite(full_address, c(address, city, country), sep = ",")
#Create dummy variables for company and product categories
refine_original <- refine_original %>%
  mutate(company_phillips = ifelse(company == "phillips", 1, 0))%>%
  mutate(company_akzo = ifelse(company == "akzo", 1, 0))%>%
  mutate(company_van_houten = ifelse(company == "van houten", 1, 0)) %>%
  mutate(company_unilever = ifelse(company == "unilever", 1, 0))

refine_original <- refine_original %>%
  mutate(product_smartphone = ifelse(product_category == "smartphone", 1, 0)) %>%
  mutate(product_tv = ifelse(product_category == "tv", 1, 0)) %>%
  mutate(product_laptop = ifelse(product_category == "laptop", 1, 0)) %>%
  mutate(product_tablet = ifelse(product_category == "tablet", 1, 0 ))

write.csv(refine_original, file = "refine_clean.csv")