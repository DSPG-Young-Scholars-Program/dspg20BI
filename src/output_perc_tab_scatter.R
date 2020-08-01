setwd("~/git/dspg20BI/")
ndc_list <- readr::read_delim("data/original/ndc_listings.txt", delim = "\t")

ndc_list %>% filter(str_detect(LABELERNAME, "7-Eleven"))

ndc_orig_ct <- ndc_list %>% count(year = stringr::str_extract(STARTMARKETINGDATE, "^\\d{4}"), 
                   ndc_comp = LABELERNAME)

ndc_orig_ct

################

legal_entities <- readr::read_csv("https://raw.githubusercontent.com/DSPG-Young-Scholars-Program/dspg20oss/danBranch/ossPy/keyFiles/curatedLegalEntitesRaw.csv",col_names = "entity", quote = "'" )
patt <- paste0(legal_entities$entity, collapse = "|")

top20_entities_remove <- c('pharmaceuticals', 'medical', 'products',
   'laboratories', 'pharma',
   'anda',
   'supply',
   'health',
   'pharmaceutical',
   'usa',
   'international',
   'care',
   'and',
   'nda',
   'coltd',
   'the',
   'home',
   'healthcare',
   # '',
   'of',
  'group',
  'holdings',
  'capital',
  'technologies',
  'association',
  'us',
  'services',
  'university',
  'bank',
  'partners',
  'energy',
  'systems',
  'intl',
  'pharms',
  'american',
  'national',
  'biosciences')

# patt2 <- paste0("\\b(", paste0(top20_entities_remove, collapse = "|"), ")\\b")
patt2 <-paste0("\\b", top20_entities_remove, "\\b", collapse = "|")
# patt2 <-paste0("\b", top20_entities_remove, "\b", collapse = "|")

###############

# potential_clean_names <- ndc_orig_ct %>% 
#   filter(year > 2012 & year <2018) %>% 
#   # tail(10) %>%
#   # filter(str_detect(ndc_comp, "\\s+[a-zA-Z]\\s+")) %>%
#   # filter(str_detect(ndc_comp, "^b\\s+"))
#   # filter(str_detect(ndc_comp, "\\s-\\s+")) %>%
#   # filter(str_detect(ndc_comp, "_")) %>%
#   mutate(corp_low = str_to_lower(ndc_comp), 
#          corp_parenth = str_remove(corp_low, pattern = "\\([^)]*\\)"), 
#          corp_punct = str_remove_all(corp_parenth, pattern = "[!\"#\'\\(\\)\\*\\+,\\.\\/\\:;%<=>\\?@\\[\\]\\^`\\{\\|\\}~]"),
#          corp_num = str_remove_all(corp_punct, pattern = "[0-9+]"),
#          corp_1char = str_remove_all(corp_num, pattern = "\\s+[a-zA-Z]\\s+"), 
#          corp_mult_sp = str_replace_all(corp_1char, pattern = "\\s+", replacement = " "), 
#          #corp_pref_b = str_remove(corp_1char, pattern = "^b\\s+")
#          corp_dashes = str_replace_all(corp_mult_sp, pattern = "\\s-\\s+", replacement = "-"),
#          corp_amps = str_replace_all(corp_dashes, pattern = "\\s&\\s+", replacement = "&"), 
#          corp_under = str_replace_all(corp_amps, pattern = "\\s_\\s+", replacement = "_"),
#          potential_fam = str_remove_all(corp_under, patt), 
#          potential_fam2 = str_remove_all(potential_fam, patt2),
#          # potential_fam2 = str_remove_all(potential_fam2, patt2),
#          potential_fam3 = str_squish(potential_fam2)
#          ) %>%
#   select(year, ndc_comp, n, potential_fam, potential_fam2, potential_fam3)

NDC_clean <- read.csv("data/working/ndc_clean.csv")

ndc_orig_ct_CORPFAM <- ndc_orig_ct %>% 
  filter(year > 2012 & year < 2018) %>%
  left_join(NDC_clean, by = c("ndc_comp" = "original_company"))


ndc_summary_table <- ndc_orig_ct_CORPFAM %>% 
  group_by(cleaned_name, year) %>% 
  summarise(listings =sum(n)) %>% 
  filter(cleaned_name != "") %>% 
  as.data.frame()

table(ndc_summary_table$cleaned_name)

########################

ndc_dna_matching <- read.csv("data/working/ndc_dna_matching.csv")
ndc_dna_matching %>% head()

ndc_potential_corpfam_listings <- ndc_dna_matching %>% #select(clean.NDC.company, corporate.family, original.NDC.company) %>% head()
  # select(corporate.family) %>%
  left_join(ndc_summary_table , by = c("corporate.family" = "cleaned_name")) %>%  # 1756 rows
  filter(!is.na(year))

ndc_potential_corpfam_listings$num_ndc_listings <- ndc_potential_corpfam_listings$n
ndc_potential_corpfam_listings$n <- NULL

ndc_potential_corpfam_listings %>% head()
# ##CHECKS
# potential_clean_names %>% filter(str_detect(str_to_lower(ndc_comp), "verio"))
# ndc_dna_matching %>% filter(str_detect(corporate.family, "verio"))
# ndc_orig_ct %>% filter(str_detect(ndc_comp, "Verio"))

########################

ndc_yr_totals <- ndc_orig_ct %>% 
  group_by(year) %>%
  summarise(total_listings = sum(n)) %>% 
  filter(year > 2012 & year < 2018) %>% 
  as.data.frame()

ndc_potential_corpfam_listings <- ndc_potential_corpfam_listings %>% 
  left_join(ndc_yr_totals, by = c("year")) %>% 
  mutate(perc_ndc = listings/total_listings,
         perc_ndc_ = scales::percent(listings/total_listings))

hist(ndc_potential_corpfam_listings$listings/ndc_potential_corpfam_listings$total_listings)
ndc_potential_corpfam_listings %>% arrange(desc(perc_ndc)) %>% head(20) %>% select(corporate.family, year, perc_ndc)
ndc_potential_corpfam_listings %>% arrange(desc(perc_ndc)) %>% head()


########################

# DNA_comp_freq <- read.csv("data/working/companyfrequencyandname.csv")
# DNA_comp_freq %>% head()
# 
DNA_clean <- read.csv("data/working/dna_clean.csv")
# DNA_clean %>% head()
# 


DNA_2013 <- read.csv("data/working/DNA_2013.csv")

DNA_total_articles_2013 <- nrow(DNA_2013)
DNA_comp_freq_2013 <- DNA_2013 %>% 
  count(dna_comp = strsplit(company_codes_about, ","), 
        year = lubridate::year(as.POSIXct(publication_date/1000, origin="1970-01-01"))) %>% 
  tidyr::unnest(cols = "dna_comp") %>% 
  filter(nchar(dna_comp)>1 ) %>% 
  mutate(dna_comp = str_to_upper(dna_comp)) %>% 
  left_join(DNA_clean, by = c("dna_comp" = "Code")) %>% 
  transmute(dna_comp_code_about = dna_comp, year, articles = n, noidea_X = X, noidea_Unn = Unnamed..0, FullDNAName= Description, corporatefamily = cleaned_companies)

DNA_comp_freq_2013_join <- DNA_comp_freq_2013 %>% group_by(year, corporatefamily) %>% summarise(articles = sum(articles))

rm(DNA_2013)

####
DNA_2014 <- read.csv("data/working/DNA_2014.csv")

DNA_total_articles_2014 <- nrow(DNA_2014)
DNA_comp_freq_2014 <- DNA_2014 %>% 
  count(dna_comp = strsplit(company_codes_about, ","), 
        year = lubridate::year(as.POSIXct(publication_date/1000, origin="1970-01-01"))) %>% 
  tidyr::unnest(cols = "dna_comp") %>% 
  filter(nchar(dna_comp)>1 ) %>% 
  mutate(dna_comp = str_to_upper(dna_comp)) %>% 
  left_join(DNA_clean, by = c("dna_comp" = "Code")) %>% 
  transmute(dna_comp_code_about = dna_comp, year, articles = n, noidea_X = X, noidea_Unn = Unnamed..0, FullDNAName= Description, corporatefamily = cleaned_companies)

DNA_comp_freq_2014_join <- DNA_comp_freq_2014 %>% group_by(year, corporatefamily) %>% summarise(articles = sum(articles))

rm(DNA_2014)

#####

DNA_2015 <- read.csv("data/working/DNA_2015.csv")

DNA_total_articles_2015 <- nrow(DNA_2015)
DNA_comp_freq_2015 <- DNA_2015 %>% 
  count(dna_comp = strsplit(company_codes_about, ","), 
        year = lubridate::year(as.POSIXct(publication_date/1000, origin="1970-01-01"))) %>% 
  tidyr::unnest(cols = "dna_comp") %>% 
  filter(nchar(dna_comp)>1 ) %>% 
  mutate(dna_comp = str_to_upper(dna_comp)) %>% 
  left_join(DNA_clean, by = c("dna_comp" = "Code")) %>% 
  transmute(dna_comp_code_about = dna_comp, year, articles = n, noidea_X = X, noidea_Unn = Unnamed..0, FullDNAName= Description, corporatefamily = cleaned_companies)

DNA_comp_freq_2015_join <- DNA_comp_freq_2015 %>% group_by(year, corporatefamily) %>% summarise(articles = sum(articles))

rm(DNA_2015)

#####


DNA_2016 <- read.csv("data/working/DNA_2016.csv")

DNA_total_articles_2016 <- nrow(DNA_2016)
DNA_comp_freq_2016 <- DNA_2016 %>% 
  count(dna_comp = strsplit(company_codes_about, ","), 
        year = lubridate::year(as.POSIXct(publication_date/1000, origin="1970-01-01"))) %>% 
  tidyr::unnest(cols = "dna_comp") %>% 
  filter(nchar(dna_comp)>1 ) %>% 
  mutate(dna_comp = str_to_upper(dna_comp)) %>% 
  left_join(DNA_clean, by = c("dna_comp" = "Code")) %>% 
  transmute(dna_comp_code_about = dna_comp, year, articles = n, noidea_X = X, noidea_Unn = Unnamed..0, FullDNAName= Description, corporatefamily = cleaned_companies)

DNA_comp_freq_2016_join <- DNA_comp_freq_2016 %>% group_by(year, corporatefamily) %>% summarise(articles = sum(articles))

rm(DNA_2016)

#####


DNA_2017 <- read.csv("data/working/DNA_2017.csv")

DNA_total_articles_2017 <- nrow(DNA_2017)
DNA_comp_freq_2017 <- DNA_2017 %>% 
  count(dna_comp = strsplit(company_codes_about, ","), 
        year = lubridate::year(as.POSIXct(publication_date/1000, origin="1970-01-01"))) %>% 
  tidyr::unnest(cols = "dna_comp") %>% 
  filter(nchar(dna_comp)>1 ) %>% 
  mutate(dna_comp = str_to_upper(dna_comp)) %>% 
  left_join(DNA_clean, by = c("dna_comp" = "Code")) %>% 
  transmute(dna_comp_code_about = dna_comp, year, articles = n, noidea_X = X, noidea_Unn = Unnamed..0, FullDNAName= Description, corporatefamily = cleaned_companies)

DNA_comp_freq_2017_join <- DNA_comp_freq_2017 %>% group_by(year, corporatefamily) %>% summarise(articles = sum(articles))

rm(DNA_2017)

#####

DNA_comp_freq_allyears_join <- rbind(DNA_comp_freq_2013_join, DNA_comp_freq_2014_join, 
                                     DNA_comp_freq_2015_join, DNA_comp_freq_2016_join, DNA_comp_freq_2017_join)

DNA_yearly_totals <- data.frame(year = c(2013:2017), total_articles = c(DNA_total_articles_2013, DNA_total_articles_2014, DNA_total_articles_2015, 
  DNA_total_articles_2016, DNA_total_articles_2017))

ndc_potential_corpfam_listings$year <- as.numeric(ndc_potential_corpfam_listings$year)

ndc_corpfam_scatter  <- ndc_potential_corpfam_listings %>% arrange(desc(perc_ndc)) %>% 
  left_join(DNA_comp_freq_allyears_join, by = c("year", "corporate.family" = "corporatefamily"))  %>%
  left_join(DNA_yearly_totals, by = "year") %>%
  mutate(articles = tidyr::replace_na(articles, replace = 0)) %>%
  mutate(perc_dna = articles/total_articles, 
         perc_dna_ = scales::percent(articles/total_articles)) 
  
ndc_corpfam_scatter %>% head()

# saveRDS(ndc_corpfam_scatter, "~/git/dspg20BI/data/working/ndc_dna_corpfam_scatter.RDS")
# write.csv(ndc_corpfam_scatter, "~/git/dspg20BI/data/working/ndc_dna_corpfam_scatter.csv")

ndc_corpfam_scatter <- readRDS("~/git/dspg20BI/data/working/ndc_dna_corpfam_scatter.RDS")

library(ggplot2)
ggplot(data = ndc_corpfam_scatter, aes(x = perc_ndc, y = perc_dna)) + 
  geom_point(stat = "identity") +
  facet_grid(~year)






