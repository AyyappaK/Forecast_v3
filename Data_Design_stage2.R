
blank_quarrantine <- lazy(FamilyISBN=="" | 
                            FamilyLeadContributor=="" | 
                            FamilyTitle=="" | 
                            is.na(ProgramCopyrightyear) |
                            is.na(ExecutiveRepTagNew) |
                            is.na(ProgramEdition)
)

not_blank_records <- lazy(!FamilyISBN=="" & 
                                !FamilyLeadContributor=="" & 
                                !FamilyTitle=="" & 
                                !is.na(ProgramCopyrightyear) &
                                !is.na(ExecutiveRepTagNew) &
                                !is.na(ProgramEdition)
)

non_sales_quarrantine <- lazy(GrossSales<=0)
sales_records <- lazy(GrossSales>0)


pmm_blanks_quarrantine <- pmm %>% 
  filter_(blank_quarrantine) 

pmm_sales_quarrantine <- pmm %>% 
  filter_(non_sales_quarrantine)

# Keep only the non_blank records and the non empty sales transactions
pmm <- pmm %>% 
  filter_(not_blank_records) %>% 
  filter_(sales_records) 

# Number of Copyright years and Program Edition titles selling in each Quarter of a transaction year (With in a Family ISBN)
pmm_isbn <- pmm %>% 
  group_by(FamilyISBN,
           TransactionCalendarYear,
           quarter) %>% 
  summarise(num_copyrights_samequarter = n_distinct(ProgramCopyrightyear),
            num_editions_samequarter = n_distinct(ProgramEdition)) %>% 
  ungroup 

# Number of Months a Family ISBN in market (sales transaction)
pmm_cume_salemonths <- pmm %>% 
  select(FamilyISBN,date) %>%
  distinct() %>%  
  group_by(FamilyISBN) %>% 
  arrange(date) %>% 
  mutate(count = 1,
         cume_isbn_sale_months = cumsum(count)) %>% 
  select(date,
         cume_isbn_sale_months) %>% 
  ungroup 

# Number of Months an Individual Copy right Title of a Family ISBN in the market (sales transaction)
pmm_salemonths_progmCopyrightYear <- pmm %>% 
  select(FamilyISBN,
           ProgramCopyrightyear,
           date) %>% 
  distinct() %>%
  group_by(FamilyISBN,ProgramCopyrightyear) %>% 
  arrange(date) %>% 
  mutate(count = 1,
         isbn_sale_months = cumsum(count)) %>% 
  select(date,isbn_sale_months) %>% 
  ungroup 

# First Transaction year in a Family ISBN
pmm_first_sales <- pmm %>% 
  group_by(FamilyISBN,
           PrintDigital) %>%
  summarise(first_sale_year = min(TransactionCalendarYear),
            first_sale_date = as.Date(min(date))) %>% 
  ungroup

# First Transaction year by Copy right year 
pmm_first_sales_progmCopyrightYear <- pmm %>% 
  group_by(FamilyISBN,
           ProgramCopyrightyear,
           PrintDigital) %>%
  summarise(first_sale_year_CrY = min(TransactionCalendarYear),
            first_sale_date_CrY = as.Date(min(date))) %>% 
  ungroup


# Join all derived data elements to the main table.
pmmd <- pmm %>% 
  left_join(pmm_first_sales) %>% 
  mutate(first_sale_year = ifelse(first_sale_year==2007 & ProgramCopyrightyear<2007,
                                  ProgramCopyrightyear,
                                  first_sale_year)) %>% 
  left_join(pmm_cume_salemonths) %>% 
  left_join(pmm_isbn) %>% 
  left_join(pmm_salemonths_progmCopyrightYear) %>% 
  left_join(pmm_first_sales_progmCopyrightYear) %>% 
  mutate(first_sale_year_CrY = ifelse(first_sale_year_CrY==2007 & ProgramCopyrightyear<2007,
                                  ProgramCopyrightyear,
                                  first_sale_year_CrY)) %>% 
  arrange(FamilyISBN,
          FamilyTitle,
          FamilyLeadContributor,
          ProgramCopyrightyear,
          TransactionCalendarYear,
          TransactionCalendarMonth,
          date,
          quarter,
          ProgramEdition,
          PrintDigital,
          SubDivisionDesc,
          ExecutiveRepTagNew,
          CustomTraditionalDrillDown,
          DPCDrillDown,
          SaleType,
          sponsorcodemasterGroup)

# pmmd %>%filter(FamilyISBN == "0070013233") %>% select(ProgramCopyrightyear,first_sale_year_CrY,first_sale_year) %>% distinct() %>% 
# head(100) %>% View()

source('bin_variables.R')

pmmd <- pmmd %>% 
  mutate(sponsorcode = as.character(sponsorcode),
         sponsorcode = ifelse(!sponsorcode %in% sponsorcode_filter, 
                              "Other", 
                              sponsorcode),
         Discipline = as.character(Discipline),
         Discipline = ifelse(!Discipline %in% discipline_filter,
                             "Other",
                             Discipline),
         AreaDescription = as.character(AreaDescription),
         AreaDescription = ifelse(!AreaDescription %in% adesc_filter,
                                  "Other",
                                  AreaDescription),
         SubDivisionDesc = as.character(SubDivisionDesc),
         SubDivisionDesc = ifelse(!SubDivisionDesc %in% sdd_filter,
                                  "Other",
                                  SubDivisionDesc))

# Standardize levels in Sale Type
pmmd <- pmmd %>% 
  mutate(
    SaleType =  as.character(SaleType),
    SaleType =  ifelse(SaleType %in% c("Standalone","StandAlone"), "StandAlone", SaleType)) %>% 
  mutate(SaleType = as.factor(SaleType))



# source('map_prior_sales_benchmarks.R')
# 
# pmmd <- pmmd %>% 
#   filter(TransactionCalendarYear>=model_start_year) %>% 
#   left_join(pmm_exec_sales_benchmarks) %>% 
#   left_join(pmm_discipline_sales_benchmarks) %>% 
#   left_join(pmm_area_sales_benchmarks) %>% 
#   left_join(pmm_sales_expand) %>% 
#   mutate(SaleType = as.character(SaleType),
#          sponsorcodemasterGroup = as.character(sponsorcodemasterGroup),
#          FirstEdition = as.character(FirstEdition),
#          transaction_year_diff = TransactionCalendarYear - first_sale_year)
# 
# 
# pmmd %>% saveRDS('L:/derived_data/sales_model/pmmd.rds')