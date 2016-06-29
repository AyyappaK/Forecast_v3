source("dependencies.R")

# Unique Identity Factors
#FamilyISBN
#ExecutiveReportingTag
#CustomTraditionalReporting
#PrintDigital
#Looseleaf
#SaleType
#ProgramCopyrightyear
#TransactionCalendarYear
#TransactionCalendarMonth

# Set group number by Unique Identifying factors, listed above
group_number = get_group_number()
pmmd_C <- pmmd %>%  
  select(FamilyISBN,
         ExecutiveReportingTag, 
         CustomTraditionalReporting,
         PrintDigital,
         Looseleaf,
         SaleType,
         ProgramCopyrightyear,
         TransactionCalendarYear,
         TransactionCalendarMonth,
         GrossSales, 
         GrossUnits) %>% 
  group_by(FamilyISBN,
           ExecutiveReportingTag, 
           CustomTraditionalReporting,
           PrintDigital,
           Looseleaf,
           SaleType,
           ProgramCopyrightyear,
           TransactionCalendarYear,
           TransactionCalendarMonth) %>% 
  summarise(GrossSales = sum(GrossSales),
            GrossUnits = sum(GrossUnits)) %>%
  ungroup() %>%
  group_by(FamilyISBN,
         ExecutiveReportingTag, 
         CustomTraditionalReporting,
         PrintDigital,
         Looseleaf,
         SaleType,
         ProgramCopyrightyear) %>%
  mutate(ID = group_number())

max(pmmd_C$ID)
dim(pmmd_C)

pmmd_C <-


  
  
  
  
  
  
  
  
pmmd_C %>% 
  #select(ID,TransactionCalendarYear,TransactionCalendarMonth, GrossSales,GrossUnits) %>%
  filter(ID == 362163) %>% arrange(TransactionCalendarYear, TransactionCalendarMonth) %>% View()
  
pmmd_C %>% group_by(ID,TransactionCalendarYear,TransactionCalendarMonth) %>% 
  summarise(n_count = n()) %>%
  filter(n_count > 1)


pmmd %>% select(FamilyISBN,ExecutiveReportingTag, CustomTraditionalReporting,
                PrintDigital,Looseleaf,SaleType,sponsorcode,
                TransactionCalendarYear,TransactionCalendarMonth,
                ProgramCopyrightyear,GrossSales, GrossUnits) %>% 
  filter(FamilyISBN == "0070013233" & ProgramCopyrightyear == 2014 & TransactionCalendarYear == 2014 & 
           TransactionCalendarMonth == 11 )%>% 
  arrange(ProgramCopyrightyear,TransactionCalendarMonth) %>%
  View()

colnames(pmmd)

