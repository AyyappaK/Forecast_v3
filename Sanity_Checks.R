# Columns taken from raw data set
intersect(colnames(pmm_raw),colnames(pmm))

# Columns not taken from raw data set
setdiff(colnames(pmm_raw),colnames(pmm))


# Check if a family ISBN has more than one program copy right year
pmm %>% select(FamilyISBN, ProgramCopyrightyear) %>% distinct() %>% filter(FamilyISBN != "") %>% group_by(FamilyISBN) %>%
  summarise(count_familyISBN_cr = n()) %>% filter(count_familyISBN_cr > 1)

# Example ISBN with multiple copy right years in a family ISBN
pmm %>% filter(FamilyISBN == "0070013233") %>% select(ProgramCopyrightyear) %>% distinct()
pmm %>% filter(FamilyISBN == "0070013233") %>% group_by(ProgramCopyrightyear) %>%  summarise(count_by_copyrightyear = n())
  
# Count by Customer Class
pmm_raw  %>% group_by(ShipToCollegePartyCustomerClass) %>% summarise(Count_CustomerClass = n())

unique(pmm_raw$DigitalRevenueLine)%>% head(15)

# Attributes defining the uniqueness of an observation
pmmd %>% select(FamilyISBN,ExecutiveReportingTag, CustomTraditionalReporting,PrintDigital,Looseleaf,SaleType,
                TransactionCalendarYear,TransactionCalendarMonth,
                ProgramCopyrightyear,GrossSales, GrossUnits) %>% 
  filter(FamilyISBN == "0070013233" & ProgramCopyrightyear == 2014 & TransactionCalendarYear == 2015 & 
           TransactionCalendarMonth == 1 )%>% 
  arrange(ProgramCopyrightyear,TransactionCalendarMonth) %>%
  View()

# Summary statistics by factors defining the uniqueness of observations
summary(pmmd %>% select(ExecutiveReportingTag,CustomTraditionalReporting,PrintDigital,
                        Looseleaf,SaleType))






