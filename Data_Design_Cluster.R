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

# Set group number by Unique Identifying factors (listed above) & group by other non - identifying factors
group_number = get_group_number() #1151329
pmmd_c  <- pmmd %>%  
  select(FamilyISBN,
         ExecutiveReportingTag, 
         CustomTraditionalReporting,
         PrintDigital,
         Looseleaf,
         SaleType,
         ProgramCopyrightyear,
         sponsorcode,
         AreaDescription,
         SubDivisionDesc,
         Discipline,
         ExecutiveRepTagNew,
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
           TransactionCalendarMonth,
           sponsorcode,
           AreaDescription,
           SubDivisionDesc,
           Discipline,
           ExecutiveRepTagNew) %>% 
  summarise(GrossSales = sum(GrossSales),
            GrossUnits = sum(GrossUnits)) %>%
  ungroup() %>%
  group_by(FamilyISBN,
         ExecutiveReportingTag, 
         CustomTraditionalReporting,
         PrintDigital,
         Looseleaf,
         SaleType,
         ProgramCopyrightyear,
         sponsorcode,
         AreaDescription,
         SubDivisionDesc,
         Discipline,
         ExecutiveRepTagNew ) %>%
  mutate(ID = group_number()) %>%
  data.frame()


# First Transaction year by Copy right year 
pmm_first_sales_progmCopyrightYear_Group <- pmmd_c %>% 
  group_by(ID,ProgramCopyrightyear) %>%
  summarise(first_sale_year_CrY = min(TransactionCalendarYear)) %>% 
  ungroup

# Join first sale transaction year to the main table
pmmd_c <- pmmd_c %>% 
  left_join(pmm_first_sales_progmCopyrightYear_Group) %>% 
  mutate(first_sale_year_CrY = ifelse(first_sale_year_CrY==2007 & ProgramCopyrightyear<2007,
                                  ProgramCopyrightyear,
                                  first_sale_year_CrY)) %>% 
  arrange(ID,
          ProgramCopyrightyear,
          TransactionCalendarYear, 
          TransactionCalendarMonth)%>%
  filter(ProgramCopyrightyear != 0) %>%
  mutate(diff = (TransactionCalendarYear - ProgramCopyrightyear))%>%
  mutate(IdDiff = paste(ID,diff, sep = "@"))

#summary(pmmd_c$diff)
#pmmd_c %>% filter(diff < 0)
#pmmd_c %>% filter(diff == 64)

# Transpose into column matrix
pmmd_ctranspose <- dcast(pmmd_c, IdDiff ~ TransactionCalendarMonth, value.var="GrossSales", fill=0) %>% 
  arrange(IdDiff) %>% mutate(YMean = rowMeans(.[2:13]))

# Remove records that have no sales in any of the month (Extra caution)
row_sub =  apply(pmmd_ctranspose[,-1], 1, function(row) all(row == 0))
pmmd_ctranspose <- pmmd_ctranspose[!row_sub,] #223995
rm(row_sub)

# Gross units are zero for almost 20,000+ records though there is Gross sales number
#pmmd_ctranspose %>% mutate(sum = rowSums(.[2:13])) %>% filter(sum <= 0)
#pmmd_c %>% filter(IdDiff == "12563@5")

# range standardization on each row
pmmd_ctranspose <- cbind(pmmd_ctranspose[,c(1,14)], 
                         t(apply(pmmd_ctranspose[,c(-1,-14)], 1, 
                                 function(x)(x-min(x))/(max(x)-min(x))))) %>% 
  data.frame() %>% mutate(YMean = range_stdz(YMean))
