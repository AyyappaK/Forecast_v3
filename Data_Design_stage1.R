#if( !any(grepl(ls(), pattern="^orcon1$"))) 

source('dependencies.R')

#most recent date indexed on the pmm rds file
most_recent_pmm_dtm <- gsub(list.files('C://RProjects/DataSets/raw_data',
                                       pattern="^pmm_final_"),
                            pattern='^(pmm_final_)([0-9]*)(.rds)$',
                            replacement="\\2") %>% 
  as.numeric %>% 
  max

#formatted name of the rds
most_recent_pmm_name <- paste0('pmm_final_',most_recent_pmm_dtm,'.rds')

last_database_refresh_date <- as.Date(as.character(most_recent_pmm_dtm),"%Y%m%d")

if( !any(grepl(ls(), pattern="^pmm_raw$"))) {
  
  pmm_raw <- readRDS(paste0('C://RProjects/DataSets/raw_data/',most_recent_pmm_name))
}



pmm <- pmm_raw %>% 
  mutate(date = as.Date(ISOdate(year=TransactionCalendarYear,
                                month=TransactionCalendarMonth,
                                day=1L)),
         quarter = quarter(date)) %>% 
  select(FamilyISBN, #
         TransactionCalendarYear, #
         TransactionCalendarMonth, #
         date,
         quarter,
         sponsorcode,
         ProgramCopyrightyear, #
         FamilyTitle,
         FamilyLeadContributor,
         FirstEdition,
         ProgramEdition, 
         SubDivisionDesc, 
         DigitalProductCategory, #
         PrintDigital, #
         Looseleaf,
         ExecutiveReportingTag, #
         SaleType, #
         sponsorcodemasterGroup,
         Discipline,
         AreaDescription,
         CustomTraditionalReporting, #
         ShipToCollegePartyCustomerClass,
         GrossSales, #depenedent variable
         GrossUnits,
         NetSales,
         NetUnits) %>% 
  mutate(college_4 = as.logical(ShipToCollegePartyCustomerClass=="4 Yr College"),
         college_2 = as.logical(ShipToCollegePartyCustomerClass=="2 Yr College"),
         college_votech = as.logical(ShipToCollegePartyCustomerClass=="Career/Votech"),
         college_undefined = as.logical(ShipToCollegePartyCustomerClass=="Other" |
                                                           is.na(ShipToCollegePartyCustomerClass))
         ) %>% 
  group_by(FamilyISBN,
           sponsorcode,
           TransactionCalendarYear,
           TransactionCalendarMonth,
           date,
           quarter,
           ProgramCopyrightyear,
           FamilyTitle,
           FamilyLeadContributor,
           FirstEdition,
           ProgramEdition,
           SubDivisionDesc,
           DigitalProductCategory,
           PrintDigital,
           Looseleaf,
           ExecutiveReportingTag,
           SaleType,
           sponsorcodemasterGroup,
           Discipline,
           AreaDescription,
           CustomTraditionalReporting,
           college_4,
           college_2,
           college_votech,
           college_undefined) %>% 
  summarise(GrossSales=sum(GrossSales),
            GrossUnits=sum(GrossUnits),
            NetSales=sum(NetSales),
            NetUnits=sum(NetUnits)) %>% 
  ungroup %>% 
  mutate(FamilyISBN = as.character(FamilyISBN),
         FamilyLeadContributor = as.character(FamilyLeadContributor),
         FamilyTitle = as.character(FamilyTitle),
         ExecutiveRepTagNew = NA_character_,
         ExecutiveRepTagNew = ifelse(PrintDigital=="Digital" & (DigitalProductCategory=="Homework/Course Management Products" | 
                                                                  DigitalProductCategory=="LearnSmart"),
                                     "Connect",
                                     ExecutiveRepTagNew),
         ExecutiveRepTagNew = ifelse(PrintDigital=="Digital" & DigitalProductCategory=="ALEKS",
                                     "ALEKS",
                                     ExecutiveRepTagNew),
         ExecutiveRepTagNew = ifelse(PrintDigital=="Digital" & DigitalProductCategory=="eBooks",
                                     "eBook",
                                     ExecutiveRepTagNew),
         ExecutiveRepTagNew = ifelse(PrintDigital=="Digital" & (DigitalProductCategory=="Experiential Products" | 
                                                                  DigitalProductCategory=="Online Courses" | 
                                                                  DigitalProductCategory=="Review and Tutoring" | 
                                                                  DigitalProductCategory=="Simulations & Games" | 
                                                                  DigitalProductCategory=="Virtual Labs" | 
                                                                  is.na(DigitalProductCategory)),
                                     "Other Digital",
                                     ExecutiveRepTagNew),
         ExecutiveRepTagNew = ifelse(PrintDigital=="Print" & sponsorcodemasterGroup=="Custom",
                                     "Print Custom",
                                     ExecutiveRepTagNew),
         ExecutiveRepTagNew = ifelse(PrintDigital=="Print" & sponsorcodemasterGroup=="Traditional",
                                     "Print Traditional",
                                     ExecutiveRepTagNew),
         NationalCustom = ifelse(CustomTraditionalReporting=="Traditional",
                                 "National",
                                 "Custom"),
         CustomTraditionalDrillDown = NA_character_,
         CustomTraditionalDrillDown = ifelse(PrintDigital == "Digital" & NationalCustom=="Custom",
                                             "Digital Custom",
                                             CustomTraditionalDrillDown),
         CustomTraditionalDrillDown = ifelse(PrintDigital == "Digital" & NationalCustom=="National",
                                             "Digital Traditional",
                                             CustomTraditionalDrillDown),
         CustomTraditionalDrillDown = ifelse(PrintDigital == "Print" & NationalCustom=="Custom",
                                             "Print Custom",
                                             CustomTraditionalDrillDown),
         CustomTraditionalDrillDown = ifelse(PrintDigital == "Print" & NationalCustom=="National",
                                             "Print Traditional",
                                             CustomTraditionalDrillDown),
         DPCDrillDown = NA_character_,
         DPCDrillDown = ifelse(PrintDigital == "Digital" & SaleType %in% c("Standalone","StandAlone"),
                               "Digital Standalone",
                               DPCDrillDown),
         DPCDrillDown = ifelse(PrintDigital == "Digital" & SaleType %in% c("PrePack","Combo"),
                               "Digital Package",
                               DPCDrillDown),
         DPCDrillDown = ifelse(PrintDigital == "Print" & NationalCustom == "Custom" & Looseleaf %in% c("No","no"),
                               "Print Bound Custom",
                               DPCDrillDown),
         DPCDrillDown = ifelse(PrintDigital == "Print" & NationalCustom == "National" & Looseleaf %in% c("No","no"),
                               "Print Bound Traditional",
                               DPCDrillDown),
         DPCDrillDown = ifelse(PrintDigital == "Print" & Looseleaf %in% c("Yes","yes"),
                               "Print Looseleaf",
                               DPCDrillDown)
  )

pmm %>% saveRDS('C://RProjects/DataSets/derived_data_2/Forecast_v3/pmm_current.rds')

head(pmm,10) %>% View()
rm(pmm_raw)
gc()
