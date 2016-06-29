
# pmmdfvars <- c("SubDivisionDesc",
#                "ExecutiveRepTagNew",
#                "sponsorcodemasterGroup",
#                "Discipline",
#                "AreaDescription",
#                c("Discipline","AreaDescription"))

#par(mar=c(3,3,3,3))
#plot(x = rownames(discipline_filter), y=  round(discipline_filter$cume_sales_share,4))

#names(Filter(is.integer, pmmd))
#names(Filter(is.factor, pmmd))
#names(Filter(is.character, pmmd))

# Sponsor code, dimensionality reduction 
sponsorcode_filter <- pmmd %>%  ungroup %>% 
     select(sponsorcode, GrossSales) %>% 
     group_by(sponsorcode) %>% 
     summarise(sales = sum(GrossSales), n_count = n()) %>% 
     ungroup %>% 
     arrange(desc(sales)) %>% 
     mutate(sales_share = sales/sum(sales), cum_sales_share = cumsum(sales/sum(sales))) %>%
     arrange(-sales_share) %>% 
     filter(cum_sales_share <= .97) %>% 
     select(sponsorcode) %>% 
     unlist() %>% as.vector()

# Sub Division Desc, dimensionality reduction
sdd_filter <- pmmd %>%  ungroup %>% 
  select(SubDivisionDesc, GrossSales) %>% 
  group_by(SubDivisionDesc) %>% 
  summarise(sales = sum(GrossSales), n_count = n()) %>% 
  ungroup %>% 
  arrange(desc(sales)) %>% 
  mutate(sales_share = sales/sum(sales), cum_sales_share = cumsum(sales/sum(sales))) %>%
  arrange(-sales_share) %>% 
  filter(cum_sales_share <= .98) %>% 
  select(SubDivisionDesc) %>% 
  unlist() %>% 
  as.vector()
  #B&E, SEM, HSSL account for 98%


exec_filter <- pmmd %>%
  ungroup %>%
  select(ExecutiveRepTagNew,GrossSales)%>%
  group_by(ExecutiveRepTagNew) %>%
  summarise(count=n(),
            sales=sum(GrossSales)) %>%
  ungroup %>%
  arrange(-sales) %>%
  mutate(sales_share = sales / sum(sales),
         cume_sales_share = cumsum(sales)/sum(sales)) %>%
  data.frame() %>%
  select(ExecutiveRepTagNew) %>%
  unlist %>%
  as.vector
# Executive reporting tag (Computed) has only 6 dimensions, and it makes sense to include them all for 
# explaining the variability


exec_filter <- pmmd %>%
  ungroup %>%
  select(ExecutiveRepTagNew,GrossSales)%>%
  group_by(ExecutiveRepTagNew) %>%
  summarise(count=n(),
            sales=sum(GrossSales)) %>%
  ungroup %>%
  arrange(-sales) %>%
  mutate(sales_share = sales / sum(sales),
         cume_sales_share = cumsum(sales)/sum(sales)) %>%
  data.frame() %>%
  select(ExecutiveRepTagNew) %>%
  unlist %>%
  as.vector


# Discipline, dimensionality reduction
discipline_filter <- pmmd %>% 
  ungroup %>% 
  select(Discipline,GrossSales)%>%
  group_by(Discipline) %>%
  summarise(count=n(),sales=sum(GrossSales)) %>%
  ungroup %>%
  arrange(-sales) %>% 
  mutate(sales_share = sales / sum(sales),
         cume_sales_share = cumsum(sales)/sum(sales)) %>%
  filter(count>=2000 | cume_sales_share<= .98) %>%
  filter(!is.na(Discipline)) %>% 
  select(Discipline) %>%
  unlist %>% 
  as.vector


adesc_filter <- pmmd %>% 
  ungroup %>% 
  select(AreaDescription,GrossSales)%>%
  group_by(AreaDescription) %>%
  summarise(count=n(),sales=sum(GrossSales)) %>%
  ungroup %>%
  arrange(-sales) %>% 
  mutate(sales_share = sales / sum(sales),
         cume_sales_share = cumsum(sales)/sum(sales)) %>% 
  #  filter(count>=1000 | cume_sales_share<=.98) %>%
  filter(sales_share>=.0075) %>%
  filter(!is.na(AreaDescription)) %>% 
  select(AreaDescription) %>%
  unlist %>% 
  as.vector


sponsorcode_filter %>% saveRDS('C://RProjects/DataSets/derived_data_2/Forecast_v3/sponsorcode_filter.rds')
adesc_filter %>% saveRDS('C://RProjects/DataSets/derived_data_2/Forecast_v3/adesc_filter.rds')
discipline_filter %>% saveRDS('C://RProjects/DataSets/derived_data_2/Forecast_v3/discipline_filter.rds')
sdd_filter %>% saveRDS('C://RProjects/DataSets/derived_data_2/Forecast_v3/sdd_filter.rds') 


