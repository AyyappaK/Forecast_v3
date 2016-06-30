
source("dependencies.R")


# New titles data import
pm_newtitle <- read.xlsx2(file = "C://RProjects/DataSets/2016-2018_Pub_Products.xlsx",
                       sheetIndex = 1,header = TRUE,stringsAsFactors = FALSE)


pm_newtitle_d <- pm_newtitle %>% 
  select(ISBN, ISBN13,PROGRAM_ISBN,TITLE,LEAD_AUTHOR,EDITION,COPYRIGHT,STAT_SPONSOR_CODE,ITEM_TYPE) %>%
  filter(EDITION > 1 & !is.na(TITLE) & !is.na(LEAD_AUTHOR) & !is.na(PROGRAM_ISBN)) %>%
  data.frame()
  

pmm_raw_d <- pmm_raw %>% 
         select(FamilyISBN,ProgramISBN,Title,LeadContributor) %>% 
         distinct() %>%
         group_by(Title) %>% 
         mutate(n_pisbn_count = n_distinct(ProgramISBN)) %>%
         filter(n_pisbn_count == 1) %>% 
         mutate(ProgramISBN = as.character(ProgramISBN), Flag = "Y") %>%
         data.frame()

pm_newtitle_ne <-  pm_newtitle_d %>% left_join(pmm_raw_d %>% select(ProgramISBN,Flag) %>% distinct(), 
                                  by = c("PROGRAM_ISBN" = "ProgramISBN"))%>% 
                   filter(is.na(Flag)) %>% select(-Flag)

# Remove special Characters & Punctuations 
pm_newtitle_ne$LEAD_AUTHOR <- tolower(gsub("[[:punct:]]", " ", pm_newtitle_ne$LEAD_AUTHOR)) 
pm_newtitle_ne$TITLE <- tolower(gsub("[[:punct:]]", " ", pm_newtitle_ne$TITLE)) 

pmm_raw_d1 <- pmm_raw_d[!duplicated(pmm_raw_d$ProgramISBN),]
# Remove special Characters & Punctuations 
pmm_raw_d1$LeadContributor <- tolower(gsub("[[:punct:]]", " ", pmm_raw_d1$LeadContributor)) 
pmm_raw_d1$Title <- tolower(gsub("[[:punct:]]", " ", pmm_raw_d1$Title)) 


BestMatchPattern <- function(title_new, titles_old, titles_ISBN) {
  result <- ""
  if(title_new[2] != "" & !is.na(title_new[2]))
  {
    distance <- stringdist(title_new[2],titles_old,method = c("lv"))
    result <- data.frame(ISBN_new = title_new[1], Title_new =  title_new[2], 
                         Title_old =  titles_old[distance == min(distance)],
                         ISBN_old =  titles_ISBN[distance == min(distance)])
  }
  return(result)
}

# Fuzzy Match New titles with Existing titles by Levenshtein min distinace string match method
Mapping_title <-  do.call("rbind",apply( pm_newtitle_ne %>% select(ISBN,TITLE), 1,
                                          BestMatchPattern,pmm_raw_d1$Title,pmm_raw_d1$ProgramISBN))


# Fuzzy Matching New Title Authors with Existing Title Authors (First Name, Last Name match)
WordMatchingBySplit <- function(title_new, titles_old, titles_ISBN) {
  result <- ""
  if(title_new[2] != "" & !is.na(title_new[2]))
  {
    vwords <- unlist(strsplit(gsub("[[:punct:]]", " ", title_new[2]), split = " "))
    for (i in 1: length(vwords)) {
      regexword <- paste("\\<",vwords[i],"\\>", sep = "")
      matches <-   grep(regexword, titles_old,ignore.case = T,value = F)
      if(length(matches) >0){ result = data.frame(ISBN_new = title_new[1], Author_new =  title_new[2],
                                                  Author_old =  titles_old[matches],ISBN_old =  titles_ISBN[matches]
                                                  #,row.names = seq(1,length(matches),by = 1)
      )}
    }
  }
  return(result)  
}
 
Mapping_Author <- do.call("rbind",apply(pm_newtitle_ne %>% select(ISBN,LEAD_AUTHOR) , 1,
                                        WordMatchingBySplit,pmm_raw_d1$LeadContributor,
                                        pmm_raw_d1$ProgramISBN))

Mapping_Author <- Mapping_Author %>% 
  filter(!is.na(ISBN_new) & !is.na(Author_new) & !is.na(Author_old)) %>% 
  arrange(ISBN_new,ISBN_old)


Mapping_Title_Author <- merge(x = Mapping_title, y= Mapping_Author, by = c("ISBN_new", "ISBN_old")) %>%
                 arrange(ISBN_new,ISBN_old)



# pm_newtitle_nomatch <- pm_newtitle_ne[!duplicated(rbind(Mapping_Title_Author,pm_newtitle_ne))
#                                       [-seq_len(nrow(Mapping_Title_Author))],]

pm_newtitle_nomatch <- pm_newtitle_ne %>% 
                  left_join(Mapping_Title_Author %>% 
                              select(ISBN_new) %>% 
                              distinct() %>% 
                              mutate(Flag = "Y", ISBN_new = as.character(ISBN_new)),
                              by = c("ISBN" = "ISBN_new")) %>%
  filter(is.na(Flag)) %>% select(-Flag)

Mapping_Title_Author %>% saveRDS('C://RProjects/DataSets/derived_data_2/Forecast_v3/Mapping_Title_Author.rds')
Mapping_Author %>% saveRDS('C://RProjects/DataSets/derived_data_2/Forecast_v3/Mapping_Author.rds')
Mapping_title %>% saveRDS('C://RProjects/DataSets/derived_data_2/Forecast_v3/Mapping_title.rds')

Mapping_Title_Author %>% write.csv('C://RProjects/DataSets/derived_data_2/Forecast_v3/Mapping_Title_Author.csv')

pm_newtitle_nomatch %>% write.csv('C://RProjects/DataSets/derived_data_2/Forecast_v3/New_titles_Nomatch.csv')


#rm(Mapping_Author,Mapping_title,Mapping_Title_Author)


