form <- (ClusterID ~

           diff +

           as.logical(PrintDigital=="Digital") +
           as.logical(PrintDigital=="Print") +
           
           as.logical(CustomTraditionalReporting=="Traditional") +
           as.logical(CustomTraditionalReporting=="Custom") +
           
           as.logical(Looseleaf==" No") +
           as.logical(Looseleaf=="Yes") +
           
           as.logical(SubDivisionDesc=="B&E") +
           as.logical(SubDivisionDesc=="HSSL") +
           as.logical(SubDivisionDesc=="SEM") +
           
           as.logical(AreaDescription=="BUSINESS & ECONOMICS") +
           as.logical(AreaDescription=="HUMANITIES") +       
           as.logical(AreaDescription=="BIOLOGY") +
           as.logical(AreaDescription=="SOCIAL SCIENCE") +          
           as.logical(AreaDescription=="MATHEMATICS & SCIENCE") +
           as.logical(AreaDescription=="FOREIGN LANGUAGES") +     
           as.logical(AreaDescription=="ENGINEERING") +
           as.logical(AreaDescription=="EDUCATION") +
           
           as.logical(SaleType=="Combo") +
           as.logical(SaleType=="PrePack") +
           as.logical(SaleType=="StandAlone") +
           
           as.logical(ExecutiveRepTagNew=="Print Custom") +
           as.logical(ExecutiveRepTagNew=="Print Traditional") +
           as.logical(ExecutiveRepTagNew=="eBook") +
           as.logical(ExecutiveRepTagNew=="Other Digital") +
           as.logical(ExecutiveRepTagNew=="Connect") +
           as.logical(ExecutiveRepTagNew=="ALEKS") +
           
           sponsorcode
         
)


colnames(pmmd_cd)
unique(pmmd_cd$Looseleaf) 
