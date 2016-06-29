
options(scipen=999) #no scientific notation


packages <- c(
  "dplyr"
  ,"RSQLServer"
  ,"RODBC"
  ,"RJDBC"
  ,"rJava"
  ,"lazyeval"
  ,"lubridate"
  ,"tidyr"
  ,"earth"
  ,"zoo"
  ,"quantmod"
  ,"mclust"
  ,"glmnet"
  ,"magrittr"
  ,"httr"
  ,"knitr"
  ,"rmarkdown"
  ,"Hmisc"
  ,"stringdist"
  ,"stringr"
  ,"data.table"
  ,"xlsx")

#load/install packages
for (pack in packages) {
  if (!(pack %in% installed.packages()[, "Package"])) {
    install.packages(pack, repos='http://cran.us.r-project.org')
  }
  library(pack, character.only=TRUE)
}



#some functions that will make life easier
nse <- function(obj) {
  if ("lazy" %in% class(obj)) {
    return(lazy_eval(obj))
  }
  else {
    return(obj)
  }
}


# Generate Group Numbers
get_group_number = function(){
  i = 0
  function(){
    i <<- i+1
    i
  }
}
