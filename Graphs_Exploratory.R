fil <- lazy(ProgramCopyrightyear>2006 & ProgramCopyrightyear < 2016 & ExecutiveRepTagNew == "Print Traditional")

ggplot(pmmd %>% filter_(fil), aes(x=date, y=GrossSales/1000000, colour=ExecutiveRepTagNew))+
  geom_line() + 
  scale_x_date(date_labels = "%b %Y") +
  facet_wrap(~ProgramCopyrightyear,nrow=8)


ggplot(data = pmmd %>% filter_(fil), aes( date,  GrossSales/100000))+
  geom_line(aes(colour =  ExecutiveRepTagNew, group = ExecutiveRepTagNew))+
  geom_point()+
  ggtitle("Sales profiling for ISBN :")+ #scale_x_discrete(drop=FALSE)+
  scale_x_date(date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle=-45, face="bold", colour="black"))



fil <- lazy(SubDivisionDesc=="B&E" &
              ProgramEdition<9)

ggplot(pmmd_predict_summarym, aes(x=date, y=GrossSales/1000000, colour=SubDivisionDesc))+
  geom_line() + 
  scale_x_date(date_labels = "%b %Y") +
  facet_wrap(~ProgramCopyrightyear,nrow=8)


p <- ggplot(df %>% 
              filter_(fil), 
            aes(x=date,
                y=GrossSales/1000000, 
                colour=ProgramEdition))
p + 
  geom_line() + 
  scale_x_date(date_labels = "%b %Y") +
  facet_wrap(~ProgramCopyrightyear,nrow=8)