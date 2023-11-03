function_plot_trend <-  function(varBirth,varPop,pop_group, CitGroup, CanGroup, AgeGroup, Title){
  
  
# varBirth="total_birth"
# varPop="population"
# pop_group="Geschlecht - Total"
# CitGroup="total"
# CanGroup="Switzerland"
# AgeGroup = "Alter - Total"
  
  
load("data/data_total.RData")
  dat.trend <- data_total %>%
    filter(Geschlecht==pop_group) %>%
    filter(Citizenship==CitGroup) %>%
    filter(Canton== CanGroup) %>%
    filter(Alter==  AgeGroup) %>%
    select(eval(substitute(varBirth)),Year, Month,eval(substitute(varPop))) %>%
    rename(birth_var = eval(substitute(varBirth)),
           denominator = eval(substitute(varPop))) %>%
    mutate(birth = ymd(paste0(Year,"-", Month,"-01")),
           birth_inc = birth_var/denominator*10000)
  

plot_trend <- ggplot()+
  annotate("rect",xmin=ymd("1890-08-01"),xmax=ymd("1890-11-01"),ymin=-Inf,ymax=Inf,alpha=1,fill="indianred1") +
  annotate("text",x=ymd("1889-09-01"),y=28,label="\"Russian flu\"",angle = 90, size=6) +
  
  annotate("rect",xmin=ymd("1915-05-01"),xmax=ymd("1915-06-01"),ymin=-Inf,ymax=Inf,alpha=1,fill="indianred1") +
  annotate("text",x=ymd("1914-03-01"),y=25,label="Begin first world war",angle = 90, size=6) +
  
  annotate("rect",xmin=ymd("1919-05-01"),xmax=ymd("1919-09-01"),ymin=-Inf,ymax=Inf,alpha=1,fill="indianred1") +
  annotate("text",x=ymd("1918-04-01"),y=25,label="End first world war/\"Spanish flu\"",angle = 90, size=6) +
  
  # annotate("rect",xmin=ymd("1919-06-01"),xmax=ymd("1919-09-01"),ymin=-Inf,ymax=Inf,alpha=0.8,fill="indianred1") +
  # annotate("text",x=ymd("1919-01-01"),y=25,label="\"Spanish flu\"",angle = 90, size=6) +
  annotate("rect",xmin=ymd("1920-09-01"),xmax=ymd("1920-11-01"),ymin=-Inf,ymax=Inf,alpha=1,fill="indianred1") +
  annotate("text",x=ymd("1921-12-01"),y=25,label="\"Spanish flu\"-later strong wave",angle = 90, size=6) +
  
  annotate("rect",xmin=ymd("1940-05-01"),xmax=ymd("1940-06-01"),ymin=-Inf,ymax=Inf,alpha=1,fill="indianred1") +
  annotate("text",x=ymd("1939-04-01"),y=25,label="Begin second World War",angle = 90, size=6) +
  
  annotate("rect",xmin=ymd("1946-01-01"),xmax=ymd("1946-02-01"),ymin=-Inf,ymax=Inf,alpha=1,fill="indianred1") +
  annotate("text",x=ymd("1946-12-01"),y=25,label="End second World War",angle = 90, size=6) +
  
  annotate("rect",xmin=ymd("1959-05-01"),xmax=ymd("1959-12-01"),ymin=-Inf,ymax=Inf,alpha=0.8,fill="indianred1") +
  annotate("text",x=ymd("1958-06-01"),y=25,label="\"Asian flu\"",angle = 90, size=6) +
  
  annotate("rect",xmin=ymd("1962-01-01"),xmax=ymd("1962-02-01"),ymin=-Inf,ymax=Inf,alpha=1,fill="indianred1") +
  annotate("text",x=ymd("1963-08-01"),y=25,label="First approval of birth control pill",angle = 90, size=6) +
  
  annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-02-01"),ymin=-Inf,ymax=Inf,alpha=1,fill="indianred1") +
  annotate("text",x=ymd("2019-11-01"),y=25,label="Covid-19",angle = 90, size=6) +
  
  annotate("rect",xmin=ymd("2021-06-01"),xmax=ymd("2021-08-01"),ymin=-Inf,ymax=Inf,alpha=1,fill="indianred1") +
  # annotate("text",x=ymd("2020-12-01"),y=25,label="Covid-19 - Second wave",angle = 90, size=6) +
  annotate("rect",xmin=ymd("2022-09-01"),xmax=ymd("2023-02-01"),ymin=-Inf,ymax=Inf,alpha=1,fill="indianred1") +
  # annotate("text",x=ymd("2020-12-01"),y=25,label="Covid-19 - Omicron",angle = 90, size=6) +
  geom_line(data=dat.trend, aes(x=birth, y=birth_inc, col="births"),lwd=1) +
  
  scale_x_date(labels = date_format("%Y"), 
               breaks = date_breaks("10 year"),
                   # limits =c(min(ymd("2018-01-01")), max(ymd("2023-09-01")))) +
               limits =c(min(ymd("1871-01-01")), max(ymd("2023-12-01")))) +
  # coord_cartesian(ylim=c(0, 50)) +
      ggtitle(Title) +
      # xlim(1910, 1968) +
      xlab("Year") +
      # ylab("Births per 10'000 inhabitants")+
      ylab("Births per 10'000 inhabitants")+
      scale_color_manual("",
                         values=c( "grey40"),
                         guide = "none") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size=20),
        # legend.position = c(0.2,0.2),
        legend.position = "bottom",
        legend.text=element_text(size=16),
        # legend.key.size = unit(3.5, 'cm'),
        # legend.spacing.x = unit(3.5, 'cm'),
        axis.text.x = element_text(size=20),
        axis.title.x  = element_text(size=20),
        axis.title.y  = element_text(size=20),
        plot.title = element_text(size=25))
  
    
  
  cowplot::save_plot(paste0("output/plot_trend_",varBirth,".pdf"),   plot_trend ,base_height=10,base_width=20)
  
}


function_plot_trend (varBirth="total_birth",varPop="population",
                     pop_group="Geschlecht - Total",CitGroup="total", 
                     CanGroup="Switzerland",AgeGroup = "Alter - Total",
                     Title ="Birth trends from 1871-2023 in Switzerland")


