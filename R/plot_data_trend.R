
varBirth="total_birth"
varPop="population"
pop_group="female"
CitGroup="total"
CanGroup="Switzerland"
AgeGroup = "15-49"
Title ="Monthly birth rate in Switzerland, 1871-2022"

load("data/data_total.RData")
  
  dat.trend <- data_total %>%
    filter(Sex %in% pop_group,
           Citizenship %in% CitGroup,
           Canton %in% CanGroup,
           Age %in% AgeGroup,
           !Year %in% 2023) %>%
    select(eval(substitute(varBirth)),Year, Month,eval(substitute(varPop))) %>%
    rename(birth_var = eval(substitute(varBirth)),
           denominator = eval(substitute(varPop))) %>%
    mutate(Year = as.numeric(Year),
           birth = ymd(paste0(Year,"-", Month,"-01")),
           birth_inc = birth_var/denominator*1000) %>%
    arrange(Year)
  




plot_trend <- ggplot()+
  geom_line(data=dat.trend, aes(x=birth, y=birth_inc, col="births"),lwd=1) +

  annotate("rect",xmin=ymd("1890-09-01"),xmax=ymd("1890-11-01"),ymin=-Inf,ymax=6.5,alpha=1,fill="lightgrey") +
  annotate("text",x=ymd("1889-08-01"),y=5, label="+9m. 1890 flu",angle = 90, size=bar_text_size) +
  
  annotate("rect",xmin=ymd("1915-05-01"),xmax=ymd("1915-07-01"),ymin=8,ymax=Inf,alpha=1,fill="lightgrey") +
  annotate("text",x=ymd("1914-03-01"),y=10,label="+9m. Start WW1",angle = 90, size=bar_text_size) +
  
  annotate("rect",xmin=ymd("1919-07-01"),xmax=ymd("1919-09-01"),ymin=8,ymax=Inf,alpha=1,fill="lightgrey") +
  annotate("text",x=ymd("1921-12-01"),y=10,label="+9m. End WW1\n& 1918 flu",lineheight=0.8,angle = 90, size=bar_text_size) +
  
  annotate("rect",xmin=ymd("1940-05-01"),xmax=ymd("1940-07-01"),ymin=6,ymax=Inf,alpha=1,fill="lightgrey") +
  annotate("text",x=ymd("1938-09-01"),y=10,label="+9m. Start\nWW2",lineheight=0.8, angle = 90, size=bar_text_size) +
  
  annotate("rect",xmin=ymd("1946-01-01"),xmax=ymd("1946-03-01"),ymin=8,ymax=Inf,alpha=1,fill="lightgrey") +
  annotate("text",x=ymd("1947-04-01"),y=10,label="+9m. End WW2",angle = 90, size=bar_text_size) +
  
  annotate("rect",xmin=ymd("1958-06-01"),xmax=ymd("1958-08-01"),ymin=7,ymax=Inf,alpha=0.8,fill="lightgrey") +
  annotate("text",x=ymd("1957-04-01"),y=10,label="+9m. 1957 flu",angle = 90, size=bar_text_size) +
  
  annotate("rect",xmin=ymd("1962-01-01"),xmax=ymd("1962-03-01"),ymin=7,ymax=Inf,alpha=1,fill="lightgrey") +
  annotate("text",x=ymd("1964-03-01"),y=10,label="+9m. 1st approval\nbirth control pill",lineheight=0.8,angle = 90, size=bar_text_size) +
  
  annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-02-01"),ymin=4.5,ymax=Inf,alpha=1,fill="lightgrey") +
  annotate("text",x=ymd("2019-11-01"),y=10,label="+9m. Start Covid-19",angle = 90, size=bar_text_size) +

  scale_color_manual("",
                     values=c( "grey40"),
                     guide = "none") +
  
  scale_x_date(labels = date_format("%Y"), 
               breaks = date_breaks("10 year"))  +
  
  scale_y_continuous(
               breaks  = seq(0, 12,1))  +
  
  ggtitle(Title) +
  xlab("Year") +
  ylab("General fertility rate (GFR) per 1'000 females in the age 15–49 years") +
  
      theme_bw() +
  theme(
    axis.text = element_text(size=axis_text_size),
    axis.title  = element_text(size=axis_title_size),
    legend.position = "bottom",
    legend.text=element_text(size=legend_text_size),
    plot.title = element_text(size=plot_title_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())

plot_trend
  
  # cowplot::save_plot(paste0("output/plot_trend_",varBirth,".pdf"),   plot_trend ,base_height=10,base_width=20)
  
  ggsave(paste0("output/plot_trend_",varBirth,".png"),  plot_trend,h=10,w=20)
  
  
### Annual crude rate vs TFR
  
  dt_tfr <- read.csv("data/TFR.csv", sep=";") %>%
    select(Year, TFR)
  
  dat.y <- dat.trend %>%
    filter(Year %in% 1871:2022) %>%
    group_by(Year, denominator) %>%
    summarise(birth_y = sum(birth_var)) %>%
    mutate(birth_c = birth_y/denominator*1000) %>%
    left_join(dt_tfr )
  
  
  coeff <- 30
  
plot_tfr <- ggplot()+
  geom_line(data=dat.y, aes(x=Year, y=birth_c, col="GFR", linetype="GFR"),lwd=1.5) +
  geom_line(data=dat.y, aes(x=Year, y=TFR*coeff, col="TRF", linetype="TRF"),lwd=1.5) +
  scale_color_manual("",
                     values=c("grey10","grey50")) +
  scale_linetype_manual("",
                     values=c("solid","longdash")) +
  
  scale_y_continuous(name = "General fertility rate (GFR) per 1'000 females in the age 15–49 years",
                     sec.axis = sec_axis(~./coeff, name = "Total fertility rate (TFR)")) +
  scale_x_continuous(breaks  = seq(1871, 2022,10)) +
    
    ggtitle("Annual birth rate in Switzerland, 1871-2022") +
    xlab("Year") +
    ylab("Crude birth rate per 1'000 females in the age 15–49 years") +
    
    theme_bw() +
    theme(
      axis.text = element_text(size=axis_text_size),
      axis.title  = element_text(size=axis_title_size),
      legend.position = c(0.8,0.8),
      legend.key.size = unit(2, 'cm'),
      legend.text=element_text(size=legend_text_size),
      plot.title = element_text(size=plot_title_size),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank())
# 
# cowplot::save_plot("output/plot_com.pdf", plot_com ,base_height=10,base_width=20)

ggsave(paste0("output/plot_TFR.png"),  plot_tfr ,h=10,w=20)




