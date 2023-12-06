
load("data/expected_birth_inla_month_total_birth_Geschlecht - Total.RData")
  
 dat.exp <- expected_birth %>%
    mutate(birth = ymd(paste0(Year,"-", Month,"-01")),
           birth_inc = birth_var/denominator*10000,
           fit_inc = fit/denominator*10000,
           LL_inc = LL/denominator *10000,
           UL_inc = UL/denominator*10000,
           excess_birth = birth_var-fit,
           rel_excess_birth = excess_birth/fit*100,
           significant_dummy = ifelse(birth_inc > LL_inc & birth_inc  < UL_inc,"non-significant","significant"),
           significant_dummy = as.factor( significant_dummy)) %>%
    filter(Year >2015)  %>%
    distinct(Year, Month, .keep_all = TRUE)
  
plot_birth <- ggplot()+
  annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
  annotate("rect",xmin=ymd("2021-07-01"),xmax=ymd("2021-10-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
  annotate("rect",xmin=ymd("2022-08-01"),xmax=ymd("2022-12-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
  annotate("rect",xmin=ymd("2023-03-01"),xmax=ymd("2023-05-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=ymd("2022-04-01"),xmax=ymd("2022-06-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="darkgreen") +
  geom_ribbon(data=dat.exp,aes(ymin=LL_inc, ymax=UL_inc,x=birth,fill="Interval"),linetype=1, alpha=1) +
  geom_line(data=dat.exp, aes(x=birth, y=birth_inc, col="births"),lwd=1.8) +
  geom_line(data=dat.exp, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
  annotate("text",x=ymd("2021-01-01"),y=6,label="A", size=6) +
  annotate("text",x=ymd("2021-09-01"),y=6,label="B", size=6) +
  annotate("text",x=ymd("2022-10-01"),y=6,label="D", size=6) +
  annotate("text",x=ymd("2023-04-01"),y=6,label="E", size=6) +
  annotate("text",x=ymd("2022-05-01"),y=6,label="C", size=6) +

      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("2016-01-01")), max(ymd("2024-01-01")))) +
      ggtitle("Monthly birth rate: Recent trends") +
      xlab("Year") +
      ylab("Births per 10'000 inhabitants")+
      scale_color_manual("",
                         breaks=c("births","fit"),
                         labels=c("Observed births", "Expected births" ),
                         values=c("red", "grey40"))+
      
      scale_fill_manual("",
                        breaks=c("Interval"),
                        labels=c("Interval of expected births"),
                        values=c( "grey90")) +
      theme_bw() +
      theme(
        axis.text.y = element_text(size=20),
        legend.position = "bottom",
        legend.text=element_text(size=16),
        axis.text.x = element_text(size=20),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=20),
        plot.title = element_text(size=20),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
    
    
    
    plot_excess <- ggplot() +
      annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("2021-07-01"),xmax=ymd("2021-10-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("2022-08-01"),xmax=ymd("2022-12-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("2023-03-01"),xmax=ymd("2023-05-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
      annotate("rect",xmin=ymd("2022-04-01"),xmax=ymd("2022-06-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="darkgreen") +
      geom_col(data= dat.exp,aes(x= birth,y =  rel_excess_birth/100, fill=significant_dummy)) +
      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("2016-01-01")), max(ymd("2024-01-01")))) +
      scale_y_continuous(labels = scales::percent, limits = c(-0.15,0.15)) +
      scale_fill_manual("",
                        breaks=c("significant","non-significant"),
                        values =c("red","grey")) +
      xlab("Year")+
      ylab("Relatitve differences")+
      theme_bw()+
      theme(
        axis.text.y = element_text(size=20),
        legend.position = "bottom",
        legend.text=element_text(size=16),
        axis.text.x = element_text(size=20),
        axis.title.x  = element_text(size=20),
        axis.title.y  = element_text(size=20),
        plot.title = element_text(size=20),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
    
    plot_together <- cowplot::plot_grid(plot_birth,plot_excess,
                                        ncol=1, nrow=2,rel_heights = c(1,.7), align="hv")
    
  
  cowplot::save_plot(paste0("output/plot_birth_2020.pdf"),   plot_together ,base_height=12,base_width=15)
  

