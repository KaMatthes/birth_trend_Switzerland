
dt <-  read_rds("data/expected_birth_inla_month_total_birth_female_2021.rds") %>%
    mutate(birth = ymd(paste0(Year,"-", Month,"-01")),
           birth_inc = birth_var/denominator*1000,
           fit_inc = fit/denominator*1000,
           LL_inc = LL/denominator *1000,
           UL_inc = UL/denominator*1000,
           excess_birth = birth_var-fit,
           rel_excess_birth = excess_birth/fit*100,
           significant_dummy = ifelse(birth_inc > LL_inc & birth_inc  < UL_inc,"no differences","excess and lower births"),
           significant_dummy = as.factor( significant_dummy)) %>%
    distinct(Year, Month, .keep_all = TRUE) %>%
  filter(Year %in% 2013:2023)
  
plot_birth <- ggplot()+
  annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
  annotate("rect",xmin=ymd("2021-07-01"),xmax=ymd("2021-10-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
  annotate("rect",xmin=ymd("2022-08-01"),xmax=ymd("2022-12-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
  annotate("rect",xmin=ymd("2023-03-01"),xmax=ymd("2023-05-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=ymd("2022-04-01"),xmax=ymd("2022-06-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="darkgreen") +
  geom_ribbon(data=dt,aes(ymin=LL_inc, ymax=UL_inc,x=birth,fill="Interval"),linetype=1, alpha=1) +
  geom_line(data=dt, aes(x=birth, y=birth_inc, col="births"),lwd=1.8) +
  geom_line(data=dt, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
  annotate("text",x=ymd("2021-01-01"),y=2,label="A", size=bar_text_size) +
  annotate("text",x=ymd("2021-09-01"),y=2,label="B", size=bar_text_size) +
  annotate("text",x=ymd("2022-10-01"),y=2,label="D", size=bar_text_size) +
  annotate("text",x=ymd("2023-04-01"),y=2,label="E", size=bar_text_size) +
  annotate("text",x=ymd("2022-05-01"),y=2,label="C", size=bar_text_size) +

      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("2013-01-01")), max(ymd("2024-01-01")))) +
  scale_y_continuous(breaks  = seq(2, 5,1))  +
  ylim(c(2,5))+
      ggtitle("Monthly birth rate: Recent trends") +
      xlab("Year") +
  ylab("Crude birth rate \n per 1'000 females in the age 15–49 years") +
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
    axis.text = element_text(size=axis_text_size),
    axis.title  = element_text(size=axis_title_size),
    legend.position = "bottom",
    legend.text=element_text(size=legend_text_size),
    plot.title = element_text(size=plot_title_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())
    
    
    
    plot_excess <- ggplot() +
      annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("2021-07-01"),xmax=ymd("2021-10-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("2022-08-01"),xmax=ymd("2022-12-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("2023-03-01"),xmax=ymd("2023-05-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
      annotate("rect",xmin=ymd("2022-04-01"),xmax=ymd("2022-06-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="darkgreen") +
      geom_col(data= dt,aes(x= birth,y =  rel_excess_birth/100, fill=significant_dummy)) +
      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("2013-01-01")), max(ymd("2024-01-01")))) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual("",
                        breaks=c("excess and lower births","no differences"),
                        values =c("red","grey")) +
      xlab("Year")+
      ylab("Relatitve differences")+
      theme_bw()+
      theme(
        axis.text = element_text(size=axis_text_size),
        axis.title  = element_text(size=axis_title_size),
        legend.position = "bottom",
        legend.text=element_text(size=legend_text_size),
        plot.title = element_text(size=plot_title_size),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
    
    
    plot_together <- cowplot::plot_grid(plot_birth,plot_excess,
                                        ncol=1, nrow=2,rel_heights = c(1,1), align="hv")
  
  cowplot::save_plot(paste0("output/plot_birth_2020.pdf"),   plot_together ,base_height=15,base_width=15)
  

