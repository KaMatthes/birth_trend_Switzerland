
dt <-  read_rds("data/expected_birth_inla_month_total_birth_female_1890.rds") %>%
      mutate(birth = ymd(paste0(Year,"-", Month,"-01")),
             birth_inc = birth_var/denominator*1000,
             fit_inc = fit/denominator*1000,
             LL_inc = LL/denominator *1000,
             UL_inc = UL/denominator*1000,
             excess_birth = birth_var-fit,
             rel_excess_birth = excess_birth/fit*100,
             significant_dummy = ifelse(birth_inc > LL_inc & birth_inc  < UL_inc,"no differences","excess and deficits births"),
             significant_dummy = as.factor( significant_dummy)) %>%
  filter(Year %in% 1885:1895)
  
    plot_birth <- ggplot()+
      
      annotate("rect",xmin=ymd("1890-08-01"),xmax=ymd("1890-11-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("text",x=ymd("1890-09-15"),y=6.5,label="+9m flu",angle = 90, size=bar_text_size) +
      
      geom_ribbon(data=dt,aes(ymin=LL_inc, ymax=UL_inc,x=birth,fill="Interval"),linetype=1, alpha=1) +
      geom_line(data=dt, aes(x=birth, y=birth_inc, col="births"),lwd=1.8) +
      geom_line(data=dt, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
    
      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("1885-01-01")), max(ymd("1895-01-01")))) +
      scale_y_continuous(
        breaks  = seq(6, 11,1))  +
      ylim(c(6,11))+
      
      ggtitle("Monthly GFR & the 1890 flu") +
      xlab("Year") +
      ylab("General fertility rate (GFR) \n per 1'000 females in the age 15–49 years") +
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
      annotate("rect",xmin=ymd("1890-08-01"),xmax=ymd("1890-11-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      geom_col(data= dt,aes(x= birth,y =  rel_excess_birth/100, fill=significant_dummy)) +
      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("1885-01-01")), max(ymd("1895-01-01")))) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual("",
                        breaks=c("excess and deficits births","no differences"),
                        values =c("red","grey")) +
      xlab("Year")+
      ylab("Relatitve differences")+
      theme_bw() +
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

  
  # cowplot::save_plot(paste0("output/plot_birth_1890.pdf"),plot_together ,base_height=15,base_width=15)
  
  ggsave(paste0("output/plot_birth_1890.png"),     plot_together ,h=15,w=15)

