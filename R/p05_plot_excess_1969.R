dt <-  read_rds("data/expected_birth_inla_month_total_birth_female_1970.rds") %>%
      mutate(birth = ymd(paste0(Year,"-", Month,"-01")),
             birth_inc = birth_var/denominator*1000,
             fit_inc = fit/denominator*1000,
             LL_inc = LL/denominator *1000,
             UL_inc = UL/denominator*1000,
             excess_birth = birth_var-fit,
             rel_excess_birth = excess_birth/fit*100,
             significant_dummy = ifelse(birth_inc > LL_inc & birth_inc  < UL_inc,"No differences","Excess and deficit births"),
             significant_dummy = as.factor( significant_dummy),
             birth = birth +15) %>%
  filter(Year %in% 1964:1974)
     
    
    plot_birth <- ggplot()+
      annotate("rect",xmin=ymd("1969-11-15"),xmax=ymd("1970-01-15"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("1970-09-15"),xmax=ymd("1970-11-15"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("text",x=ymd("1969-12-15"),y=7,label="+9m. flu wave 1",angle = 90, size=bar_text_size,family = "serif") +
      annotate("text",x=ymd("1970-10-15"),y=7,label="+9m. flu wave 2",angle = 90, size=bar_text_size,family = "serif") +
      
      geom_ribbon(data=dt,aes(ymin=LL_inc, ymax=UL_inc,x=birth,fill="Interval"),linetype=1, alpha=1) +
      geom_line(data=dt, aes(x=birth, y=birth_inc, col="births"),lwd=1.8) +
      geom_line(data=dt, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
      scale_y_continuous(breaks  = seq(4, 8,1))  +
      ylim(c(2,8))+
      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("1964-01-01")), max(ymd("1974-01-01")))) +
      ggtitle("(a) Expected and observed GFR") +
      xlab("Year") +
      ylab("GFR per 1,000 women aged 15–49") +
      scale_color_manual("",
                         breaks=c("births","fit"),
                         labels=c("Observed births", "Expected births" ),
                         values=c("red", "grey40"))+
      scale_fill_manual("",
                        breaks=c("Interval"),
                        labels=c("95 per cent CrI of expected births"),
                        values=c( "grey90")) +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +      
      theme_bw() +
      theme(
        text = element_text(family = "serif"),
        axis.text = element_text(size=axis_text_size),
        axis.title  = element_text(size=axis_title_size),
        legend.position = "bottom",
        legend.text=element_text(size=legend_text_size),
        plot.title = element_text(size=plot_title_size),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
    
    plot_excess <- ggplot() +
      annotate("rect",xmin=ymd("1969-11-15"),xmax=ymd("1970-01-15"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("1970-09-15"),xmax=ymd("1970-11-15"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      geom_col(data= dt,aes(x= birth,y =  rel_excess_birth, fill=significant_dummy)) +
      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("1964-01-01")), max(ymd("1974-01-01")))) +
      # scale_y_continuous(labels = scales::percent) +
      scale_fill_manual("",
                        breaks=c("Excess and deficit births","No differences"),
                        values =c("red","grey")) +
      xlab("Year")+
      ylab("Relative differences (percentages)")+
      ggtitle("(b) Relative excess and deficit GFR") +
      theme_bw() +
      theme(
        text = element_text(family = "serif"),
        axis.text = element_text(size=axis_text_size),
        axis.title  = element_text(size=axis_title_size),
        legend.position = "bottom",
        legend.text=element_text(size=legend_text_size),
        plot.title = element_text(size=plot_title_size),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

    
    plot_together <- cowplot::plot_grid(plot_birth,plot_excess,
                                        ncol=1, nrow=2,rel_heights = c(1,1), align="hv")

  
  cowplot::save_plot(paste0("output/plot_birth_1969.pdf"),plot_together ,base_height=15,base_width=15)
  
  ggsave(paste0("output/plot_birth_1969.png"),     plot_together ,h=15,w=15,dpi = 1000)