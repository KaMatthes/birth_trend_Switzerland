function_plot_russian <- function(group_data, pop_group, Title) {
  

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
      filter(Year > 1884 & Year < 1896)
    
  
    plot_birth <- ggplot()+
      
      annotate("rect",xmin=ymd("1890-08-01"),xmax=ymd("1890-11-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
      annotate("text",x=ymd("1890-09-15"),y=28,label="\"Russian flu\"",angle = 90, size=6) +
      
      geom_ribbon(data=dat.exp,aes(ymin=LL_inc, ymax=UL_inc,x=birth,fill="Interval"),linetype=1, alpha=0.5) +
      # geom_line(data=dat.exp,aes(x=birth, y=LL_inc, col="Interval"),linetype=1, alpha=0.3) +
      # geom_line(data=dat.exp,aes(x=birth, y=UL_inc, col="Interval"),linetype=1, alpha=0.3) +
      geom_line(data=dat.exp, aes(x=birth, y=birth_inc, col="births"),lwd=1.8) +
      geom_line(data=dat.exp, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
    
      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("1885-01-01")), max(ymd("1895-01-01")))) +
      # coord_cartesian(ylim=c(0, 50)) +
      ggtitle(Title) +
      # xlim(1910, 1968) +
      xlab("Year") +
      # ylab("Births per 10'000 inhabitants")+
      ylab("Births per 10'000 inhabitants")+
      scale_color_manual("",
                         breaks=c("births","fit"),
                         labels=c("Observed births", "Expected births" ),
                         values=c("red", "grey40"))+
      
      scale_fill_manual("",
                        breaks=c("Interval"),
                        labels=c("Interval of expected birth"),
                        values=c( "grey")) +
      theme_bw() +
      theme(
        axis.text.y = element_text(size=20),
        # legend.position = c(0.2,0.2),
        legend.position = "bottom",
        legend.text=element_text(size=16),
        # legend.key.size = unit(3.5, 'cm'),
        # legend.spacing.x = unit(3.5, 'cm'),
        axis.text.x = element_text(size=20),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=20),
        plot.title = element_text(size=20))
    
    
    plot_excess <- ggplot() +
      annotate("rect",xmin=ymd("1890-08-01"),xmax=ymd("1890-11-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
      geom_col(data= dat.exp,aes(x= birth,y =  rel_excess_birth/100, fill=significant_dummy)) +
      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("1888-01-01")), max(ymd("1893-01-01")))) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual("",
                        breaks=c("significant","non-significant"),
                        values =c("red","grey")) +
      xlab("Year")+
      ylab("Relatitve differences")+
      # ggtitle(Title) +
      theme_bw()+
      #theme_light(base_size = 16)+
      theme_bw() +
      theme(
        axis.text.y = element_text(size=20),
        # legend.position = c(0.1,0.1),
        legend.position = "bottom",
        legend.text=element_text(size=16),
        # legend.key.size = unit(3.5, 'cm'),
        # legend.spacing.x = unit(3.5, 'cm'),
        axis.text.x = element_text(size=20),
        axis.title.x  = element_text(size=20),
        axis.title.y  = element_text(size=20),
        plot.title = element_text(size=20))

    
    plot_together <- cowplot::plot_grid(plot_birth,plot_excess,
                                        ncol=1, nrow=2,rel_heights = c(1,.7), align="hv")

  
  cowplot::save_plot(paste0("output/plot_birth_1890_",group_data,".pdf"),plot_together ,base_height=12,base_width=15)
  
}

function_plot_russian(group_data="total_birth",pop_group="pop",Title="Total births")


