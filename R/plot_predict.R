
  load("data/predicted_birth_inla_monthtotal_birth_Geschlecht - Total.RData")
  #   
    dat.exp <-  fitted_birth %>%
      mutate(birth = ymd(paste0(Year,"-", Month,"-01")),
             birth_pre_inc = birth_pre/denominator*10000,
             birth_inc = birth_var/denominator*10000,
             fit_inc = fit/denominator*10000,
             LL_inc = LL/denominator *10000,
             UL_inc = UL/denominator*10000,
             excess_birth = birth_inc-fit_inc,
             rel_excess_birth = excess_birth/fit_inc*100,
             significant_dummy = ifelse(birth_inc > LL_inc & birth_inc  < UL_inc,"non-significant","significant"),
             significant_dummy = as.factor( significant_dummy)) %>%
      distinct(Year, Month, .keep_all = TRUE)
    
 

    plot_birth <- ggplot()+
      annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("2021-07-01"),xmax=ymd("2021-10-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("2022-08-01"),xmax=ymd("2022-12-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="turquoise2") +
      annotate("rect",xmin=ymd("2023-03-01"),xmax=ymd("2023-05-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
      annotate("rect",xmin=ymd("2022-04-01"),xmax=ymd("2022-06-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="darkgreen") +
      
      #Total birth women
      annotate("text",x=ymd("2021-01-01"),y=6,label="A", size=6) +
      annotate("text",x=ymd("2021-09-01"),y=6,label="B", size=6) +
      annotate("text",x=ymd("2022-10-01"),y=6,label="D", size=6) +
      annotate("text",x=ymd("2023-04-01"),y=6,label="E", size=6) +
      annotate("text",x=ymd("2022-05-01"),y=6,label="C", size=6) +

      
      geom_ribbon(data=dat.exp,aes(ymin=LL_inc, ymax=UL_inc,x=birth,fill="Interval"),linetype=1, alpha=1) +
      geom_line(data=dat.exp, aes(x=birth, y=birth_inc, col="births"),lwd=1.8) +
      geom_line(data=dat.exp, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
     

      scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 year")) +
      ggtitle("Monthly birth rate: Predicted vs. observed") +
      xlab("Year") +
      ylab("Births per 10'000 inhabitants")+
      scale_color_manual("",
                         breaks=c("births","fit"),
                         labels=c("Observed births","Predicted births (from 2021)"),
                         values=c("red",  "grey40"))+
      
      scale_fill_manual("",
                        breaks=c("Interval"),
                        labels=c("Interval of expected births"),
                        values=c( "grey90")) +
      theme_bw()+
      theme(
        axis.text.y = element_text(size=20),
        legend.position = "bottom",
        legend.text=element_text(size=16),
        axis.text.x = element_text(size=20),
        axis.title.x  = element_text(size=20),
        axis.title.y  = element_text(size=20),
        plot.title = element_text(size=20))
    
    
  cowplot::save_plot(paste0("output/plot_birth_predicted.pdf"),plot_birth ,base_height=8,base_width=15)
  

