
load("data/expected_conception_inla_month.RData")
dat.conception <- expected_birth %>%
  mutate(birth = ymd(paste0(Year,"-", Month,"-01")),
         birth_inc = birth_var/denominator*10000,
         fit_inc = fit/denominator*10000,
         LL_inc = LL/denominator *10000,
         UL_inc = UL/denominator*10000,
         excess_birth = birth_var-fit,
         rel_excess_birth = excess_birth/fit*100,
         significant_dummy = ifelse(birth_inc > LL_inc & birth_inc  < UL_inc,"non-significant","significant"),
         significant_dummy = as.factor( significant_dummy)) %>%
  filter(Year >2015)


load("data/expected_birth_inla_month_total_birth_Geschlecht - Total.RData")
dat.birth <-expected_birth %>%
  mutate(birth = ymd(paste0(Year,"-", Month,"-01")),
         birth_inc = birth_var/denominator*10000,
         fit_inc = fit/denominator*10000,
         LL_inc = LL/denominator *10000,
         UL_inc = UL/denominator*10000,
         excess_birth = birth_var-fit,
         rel_excess_birth = excess_birth/fit*100,
         significant_dummy = ifelse(birth_inc > LL_inc & birth_inc  < UL_inc,"non-significant","significant"),
         significant_dummy = as.factor( significant_dummy)) %>%
  filter(Year >2015)
  

plot_conception <- ggplot()+
  geom_ribbon(data=dat.conception,aes(ymin=LL_inc, ymax=UL_inc,x=birth,fill="Interval"),linetype=1, alpha=0.5) +
  geom_line(data=dat.conception, aes(x=birth, y=birth_inc, col="births"),lwd=1.8) +
  geom_line(data=dat.conception, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
  annotate("rect",xmin=ymd("2020-03-01"),xmax=ymd("2020-04-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  annotate("rect",xmin=ymd("2020-10-01"),xmax=ymd("2020-12-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  annotate("rect",xmin=ymd("2021-12-01"),xmax=ymd("2022-05-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  
  annotate("text",x=ymd("2020-04-01"),y=7,label="first wave",angle = 90, size=4) +
  annotate("text",x=ymd("2020-11-01"),y=7,label="second wave",angle = 90, size=4) +
  annotate("text",x=ymd("2022-02-01"),y=6.9,label="Omicron wave",angle = 90, size=4) +
  scale_x_date(labels = date_format("%Y"), 
               breaks = date_breaks("1 year"),
               limits =c(min(ymd("2016-01-01")), max(ymd("2024-01-01")))) +
  ggtitle("Conception") +
  xlab("Year") +
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
    legend.position = "bottom",
    legend.text=element_text(size=16),
    axis.text.x = element_text(size=20),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=20),
    plot.title = element_text(size=20))


plot_excess_conception <- ggplot() +
  annotate("rect",xmin=ymd("2020-03-01"),xmax=ymd("2020-04-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  annotate("rect",xmin=ymd("2020-10-01"),xmax=ymd("2020-12-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  annotate("rect",xmin=ymd("2021-12-01"),xmax=ymd("2022-05-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  geom_col(data= dat.conception,aes(x= birth,y =  rel_excess_birth/100, fill=significant_dummy)) +
  scale_x_date(labels = date_format("%Y"), 
               breaks = date_breaks("1 year"),
               limits =c(min(ymd("2016-01-01")), max(ymd("2024-01-01")))) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual("",
                    breaks=c("significant","non-significant"),
                    values =c("red","grey")) +
  xlab("Year")+
  ylab("Relatitve differences")+
  ggtitle("Conception") +
  theme_bw()+
  theme(
    axis.text.y = element_text(size=20),
    legend.position = "bottom",
    legend.text=element_text(size=16),
    axis.text.x = element_text(size=20),
    axis.title.x  = element_text(size=20),
    axis.title.y  = element_text(size=20),
    plot.title = element_text(size=20))

plot_birth <- ggplot()+
  geom_ribbon(data=dat.birth,aes(ymin=LL_inc, ymax=UL_inc,x=birth,fill="Interval"),linetype=1, alpha=0.5) +
  geom_line(data=dat.birth, aes(x=birth, y=birth_inc, col="births"),lwd=1.8) +
  geom_line(data=dat.birth, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
  annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-01-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  annotate("rect",xmin=ymd("2021-07-01"),xmax=ymd("2021-09-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  annotate("rect",xmin=ymd("2022-09-01"),xmax=ymd("2023-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("2016-01-01")), max(ymd("2024-01-01")))) +
  ggtitle("Birth") +
      xlab("Year") +
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
        legend.position = "bottom",
        legend.text=element_text(size=16),
        axis.text.x = element_text(size=20),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=20),
        plot.title = element_text(size=20))

plot_excess_birth <- ggplot() +
  annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-01-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  annotate("rect",xmin=ymd("2021-07-01"),xmax=ymd("2021-09-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  annotate("rect",xmin=ymd("2022-09-01"),xmax=ymd("2023-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  geom_col(data= dat.birth,aes(x= birth,y =  rel_excess_birth/100, fill=significant_dummy)) +
  scale_x_date(labels = date_format("%Y"), 
               breaks = date_breaks("1 year"),
               limits =c(min(ymd("2016-01-01")), max(ymd("2024-01-01")))) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual("",
                    breaks=c("significant","non-significant"),
                    values =c("red","grey")) +
  xlab("Year")+
  ylab("Relatitve differences")+
  ggtitle("Birth") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size=20),
    legend.position = "bottom",
    legend.text=element_text(size=16),
    axis.text.x = element_text(size=20),
    axis.title.x  = element_text(size=20),
    axis.title.y  = element_text(size=20),
    plot.title = element_text(size=20))




plot_together <- cowplot::plot_grid(plot_conception, plot_birth,
                                    plot_excess_conception,plot_excess_birth,
                                    ncol=1, nrow=4,rel_heights = c(1,1,1,1), align="hv")


cowplot::save_plot(paste0("output/plot_conception.pdf"),plot_together ,base_height=20,base_width=15)

 