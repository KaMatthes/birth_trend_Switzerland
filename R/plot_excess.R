function_plot_expected <- function(Sex, Title) {
  
  if(Sex=="Total"){
 # load("data/expected_birth_freq_month_Geschlecht - Total.RData")
  # dat_f <- expected_birth
  load("data/expected_birth_inla_month_Geschlecht - Total.RData")
  #   dat_b <- expected_birth

}

else if(Sex=="Frau"){
load("data/expected_birth_inla_month_Frau.RData")

}

# dat.exp <- expected_birth %>%
#   mutate(count_inc = count/pop.monthly*10000,
#          lower_inc = lower/pop.monthly *10000,
#          upper_inc = upper/pop.monthly*10000,
#          excess_birth = count-pred,
#          rel_excess_birth = excess_birth/pred*100,
#          significant_dummy = ifelse(count > lower & count < upper,0,1),
#          significant_dummy = as.factor( significant_dummy),
#          Difference_sig =  ifelse( excess_birth > 0, "More than expected", "Fewer than expected"),
#          Difference_sig= replace( Difference_sig, significant_dummy==1 & Difference_sig=="More than expected", "Significant more")) %>%
#   filter(Year > 2016)
# 


dat.exp <- expected_birth %>%
  mutate(count_inc = count.y/pop.monthly.y*10000,
         lower_inc = LL/pop.monthly.y *10000,
         upper_inc = UL/pop.monthly.y*10000,
         fit_inc = fit/pop.monthly.y*10000,
         excess_birth = count.y-fit,
         rel_excess_birth = excess_birth/fit*100,
         significant_dummy = ifelse(count_inc > lower_inc & count_inc  < upper_inc,"non-significant","significant"),
         significant_dummy = as.factor( significant_dummy)) %>%
  filter(Year>2017)
         # Difference_sig =  ifelse(excess_birth > 0, "More than expected", "Fewer than expected"),
         # Difference_sig= replace( Difference_sig, significant_dummy==1 & Difference_sig=="More than expected", "Significant more") %>%
  # filter(Year > 2016)


plot_birth <- ggplot()+
  geom_ribbon(data=dat.exp,aes(ymin=lower_inc, ymax=upper_inc,x=birth, col="Interval"),linetype=1, alpha=0.2) +
  geom_line(data=dat.exp, aes(x=birth, y=count_inc, col="births"),lwd=1) +
  geom_line(data=dat.exp, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 year")) +
  # coord_cartesian(ylim=c(0, 50)) +
  ggtitle(Title) +
  xlab("Year") +
  ylab("Births per 10'000 inhabitants")+
  scale_color_manual("",
                     breaks=c("fit","births","Interval"),
                     labels=c("Expected birth", "Observed Birth","Interval of expected birth"),
                     values=c( "grey40","red", "grey"))+
  theme_bw() +
  theme(
    axis.text.y = element_text(size=20),
    legend.position = c(0.1,0.1),
    legend.text=element_text(size=20),
    # legend.key.size = unit(0.5, 'cm'),
    # legend.spacing.x = unit(3.5, 'cm'),
    axis.text.x = element_text(size=20),
    axis.title.x  = element_text(size=20),
    axis.title.y  = element_text(size=20),
    plot.title = element_text(size=20))



plot_excess <- ggplot() +
  geom_col(data= dat.exp,aes(x= birth,y =  rel_excess_birth/100, fill=significant_dummy)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 year")) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 year")) +
  scale_fill_manual("",
                    breaks=c("non-significant","significant"),
                    values =c("grey","red")) +
  xlab("Year")+
  ylab("Relatitve differences")+
  ggtitle(Title) +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme_bw() +
  theme(
    axis.text.y = element_text(size=20),
    legend.position = c(0.1,0.1),
    legend.text=element_text(size=20),
    # legend.key.size = unit(3.5, 'cm'),
    # legend.spacing.x = unit(3.5, 'cm'),
    axis.text.x = element_text(size=20),
    axis.title.x  = element_text(size=20),
    axis.title.y  = element_text(size=20),
    plot.title = element_text(size=20))


plot_together <- cowplot::plot_grid(plot_birth,plot_excess,
                                    ncol=1, nrow=2, align="hv")

cowplot::save_plot(paste0("output/plot_birth_2018_",Sex,".pdf"), plot_together ,base_height=12,base_width=20)

}

function_plot_expected(Sex="Total",Title="2018 -Total Population")
function_plot_expected(Sex="Frau", Title="2018 - Population Women 15-49 years")
