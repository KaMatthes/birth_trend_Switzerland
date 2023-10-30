function_plot_expected <- function(Sex, Title) {
  
  if(Sex=="Total"){
load("data/expected_birth_inla_month_Geschlecht - Total.RData")

}

else if(Sex=="Frau"){
load("data/expected_birth_inla_month_Frau.RData")

}

dat.exp <- expected_birth %>%
  mutate(count_inc = count.y/pop.monthly.y*10000,
         LL_inc = LL/pop.monthly.y *10000,
         UL_inc = UL/pop.monthly.y*10000)

plot_birth <- ggplot()+
  geom_ribbon(data=dat.exp,aes(ymin=LL_inc, ymax=UL_inc,x=birth, col="Interval expected"),linetype=1, alpha=0.3) +
  geom_line(data=dat.exp, aes(x=birth, y=count_inc, col="births"),lwd=1) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 year")) +
  # coord_cartesian(ylim=c(0, 50)) +
  ggtitle(Title) +
  # xlim(1910, 1968) +
  xlab("Year") +
  ylab("Births per 10'000 inhabitants")+
  scale_color_manual("",
                     values=c( "red","grey"))+
  theme_bw() +
  theme(
    axis.text.y = element_text(size=20),
    legend.position = c(0.1,0.2),
    legend.text=element_text(size=20),
    # legend.key.size = unit(3.5, 'cm'),
    # legend.spacing.x = unit(3.5, 'cm'),
    axis.text.x = element_text(size=20),
    axis.title.x  = element_text(size=20),
    axis.title.y  = element_text(size=20),
    plot.title = element_text(size=25))



cowplot::save_plot(paste0("output/plot_birth_",Sex,".pdf"), plot_birth ,base_height=7,base_width=15)

}

function_plot_expected(Sex="Total",Title="Total Population")
function_plot_expected(Sex="Frau", Title="Women 15-49 years")
