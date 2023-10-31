function_plot_expected <- function(group_data, pop_group, Title) {
  

    # load("data/predicted_birth_inla_monthtotal_birth_Geschlecht - Total.RData")
  
  
  load("data/predicted_birth_inla_monthtotal_birth_Frau.RData")
    
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
             significant_dummy = as.factor( significant_dummy))
    
 

    plot_birth <- ggplot()+
      geom_ribbon(data=dat.exp,aes(ymin=LL_inc, ymax=UL_inc,x=birth,fill="Interval"),linetype=1, alpha=0.3) +
      # geom_line(data=dat.exp,aes(x=birth, y=LL_inc, col="Interval"),linetype=1, alpha=0.3) +
      # geom_line(data=dat.exp,aes(x=birth, y=UL_inc, col="Interval"),linetype=1, alpha=0.3) +
      geom_line(data=dat.exp, aes(x=birth, y=birth_inc, col="births"),lwd=1) +
      geom_line(data=dat.exp, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
      scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 year")) +
      # coord_cartesian(ylim=c(0, 50)) +
      ggtitle(Title) +
      # xlim(1910, 1968) +
      xlab("Year") +
      # ylab("Births per 10'000 inhabitants")+
      ylab("Births per 10'000 inhabitants")+
      scale_color_manual("",
                         breaks=c("fit","births"),
                         labels=c("Predicted birth from 2021", "Observed Birth"),
                         values=c( "grey40","red", "grey"))+
      
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
        axis.title.x  = element_text(size=20),
        axis.title.y  = element_text(size=20),
        plot.title = element_text(size=25))
    
    # plot_excess <- ggplot() +
    #   geom_col(data= dat.exp,aes(x= birth,y =  rel_excess_birth/100, fill=significant_dummy)) +
    #   scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 year")) +
    #   scale_y_continuous(labels = scales::percent) +
    #   scale_fill_manual("",
    #                     breaks=c("non-significant","significant"),
    #                     values =c("grey","red")) +
    #   xlab("Year")+
    #   ylab("Relatitve differences")+
    #   # ggtitle(Title) +
    #   theme_bw()+
    #   #theme_light(base_size = 16)+
    #   theme_bw() +
    #   theme(
    #     axis.text.y = element_text(size=20),
    #     # legend.position = c(0.1,0.1),
    #     legend.position = "bottom",
    #     legend.text=element_text(size=16),
    #     # legend.key.size = unit(3.5, 'cm'),
    #     # legend.spacing.x = unit(3.5, 'cm'),
    #     axis.text.x = element_text(size=20),
    #     axis.title.x  = element_text(size=20),
    #     axis.title.y  = element_text(size=20),
    #     plot.title = element_text(size=20))
    # 
    # plot_together <- cowplot::plot_grid(plot_birth,plot_excess,
    #                                     ncol=1, nrow=2, align="hv")
    # 

  
  cowplot::save_plot(paste0("output/plot_birth_predicted_pop_15_49.pdf"),plot_birth ,base_height=10,base_width=15)
  
}

# function_plot_expected(group_data="parity_1",pop_group="total",Title="Parity = 1")
# function_plot_expected(group_data="parity_sup_1",pop_group="total",Title="Parity > 1")
# 
# function_plot_expected(group_data="single_birth",pop_group="total",Title="Single birth")
# function_plot_expected(group_data="multiple_birth",pop_group="total",Title="Multiple birth")
# 
# function_plot_expected(group_data="females",pop_group="total",Title="Females")
# function_plot_expected(group_data="males",pop_group="total",Title="Males")

function_plot_expected(group_data="total_birth",pop_group="pop",Title="Total population")

# 
# function_plot_expected(group_data="mat_age_below_30",pop_group="pop",Title="Birth age < 30 (population women aged 15-29)")
# function_plot_expected(group_data="mat_age_above_or_eq_30",pop_group="pop",Title=" Birth >=30 (population women aged 30-49)")
# 
# 
# function_plot_expected(group_data="german_romansh",pop_group="pop",Title="German")
# function_plot_expected(group_data="french",pop_group="pop",Title="French")
# function_plot_expected(group_data="italy",pop_group="pop",Title="Italian")
# 
# function_plot_expected(group_data="swiss",pop_group="pop",Title="Swiss")
# function_plot_expected(group_data="non_swiss",pop_group="pop",Title="Non Swiss")
