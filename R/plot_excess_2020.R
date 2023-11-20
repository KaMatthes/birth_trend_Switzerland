function_plot_expected <- function(group_data, Title) {
  
  if(group_data=="parity_1"){
    load("data/expected_birth_inla_month_parity_1_Geschlecht - Total.RData")
    }
  
  else if(group_data=="parity_2"){
    load("data/expected_birth_inla_month_parity_2_Geschlecht - Total.RData")
  }
  
  else if(group_data=="parity_3"){
    load("data/expected_birth_inla_month_parity_3_Geschlecht - Total.RData")
  }
  
  else if(group_data=="parity_sup_3"){
    load("data/expected_birth_inla_month_parity_sup_3_Geschlecht - Total.RData")
  }
 

  # else if(group_data=="females"){
  #   load("data/expected_birth_inla_month_females_Geschlecht - Total.RData")
  #   }
  # 
  # else if(group_data=="males"){
  #   load("data/expected_birth_inla_month_males_Geschlecht - Total.RData")
  #   }
  
  else if(group_data=="total_birth"){
    load("data/expected_birth_inla_month_total_birth_Geschlecht - Total.RData")
  }
  
  else if(group_data=="total_birth_females"){
    load("data/expected_birth_inla_month_total_birth_Frau.RData")
    }

  else if(group_data=="mat_age_below_30"){
    load("data/expected_birth_inla_month_mat_age_below_30_Geschlecht - Total.RData")
  }
  
  else if(group_data=="mat_age_above_or_eq_30"){
    load("data/expected_birth_inla_month_mat_age_above_or_eq_30_Geschlecht - Total.RData")
  }
  
  
  else if(group_data=="mat_age_below_30_pop_women_15_29"){
    load("data/expected_birth_inla_month_mat_age_below_30_Frau.RData")
  }
  
  else if(group_data=="mat_age_above_or_eq_30_pop_women_30_49"){
    load("data/expected_birth_inla_month_mat_age_above_or_eq_30_Frau.RData")
  }
  
#   else if(group_data=="german_romansh"){
#     load("data/expected_birth_inla_month_german_romansh_Geschlecht - Total.RData")
# }
#   
#   else if(group_data=="french"){
#     load("data/expected_birth_inla_month_french_Geschlecht - Total.RData")
# 
#     
#   }
#   
#   else if(group_data=="italy"){
#     load("data/expected_birth_inla_month_italy_Geschlecht - Total.RData")
#   
#   }
#   
  else if(group_data=="swiss"){
    load("data/expected_birth_inla_month_swiss_Geschlecht - Total.RData")


  }

  else if(group_data=="non_swiss"){
    load("data/expected_birth_inla_month_non_swiss_Geschlecht - Total.RData")

  }

  
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
  geom_ribbon(data=dat.exp,aes(ymin=LL_inc, ymax=UL_inc,x=birth,fill="Interval"),linetype=1, alpha=0.5) +
      # geom_line(data=dat.exp,aes(x=birth, y=LL_inc, col="Interval"),linetype=1, alpha=0.3) +
      # geom_line(data=dat.exp,aes(x=birth, y=UL_inc, col="Interval"),linetype=1, alpha=0.3) +
  geom_line(data=dat.exp, aes(x=birth, y=birth_inc, col="births"),lwd=1.8) +
  geom_line(data=dat.exp, aes(x=birth, y=fit_inc, col="fit"),lwd=1) +
  annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  annotate("rect",xmin=ymd("2021-07-01"),xmax=ymd("2021-09-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
  annotate("rect",xmin=ymd("2022-09-01"),xmax=ymd("2023-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
 
  # # Total birth
  # annotate("text",x=ymd("2021-01-01"),y=7,label="Covid-19 - first wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2021-08-01"),y=7,label="Covid-19 - second wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2022-12-01"),y=6.9,label="Covid-19 - Omicron wave",angle = 90, size=4) +

  # Total birth women
  # annotate("text",x=ymd("2021-01-01"),y=35,label="Covid-19 - first wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2021-08-01"),y=35,label="Covid-19 - second wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2022-12-01"),y=35,label="Covid-19 - Omicron wave",angle = 90, size=4) +
  
  
  ## aparity == 1
  # annotate("text",x=ymd("2021-01-01"),y=3.5,label="Covid-19 - first wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2021-08-01"),y=3.5,label="Covid-19 - second wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2022-12-01"),y=3.5,label="Covid-19 - Omicron wave",angle = 90, size=4) +
  # 
  ## aparity == 2
  # annotate("text",x=ymd("2021-01-01"),y=0.9,label="Covid-19 - first wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2021-08-01"),y=0.9,label="Covid-19 - second wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2022-12-01"),y=0.9,label="Covid-19 - Omicron wave",angle = 90, size=4) +
  # 
  
  # # age < 30
  # annotate("text",x=ymd("2021-01-01"),y=2.2,label="Covid-19 - Second wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2021-08-01"),y=2.2,label="Covid-19 - first wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2022-12-01"),y=2.2,label="Covid-19 - Omicron wave",angle = 90, size=4)+

  # # age > 30
  # annotate("text",x=ymd("2021-01-01"),y=5.3,label="Covid-19 - Second wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2021-08-01"),y=5.3,label="Covid-19 - first wave",angle = 90, size=4) +
  # annotate("text",x=ymd("2022-12-01"),y=5.3,label="Covid-19 - Omicron wave",angle = 90, size=4)+

      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("2016-01-01")), max(ymd("2024-01-01")))) +
                   # limits =c(min(ymd("2016-01-01")), max(ymd("2023-01-01")))) +
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
      geom_col(data= dat.exp,aes(x= birth,y =  rel_excess_birth/100, fill=significant_dummy)) +
      annotate("rect",xmin=ymd("2020-12-01"),xmax=ymd("2021-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
      annotate("rect",xmin=ymd("2021-07-01"),xmax=ymd("2021-09-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
      annotate("rect",xmin=ymd("2022-09-01"),xmax=ymd("2023-02-01"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="indianred1") +
      scale_x_date(labels = date_format("%Y"), 
                   breaks = date_breaks("1 year"),
                   limits =c(min(ymd("2016-01-01")), max(ymd("2024-01-01")))) +
                   # limits =c(min(ymd("2016-01-01")), max(ymd("2023-01-01")))) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual("",
                        breaks=c("significant","non-significant"),
                        values =c("red","grey")) +
      xlab("Year")+
      ylab("Relatitve differences")+
      # ggtitle(Title) +
      theme_bw()+
      #theme_light(base_size = 16)+
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
    
  
  cowplot::save_plot(paste0("output/plot_birth_2020_",group_data,".pdf"),   plot_together ,base_height=12,base_width=15)
  
}



function_plot_expected(group_data="total_birth",Title="Total births")

function_plot_expected(group_data="parity_1",Title="Parity = 1")
function_plot_expected(group_data="parity_2",Title="Parity = 2")
function_plot_expected(group_data="parity_3",Title="Parity = 3")
function_plot_expected(group_data="parity_sup_3",Title="Parity > 3")
function_plot_expected(group_data="parity_sup2",Title="Parity > 2")

# function_plot_expected(group_data="single_birth",pop_group="total",Title="Single birth")
# function_plot_expected(group_data="multiple_birth",pop_group="total",Title="Multiple birth")
# 
# function_plot_expected(group_data="females",pop_group="total",Title="Females")
# function_plot_expected(group_data="males",pop_group="total",Title="Males")


function_plot_expected(group_data="total_birth_females",Title="Population women aged 15-49")

function_plot_expected(group_data="mat_age_below_30",Title="Births women age < 30 years")
function_plot_expected(group_data="mat_age_above_or_eq_30",Title=" Births women aged >=30 years")



function_plot_expected(group_data="mat_age_below_30_pop_women_15_29",Title="Births women age < 30 years population 15-29")
function_plot_expected(group_data="mat_age_below_30_pop_women_30_49",Title=" Births women aged >=30 years population 30-49")


# function_plot_expected(group_data="german_romansh",pop_group="pop",Title="German")
# function_plot_expected(group_data="french",pop_group="pop",Title="French")
# function_plot_expected(group_data="italy",pop_group="pop",Title="Italian")
# 
function_plot_expected(group_data="swiss",Title="Swiss")
function_plot_expected(group_data="non_swiss",Title="Non Swiss")

