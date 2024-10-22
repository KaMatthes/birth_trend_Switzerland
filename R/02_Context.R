data_cont <- read.xlsx("data/Context.xlsx")


Figure1 <- ggplot() +
  annotate("rect",xmin=53,xmax=56,ymin=-Inf,ymax=Inf,alpha=0.25,fill="orange")+
  annotate("rect",xmin=27,xmax=29,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  annotate("rect",xmin=33,xmax=38,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  annotate("rect",xmin=46,xmax=52,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  geom_vline(xintercept = 27, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 65, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 50, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 37, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 41, linetype="dashed", color="darkgrey", size=2)+
  geom_hline(yintercept = 0, color="black", size=0.2)+
  geom_bar(data = data_cont, aes(y=Hosp,x=Months1), stat="identity", fill="black")+
  annotate("text", x=25.5, y=-300, label="a", angle=0, size=10, color="black")+
  annotate("text", x=28.5, y=-300, label="b", angle=0, size=10, color="black")+
  annotate("text", x=35, y=-300, label="c", angle=0, size=10, color="black")+
  annotate("text", x=38.5, y=-300, label="d", angle=0, size=10, color="black")+
  annotate("text", x=42.5, y=-300, label="e", angle=0, size=10, color="black")+
  annotate("text", x=48, y=-300, label="f", angle=0, size=10, color="black")+
  annotate("text", x=51.5, y=-300, label="g", angle=0, size=10, color="black")+
  annotate("text", x=54.8, y=-300, label="h", angle=0, size=10, color="black")+
  annotate("text", x=63.5, y=-300, label="i", angle=0, size=10, color="black")+
  xlab("Months")+
  ylab("Count (n)")+
  ggtitle("A) Hospitalisations due to COVID-19 ") +
  ylim(-300, 7000)+
  scale_x_continuous(limits=c(1, 72), breaks = c(1,13,25,37,49,61),
                     label = c("2018", "2019", "2020", "2021", "2022", "2023"))+
  theme(axis.text.x = element_text(size=40, angle=45, vjust = 0.5, hjust=0.5))+
  theme_bw()+
  theme(text = element_text(size = 50))+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank())



Figure2 <- ggplot() +
  annotate("rect",xmin=53,xmax=56,ymin=-Inf,ymax=Inf,alpha=0.25,fill="orange")+
  annotate("rect",xmin=27,xmax=29,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  annotate("rect",xmin=33,xmax=38,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  annotate("rect",xmin=46,xmax=52,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  geom_vline(xintercept = 27, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 65, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 50, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 37, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 41, linetype="dashed", color="darkgrey", size=2)+
  geom_hline(yintercept = 0, color="black", size=0.2)+
  geom_point(data = data_cont, aes(y=CPI,x=Months1), shape=16, size=8)+
  annotate("text", x=25.5, y=95, label="a", angle=0, size=10, color="black")+
  annotate("text", x=28.5, y=95, label="b", angle=0, size=10, color="black")+
  annotate("text", x=35, y=95, label="c", angle=0, size=10, color="black")+
  annotate("text", x=38.5, y=95, label="d", angle=0, size=10, color="black")+
  annotate("text", x=42.5, y=95, label="e", angle=0, size=10, color="black")+
  annotate("text", x=48, y=95, label="f", angle=0, size=10, color="black")+
  annotate("text", x=51.5, y=95, label="g", angle=0, size=10, color="black")+
  annotate("text", x=54.8, y=95, label="h", angle=0, size=10, color="black")+
  annotate("text", x=63.5, y=95, label="i", angle=0, size=10, color="black")+
  xlab("Months")+
  ylab("CPI")+
  ggtitle("B) Consumer Prize Index ") +
  ylim(95, 105)+
  scale_x_continuous(limits=c(1, 72), breaks = c(1,13,25,37,49,61),
                     label = c("2018", "2019", "2020", "2021", "2022", "2023"))+
  theme(axis.text.x = element_text(size=40, angle=45, vjust = 0.5, hjust=0.5))+
  theme_bw()+
  theme(text = element_text(size = 50))+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank())



Figure3 <- ggplot() +
  annotate("rect",xmin=53,xmax=56,ymin=-Inf,ymax=Inf,alpha=0.25,fill="orange")+
  annotate("rect",xmin=27,xmax=29,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  annotate("rect",xmin=33,xmax=38,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  annotate("rect",xmin=46,xmax=52,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  geom_vline(xintercept = 27, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 65, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 50, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 37, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 41, linetype="dashed", color="darkgrey", size=2)+
  geom_point(data = data_cont, aes(y=Unemployment.rate,x=Months1), shape=16, size=8)+
  annotate("text", x=25.5, y=0, label="a", angle=0, size=10, color="black")+
  annotate("text", x=28.5, y=0, label="b", angle=0, size=10, color="black")+
  annotate("text", x=35, y=0, label="c", angle=0, size=10, color="black")+
  annotate("text", x=38.5, y=0, label="d", angle=0, size=10, color="black")+
  annotate("text", x=42.5, y=0, label="e", angle=0, size=10, color="black")+
  annotate("text", x=48, y=0, label="f", angle=0, size=10, color="black")+
  annotate("text", x=51.5, y=0, label="g", angle=0, size=10, color="black")+
  annotate("text", x=54.8, y=0, label="h", angle=0, size=10, color="black")+
  annotate("text", x=63.5, y=0, label="i", angle=0, size=10, color="black")+
  xlab("Months")+
  ylab("Percentage")+
  ggtitle("C) Unemployment rate") +
  ylim(0, 5)+
  scale_x_continuous(limits=c(1, 72), breaks = c(1,13,25,37,49,61),
                     label = c("2018", "2019", "2020", "2021", "2022", "2023"))+
  theme(axis.text.x = element_text(size=40, angle=45, vjust = 0.5, hjust=0.5))+
  theme_bw()+
  theme(text = element_text(size = 50))+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank())


Figure4 <- ggplot() +
  annotate("rect",xmin=53,xmax=56,ymin=-Inf,ymax=Inf,alpha=0.25,fill="orange")+
  annotate("rect",xmin=27,xmax=29,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  annotate("rect",xmin=33,xmax=38,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  annotate("rect",xmin=46,xmax=52,ymin=-Inf,ymax=Inf,alpha=0.5,fill="lightblue")+
  geom_vline(xintercept = 27, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 65, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 50, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 37, linetype="dashed", color="darkgrey", size=2)+
  geom_vline(xintercept = 41, linetype="dashed", color="darkgrey", size=2)+
  geom_hline(yintercept = 0, color="black", size=0.2)+
  geom_bar(data = data_cont, aes(y=VaccOneDos,x=Months1), stat="identity", fill="darkgrey")+
  geom_bar(data = data_cont, aes(y=VaccFully,x=Months1), stat="identity", fill="black")+
  annotate("text", x=25.5, y=-5, label="a", angle=0, size=10, color="black")+
  annotate("text", x=28.5, y=-5, label="b", angle=0, size=10, color="black")+
  annotate("text", x=35, y=-5, label="c", angle=0, size=10, color="black")+
  annotate("text", x=38.5, y=-5, label="d", angle=0, size=10, color="black")+
  annotate("text", x=42.5, y=-5, label="e", angle=0, size=10, color="black")+
  annotate("text", x=48, y=-5, label="f", angle=0, size=10, color="black")+
  annotate("text", x=51.5, y=-5, label="g", angle=0, size=10, color="black")+
  annotate("text", x=54.8, y=-5, label="h", angle=0, size=10, color="black")+
  annotate("text", x=63.5, y=-5, label="i", angle=0, size=10, color="black")+
  xlab("Months")+
  ylab("Percentage")+
  ggtitle("D) COVID-19 Vaccination coverage 20-40y ") +
  ylim(-5, 100)+
  scale_x_continuous(limits=c(1, 72), breaks = c(1,13,25,37,49,61),
                     label = c("2018", "2019", "2020", "2021", "2022", "2023"))+
  theme(axis.text.x = element_text(size=40, angle=45, vjust = 0.5, hjust=0.5))+
  theme_bw()+
  theme(text = element_text(size = 50))+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank())


Figure_Cont <- plot_grid(Figure1, Figure2, Figure3, Figure4, ncol = 1,align = 'v')

cowplot::save_plot("output/context.png", Figure_Cont,base_height=40,base_width=25)



