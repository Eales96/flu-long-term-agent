# Set location
setwd('C:/Users/EALESO/PycharmProjects/pythonProject/figure_scripts')

# Load plotting functions
source('plot_functions.R')


# Specify simulation parameters (obtained from main.py)
pop_size <- 80000 #160000
N_years <- 160 #160
N_iter <- 256 #128


#Load in baseline scenario
datB <- read.table('input_data/AttackRates/baseline.csv', header=F, sep=',')


# Load in different waning immunity parameter values
dat0 <- read.table('input_data/AttackRates/05_waning_immunity.csv', header=F, sep=',')
dat1 <- read.table('input_data/AttackRates/1_waning_immunity.csv', header=F, sep=',')
dat2 <- read.table('input_data/AttackRates/2_waning_immunity.csv', header=F, sep=',')
dat3 <- read.table('input_data/AttackRates/3_waning_immunity.csv', header=F, sep=',')
dat4 <- read.table('input_data/AttackRates/4_waning_immunity.csv', header=F, sep=',')
dat5 <- read.table('input_data/AttackRates/5_waning_immunity.csv', header=F, sep=',')


################################################################################################################################
# Waning Immunity figure (Main)
################################################################################################################################
# Dynamics over time
AR1 <- attack_rate_over_time(dat = datB, pop_size, N_years, N_iter)
AR2 <- attack_rate_over_time(dat = dat0, pop_size, N_years, N_iter)
AR3 <- attack_rate_over_time(dat = dat5, pop_size, N_years, N_iter)
AR4 <- attack_rate_over_time(dat = dat3, pop_size, N_years, N_iter)

AR1[[2]]$tau <- "1.27 (empirical estimate)"
AR2[[2]]$tau <- "0.5"
AR3[[2]]$tau <- "5.0"
AR4[[2]]$tau <- "3.0"

AR_reg <- rbind(AR1[[2]], AR2[[2]], AR3[[2]], AR4[[2]])
AR_reg$tau <- factor(AR_reg$tau)

ARD1 <- attack_rate_over_decade(dat = datB, pop_size, N_years, N_iter)
ARD2 <- attack_rate_over_decade(dat = dat0, pop_size, N_years, N_iter)
ARD3 <- attack_rate_over_decade(dat = dat5, pop_size, N_years, N_iter)
ARD4 <- attack_rate_over_decade(dat = dat3, pop_size, N_years, N_iter)

ARD1[[2]]$tau <- "1.27 (empirical estimate)"
ARD2[[2]]$tau <- "0.5"
ARD3[[2]]$tau <- "5.0"
ARD4[[2]]$tau <- "3.0"

ARD_reg <- rbind(ARD1[[2]], ARD2[[2]], ARD3[[2]], ARD4[[2]])
ARD_reg$tau <- factor(ARD_reg$tau)

ARD_reg <- ARD_reg[ARD_reg$TSI<=160,]
AR_reg <- AR_reg[AR_reg$TSI<=160,]

#########################################################################################
AR_reg$TSI <- AR_reg$TSI-1
ARD_reg$TSI <- ARD_reg$TSI-1

pltAS_R1A <- ggplot()+
  geom_line(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_point(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_ribbon(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tau, fill=tau), alpha=0.5)+
  #geom_errorbar(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR, group=tau), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual attack rate")+
  scale_color_brewer("Duration of short-term\nimmunity (years)", palette = "Dark2")+
  scale_fill_brewer("Duration of short-term\nimmunity (years)", palette = "Dark2")+
  theme_bw()+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  theme(legend.position = c(0.85,0.8),
        legend.background = element_rect(color='black'))




pltAS_R1B <- ggplot()+
  geom_line(data = AR_reg[AR_reg$TSI<11,], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_point(data = AR_reg[AR_reg$TSI<11,], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_ribbon(data = AR_reg[AR_reg$TSI<11,], aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tau, fill=tau), alpha=0.5)+
  #geom_errorbar(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR, group=tau), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual\nattack rate")+
  scale_color_brewer("Duration of short-term\nimmunity (years)", palette = "Dark2")+
  scale_fill_brewer("Duration of short-term\nimmunity (years)", palette = "Dark2")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,10,1))+
  theme(legend.position = "none",
        legend.background = element_rect(color='black'),
        plot.background = element_rect(color = 'black'))


plt1B<- ggplot()+
  geom_line(data = AR_D, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec))+
  geom_ribbon(data = AR_D, aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec), alpha=0.5)+
  #geom_jitter(data=AR1[[1]], aes(x=TSI, y=AR), alpha=0.2, width=0.2, height=0)+
  #geom_errorbar(data=ARD[[2]], aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual attack rate")+
  scale_x_continuous(breaks=c(0,20,40,60,80,100, 120, 140,160))+
  theme_bw(base_size = 12)


pltAS_R1C <- ggplot()+
  geom_line(data = ARD_reg[ARD_reg$tau=="0.5",], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec, color=tau))+
  geom_ribbon(data = ARD_reg[ARD_reg$tau=="0.5",], aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec, fill=tau), alpha=0.5)+
  geom_line(data = ARD_reg[ARD_reg$tau=="3.0",], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec, color=tau))+
  geom_ribbon(data = ARD_reg[ARD_reg$tau=="3.0",], aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec, fill=tau), alpha=0.5)+
  geom_line(data = ARD_reg[ARD_reg$tau=="5.0",], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec, color=tau))+
  geom_ribbon(data = ARD_reg[ARD_reg$tau=="5.0",], aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec, fill=tau), alpha=0.5)+
  geom_line(data = ARD_reg[ARD_reg$tau=="1.27 (empirical estimate)",], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec, color=tau))+
  geom_ribbon(data = ARD_reg[ARD_reg$tau=="1.27 (empirical estimate)",], aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec, fill=tau), alpha=0.5)+
  #geom_errorbar(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR, group=tau), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual\nattack rate")+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+
  coord_cartesian(ylim=c(0.1,0.27))+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  theme(legend.position = "none",
        legend.background = element_rect(color='black'),
        legend.title = element_blank())

pltW_A <- pltAS_R1A + annotation_custom(ggplotGrob(pltAS_R1B), xmin = 10, xmax = 110, 
                              ymin = 0.35, ymax = 0.55)


####################################################################################################################
# Correlation plot

acs <- autocorrelation_over_time(datB, pop_size, N_iter, N_years)

acs05 <- autocorrelation_over_time(dat0, pop_size, N_iter, N_years)
acs1 <- autocorrelation_over_time(dat1, pop_size, N_iter, N_years)
acs2 <- autocorrelation_over_time(dat2, pop_size, N_iter, N_years)
acs3 <- autocorrelation_over_time(dat3, pop_size, N_iter, N_years)
acs4 <- autocorrelation_over_time(dat4, pop_size, N_iter, N_years)
acs5 <- autocorrelation_over_time(dat5, pop_size, N_iter, N_years)


fishers_z<- function(r){
  log((1+r) / (1-r)) / 2
  
}

inv_fishers_z <- function(z){
  
  (exp(2*z)-1)/(exp(2*z)+1)
}
new_analytics <- function(acs, label){
  
  cor_df <- data.frame()
  for(i in seq_len(length(unique(acs$dif)))){
    comp_df <- acs[acs$dif==i & acs$year>20 & acs$year<(161-i),]
    print(i)
    
    
    comp_df$new_mn <- fishers_z(comp_df$cor)
    comp_df$new_lwr <- fishers_z(comp_df$cor_lwr)
    comp_df$new_upr <- fishers_z(comp_df$cor_upr)
    comp_df$new_sd <- abs(comp_df$new_lwr- comp_df$new_upr)/3.92
    
    cor <- mean(comp_df$new_mn)
    cor_sd <- sqrt(sum(comp_df$new_sd**2))/nrow(comp_df)
    
    
    
    rndm <- rnorm(10000, cor, cor_sd)
    
    
    row_df <- data.frame(cor = inv_fishers_z(cor),
                         cor_lwr = inv_fishers_z(cor-1.96*cor_sd),
                         cor_upr = inv_fishers_z(cor+1.96*cor_sd),
                         p_val = length(inv_fishers_z(rndm[rndm>0]))/10000,
                         yr_dif = i,
                         label = label)
    
    cor_df<-rbind(cor_df, row_df)
    
    
  }
  
  return(cor_df)
  
}

corWB <- new_analytics(acs, "1.27 (empirical estimate)")
corW05 <- new_analytics(acs05, "0.5")
corW1 <- new_analytics(acs1, "1")
corW2 <- new_analytics(acs2, "2")
corW3 <- new_analytics(acs3, "3")
corW4 <- new_analytics(acs4, "4")
corW5 <- new_analytics(acs5, "5")

corW_df <- rbind(corWB,corW05, corW1, corW2, corW3, corW4, corW5)


##################


pltW_B <- ggplot()+
  geom_line(data=corW_df, aes(x=yr_dif, y=cor, color=label), position=position_dodge(width = 0.2), alpha=0.6)+
  geom_errorbar(data=corW_df, aes(x=yr_dif, y=cor,ymin=cor_lwr, ymax=cor_upr, color=label), width=0.0, position=position_dodge(width = 0.2))+
  geom_point(data=corW_df, aes(x=yr_dif, y=cor, color=label),position=position_dodge(width = 0.2))+
  xlab("Years between epidemics")+
  ylab("Correlation between\nannual attack rates")+
  geom_hline(yintercept = 0.0, linetype="dashed")+
  scale_color_brewer("Duration of short-term\nimmunity (years)", palette = "Set1")+
  scale_fill_brewer("Duration of short-term\nimmunity (years)", palette = "Set1")+
  theme_bw()+
  coord_cartesian(xlim=c(1,10))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), minor_breaks =c(1,2,3,4,5,6,7,8,9,10) )+
  theme(legend.position = c(0.7,0.23),
        legend.background = element_rect(color='black'))







pltW_A<-pltW_A+labs(tag="A")+
  theme(plot.tag.position=c(0.01,0.99),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13))
pltW_B<-pltW_B+labs(tag="B")+
  theme(plot.tag.position=c(0.01,0.99),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13))
pltW_C<-pltAS_R1C+labs(tag="B")+
  theme(plot.tag.position=c(0.01,0.99))

grd <- plot_grid(pltW_C, pltW_B, nrow=2, rel_heights = c(0.6,1))

plot_grid(pltW_A, pltW_B, ncol=2, rel_widths = c(1,0.6))
ggsave('figures/WaningImmunity/WaningImmunity_main.pdf', width=12, height=6)





########################################################################################
A_AR1_00 <- age_attack_rate_by_tsi(dat = datB,pop_size, N_years, N_iter, tsi=1)
A_AR1_01 <- age_attack_rate_by_decade(dat = datB,pop_size, N_years, N_iter, tsi=11)#40
A_AR1_02 <- age_attack_rate_by_decade(dat = datB,pop_size, N_years, N_iter, tsi=71)#80
A_AR1_03 <- age_attack_rate_by_decade(dat = datB,pop_size, N_years, N_iter, tsi=151)#80

A_AR2_00 <- age_attack_rate_by_tsi(dat = dat0,pop_size, N_years, N_iter, tsi=1)
A_AR2_01 <- age_attack_rate_by_decade(dat = dat0,pop_size, N_years, N_iter, tsi=11)
A_AR2_02 <- age_attack_rate_by_decade(dat = dat0,pop_size, N_years, N_iter, tsi=71)
A_AR2_03 <- age_attack_rate_by_decade(dat = dat0,pop_size, N_years, N_iter, tsi=151)

A_AR3_00 <- age_attack_rate_by_tsi(dat = dat5,pop_size, N_years, N_iter, tsi=1)
A_AR3_01 <- age_attack_rate_by_decade(dat = dat5,pop_size, N_years, N_iter, tsi=11)
A_AR3_02 <- age_attack_rate_by_decade(dat = dat5,pop_size, N_years, N_iter, tsi=71)
A_AR3_03 <- age_attack_rate_by_decade(dat = dat5,pop_size, N_years, N_iter, tsi=151)

A_AR6_00 <- age_attack_rate_by_tsi(dat = dat3,pop_size, N_years, N_iter, tsi=1)
A_AR6_01 <- age_attack_rate_by_tsi(dat = dat3,pop_size, N_years, N_iter, tsi=11)
A_AR6_02 <- age_attack_rate_by_decade(dat = dat3,pop_size, N_years, N_iter, tsi=71)#80
A_AR6_03 <- age_attack_rate_by_decade(dat = dat3,pop_size, N_years, N_iter, tsi=151)#80



age_df_grp1 <- rbind(A_AR1_00[[3]],A_AR1_01[[3]],A_AR1_02[[3]],A_AR1_03[[3]])
age_df_grp2 <- rbind(A_AR2_00[[3]],A_AR2_01[[3]],A_AR2_02[[3]],A_AR2_03[[3]])
age_df_grp3 <- rbind(A_AR3_00[[3]],A_AR3_01[[3]],A_AR3_02[[3]],A_AR3_03[[3]])
age_df_grp4 <- rbind(A_AR6_00[[3]],A_AR6_01[[3]],A_AR6_02[[3]],A_AR6_03[[3]])

age_df1 <- rbind(A_AR1_00[[2]],A_AR1_01[[2]],A_AR1_02[[2]],A_AR1_03[[2]])
age_df2 <- rbind(A_AR2_00[[2]],A_AR2_01[[2]],A_AR2_02[[2]],A_AR2_03[[2]])
age_df3 <- rbind(A_AR3_00[[2]],A_AR3_01[[2]],A_AR3_02[[2]],A_AR3_03[[2]])
age_df4 <- rbind(A_AR6_00[[2]],A_AR6_01[[2]],A_AR6_02[[2]],A_AR6_03[[2]])


age_df1$tau <- "1.27 (empirical estimate)"
age_df2$tau <- "0.5"
age_df3$tau <- "5.0"
age_df4$tau <- "3.0"

age_df_reg <- rbind(age_df1,age_df2,age_df3, age_df4)


##################
levels(age_df_reg$tsi) <- list("Year of introduction"="1", "10-19 years"="11", "70-79 years"="71", "150-159 years"="151")



col_custom <- RColorBrewer::brewer.pal(8, "Set1")[1:4]
pltW_C <- ggplot()+
  geom_line(data=age_df_reg, aes(x=age, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+1.96*sem_AR, group=tsi, color=tsi))+
  geom_ribbon(data=age_df_reg, aes(x=age, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tsi, fill=tsi),alpha=0.5)+
  scale_color_manual("Time since introduction",values=col_custom)+
  scale_fill_manual("Time since introduction",values=col_custom)+
  xlab("Age")+
  ylab("Attack rate")+
  facet_wrap(.~tau, nrow=1)+
  theme_bw()+
  theme(legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black'))

# Age by all durations of positivity

A_AR4_00 <- age_attack_rate_by_tsi(dat = dat1,pop_size, N_years, N_iter, tsi=1)
A_AR4_01 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=10)
A_AR4_02 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=70)#80

A_AR5_00 <- age_attack_rate_by_tsi(dat = dat2,pop_size, N_years, N_iter, tsi=1)
A_AR5_01 <- age_attack_rate_by_decade(dat = dat2,pop_size, N_years, N_iter, tsi=10)
A_AR5_02 <- age_attack_rate_by_decade(dat = dat2,pop_size, N_years, N_iter, tsi=70)#80

A_AR7_00 <- age_attack_rate_by_tsi(dat = dat4,pop_size, N_years, N_iter, tsi=1)
A_AR7_01 <- age_attack_rate_by_decade(dat = dat4,pop_size, N_years, N_iter, tsi=10)
A_AR7_02 <- age_attack_rate_by_decade(dat = dat4,pop_size, N_years, N_iter, tsi=70)#80



age_df1 <- rbind(A_AR1_00[[2]],A_AR1_01[[2]],A_AR1_02[[2]])
age_df2 <- rbind(A_AR2_00[[2]],A_AR2_01[[2]],A_AR2_02[[2]])
age_df3 <- rbind(A_AR3_00[[2]],A_AR3_01[[2]],A_AR3_02[[2]])

age_df4 <- rbind(A_AR4_00[[2]],A_AR4_01[[2]],A_AR4_02[[2]])
age_df5 <- rbind(A_AR5_00[[2]],A_AR5_01[[2]],A_AR5_02[[2]])
age_df6 <- rbind(A_AR6_00[[2]],A_AR6_01[[2]],A_AR6_02[[2]])
age_df7 <- rbind(A_AR7_00[[2]],A_AR7_01[[2]],A_AR7_02[[2]])


age_df1$tau <- "1.27 (empirical estimate)"
age_df2$tau <- "0.5"
age_df3$tau <- "5.0"

age_df4$tau <- "1.0"
age_df5$tau <- "2.0"
age_df6$tau <- "3.0"
age_df7$tau <- "4.0"


age_df <- rbind(age_df1,age_df2,age_df3,
                    age_df4,age_df5,age_df6,
                    age_df6)

levels(age_df$tsi) <- list("Year of introduction"="1","10-19 years"="10", "70-79 years"="70")

age_df1 <- age_df[age_df$tsi=="70-79 years",]
age_df2 <- age_df[age_df$tsi=="Year of introduction",]

age_df1$mn_AR <- age_df1$mn_AR/age_df2$mn_AR
age_df1$sem_AR <- age_df1$sem_AR/age_df2$mn_AR
age_df1$tau <- as.factor(age_df1$tau)

age_df1$sem_AR



pltW_D <- ggplot()+
  geom_line(data=age_df1, aes(x=age, y=mn_AR,  color=tau))+
  geom_ribbon(data=age_df1, aes(x=age, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, fill=tau),alpha=0.5)+
  scale_color_brewer("Duration of short-term\nimmunity (years)",palette='Dark2')+
  scale_fill_brewer("Duration of short-term\nimmunity (years)",palette="Dark2")+
  xlab("Age")+
  ylab("Proportion of the attack rate in year of introduction")+
  theme_bw()+
  theme(legend.position = c(0.3,0.89),
        legend.background = element_rect(colour = 'black'))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),
         color=guide_legend(nrow=2, byrow = TRUE))



age_df1 <- age_df[age_df$tsi=="70-79 years",]
age_df2 <- age_df[age_df$tsi=="10-19 years",]

age_df1$mn_AR <- age_df1$mn_AR/age_df2$mn_AR
age_df1$sem_AR <- age_df1$sem_AR/age_df2$mn_AR
age_df1$tau <- as.factor(age_df1$tau)

age_df1$sem_AR



pltW_E <- ggplot()+
  geom_line(data=age_df1, aes(x=age, y=mn_AR,  color=tau))+
  geom_ribbon(data=age_df1, aes(x=age, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, fill=tau),alpha=0.5)+
  scale_color_brewer("Duration of short-term\nimmunity (years)",palette='Dark2')+
  scale_fill_brewer("Duration of short-term\nimmunity (years)",palette="Dark2")+
  xlab("Age")+
  ylab("Proportion of the attack rate in years 10-19")+
  theme_bw()+
  theme(legend.position = c(0.24,0.89),
        legend.background = element_rect(colour = 'black'))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),
         color=guide_legend(nrow=2, byrow = TRUE))



pltW_A<-pltW_A+labs(tag="A")+
  theme(plot.tag.position=c(0.01,0.95))
pltW_B<-pltW_B+labs(tag="C")+
  theme(plot.tag.position=c(0.01,0.95))
pltW_C<-pltAS_R1C+labs(tag="B")+
  theme(plot.tag.position=c(0.01,0.95))

grd <- plot_grid(pltW_C, pltW_B, nrow=2, rel_widths = c(1,1))

plot_grid(pltW_A, grd, ncol=2, rel_widths = c(1,0.5))
ggsave('figures/WaningImmunity/WaningImmunity_main.pdf', width=14, height=6)


pltW_C<-pltW_C+labs(tag="A")+
  theme(plot.tag.position=c(0.01,0.95))
pltW_D<-pltW_D+labs(tag="B")+
  theme(plot.tag.position=c(0.01,0.95))
pltW_E<-pltW_E+labs(tag="C")+
  theme(plot.tag.position=c(0.01,0.95))

pltW_row <- plot_grid(pltW_D, pltW_E, nrow=1, rel_widths = c(1,1))
plot_grid(pltW_C,pltW_row, nrow=2, rel_heights = c(1,1.5))
ggsave('figures/WaningImmunity/WaningImmunity_Age_sup.pdf', width=14, height=10)

##########################################################################################################################
# Dynamics by Cohort supplemental
##########################################################################################################################

dbc1 <- dynamics_by_cohort(datB, pop_size, N_years, N_iter)
dbc2 <- dynamics_by_cohort(dat0, pop_size, N_years, N_iter)
dbc3 <- dynamics_by_cohort(dat5, pop_size, N_years, N_iter)
dbc4 <- dynamics_by_cohort(dat3, pop_size, N_years, N_iter)


dbc1$tau <- "1.27 (empirical estimate)"
dbc2$tau <- "0.5"
dbc3$tau <- "5.0"
dbc4$tau <- "3.0"


dbc <- rbind(dbc1, dbc2, dbc3, dbc4)


dbc$age_grp <- factor(dbc$age_grp)

levels(dbc$age_grp) <- list("0-4 years" = "1",
                             "5-9 years" = "2",
                             "10-14 years" = "3",
                             "15-19 years" = "4",
                             "20-24 years" = "5",
                             "25-29 years" = "6",
                             "30-34 years" = "7",
                             "35-39 years" = "8",
                             "40-44 years" = "9",
                             "45-49 years" = "10",
                             "50-54 years" = "11",
                             "55-59 years" = "12",
                             "60-64 years" = "13",
                             "65-69 years" = "14",
                             "70-74 years" = "15",
                             "75-79 years" = "16")


dbc <- dbc[dbc$birth_year<81,]
dbc <- dbc[dbc$yrs>4,]
dbc$ar <- dbc$ar/5
dbc$sem_ar <- dbc$sem_ar/5




ggplot()+
  scale_x_continuous(breaks=c(-80,-60,-40,-20,0,20,40,60,80), labels = c(-80,-60,-40,-20,"0\nPandemic\nyear",20,40,60,80))+
  xlab("Birth year")+
  ylab("Mean attack rate")+
  #geom_vline(aes(xintercept = birth_year), data=dbc1_mn1, linetype='dashed', col='red', alpha=0.1)+
  #geom_vline(aes(xintercept = birth_year), data=dbc1_mn3, linetype='dashed', col='blue', alpha=0.1)+
  #geom_hline(aes(yintercept = ar), data=dbc1_mn1, linetype='dashed', col='red')+
  #geom_hline(aes(yintercept = min), data=dbc1, linetype='dashed', col='blue')+
  geom_line(data=dbc, aes(x=birth_year, y=ar, color=tau))+
  geom_ribbon(data=dbc, aes(x=birth_year, y=ar, ymin=ar-1.96*sem_ar, ymax=ar+1.96*sem_ar, fill=tau), alpha=0.5)+
  facet_wrap(.~age_grp, nrow=4, scales = 'free_y')+
  coord_cartesian(xlim=c(-80,80))+
  scale_color_brewer("Duration of short-term\nimmunity (years)", palette = "Dark2")+
  scale_fill_brewer("Duration of short-term\nimmunity (years)", palette = "Dark2")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave('figures/WaningImmunity/CohortByAge_WI.pdf', width=14, height=10)
