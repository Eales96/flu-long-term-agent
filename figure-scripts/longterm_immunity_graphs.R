# Set location
setwd('/flu-long-term-agent/figure_scripts')

# Load plotting functions and packages
source('plot_functions.R')


# Specify simulation parameters (obtained from main.py)
pop_size <- 80000
N_years <- 160
N_iter <- 256

#Load in baseline scenario
dat20 <- read.table('input_data/AttackRates/baseline.csv', header=F, sep=',')

# Load in different antigenic seniority parameter values
#dat05 <- read.table('input_data/AttackRates/05_longterm_immunity.csv', header=F, sep=',')
dat10 <- read.table('input_data/AttackRates/10_longterm_immunity.csv', header=F, sep=',')
dat15 <- read.table('input_data/AttackRates/15_longterm_immunity.csv', header=F, sep=',')
dat25 <- read.table('input_data/AttackRates/25_longterm_immunity.csv', header=F, sep=',')
dat30 <- read.table('input_data/AttackRates/30_longterm_immunity.csv', header=F, sep=',')
dat35 <- read.table('input_data/AttackRates/35_longterm_immunity.csv', header=F, sep=',')
dat40 <- read.table('input_data/AttackRates/40_longterm_immunity.csv', header=F, sep=',')

################################################################################################################################
# Longterm immunity figure (Main)
################################################################################################################################

AR1 <- attack_rate_over_time(dat = dat20, pop_size, N_years, N_iter)
AR2 <- attack_rate_over_time(dat = dat40, pop_size, N_years, N_iter)
AR3 <- attack_rate_over_time(dat = dat10, pop_size, N_years, N_iter)
AR4 <- attack_rate_over_time(dat = dat15, pop_size, N_years, N_iter)

AR1[[2]]$tau <- "2.02 (empirical long-term immunity)"
AR2[[2]]$tau <- "4.0 (high long-term immunity)"
AR3[[2]]$tau <- "1.0"
AR4[[2]]$tau <- "1.5"

AR_reg <- rbind(AR1[[2]], AR2[[2]], AR3[[2]], AR4[[2]])
AR_reg$tau <- factor(AR_reg$tau)

ARD1 <- attack_rate_over_decade(dat = dat20, pop_size, N_years, N_iter)
ARD2 <- attack_rate_over_decade(dat = dat40, pop_size, N_years, N_iter)
ARD3 <- attack_rate_over_decade(dat = dat10, pop_size, N_years, N_iter)
ARD4 <- attack_rate_over_decade(dat = dat15, pop_size, N_years, N_iter)

ARD1[[2]]$tau <- "2.02 (empirical long-term immunity)"
ARD2[[2]]$tau <- "4.0 (high long-term immunity)"
ARD3[[2]]$tau <- "1.0"
ARD4[[2]]$tau <- "1.5"

ARD_reg <- rbind(ARD1[[2]], ARD2[[2]], ARD3[[2]], ARD4[[2]])
ARD_reg$tau <- factor(ARD_reg$tau)

ARD_reg <- ARD_reg[ARD_reg$TSI<=160,]
AR_reg <- AR_reg[AR_reg$TSI<=160,]

#########################################################################################
A_AR1_00 <- age_attack_rate_by_tsi(dat = dat20,pop_size, N_years, N_iter, tsi=1)
A_AR1_01 <- age_attack_rate_by_decade(dat = dat20,pop_size, N_years, N_iter, tsi=11)#40
A_AR1_02 <- age_attack_rate_by_decade(dat = dat20,pop_size, N_years, N_iter, tsi=71)#80
A_AR1_03 <- age_attack_rate_by_decade(dat = dat20,pop_size, N_years, N_iter, tsi=151)#80

A_AR2_00 <- age_attack_rate_by_tsi(dat = dat40,pop_size, N_years, N_iter, tsi=1)
A_AR2_01 <- age_attack_rate_by_decade(dat = dat40,pop_size, N_years, N_iter, tsi=11)
A_AR2_02 <- age_attack_rate_by_decade(dat = dat40,pop_size, N_years, N_iter, tsi=71)
A_AR2_03 <- age_attack_rate_by_decade(dat = dat40,pop_size, N_years, N_iter, tsi=151)

A_AR3_00 <- age_attack_rate_by_tsi(dat = dat10,pop_size, N_years, N_iter, tsi=1)
A_AR3_01 <- age_attack_rate_by_decade(dat = dat10,pop_size, N_years, N_iter, tsi=11)
A_AR3_02 <- age_attack_rate_by_decade(dat = dat10,pop_size, N_years, N_iter, tsi=71)
A_AR3_03 <- age_attack_rate_by_decade(dat = dat10,pop_size, N_years, N_iter, tsi=151)

A_AR5_00 <- age_attack_rate_by_tsi(dat = dat15,pop_size, N_years, N_iter, tsi=1)
A_AR5_01 <- age_attack_rate_by_tsi(dat = dat15,pop_size, N_years, N_iter, tsi=11) ##
A_AR5_03 <- age_attack_rate_by_tsi(dat = dat15,pop_size, N_years, N_iter, tsi=71) ##
A_AR5_02 <- age_attack_rate_by_decade(dat = dat15,pop_size, N_years, N_iter, tsi=151)#80

age_df_grp1 <- rbind(A_AR1_00[[3]],A_AR1_01[[3]],A_AR1_02[[3]],A_AR1_03[[3]])
age_df_grp2 <- rbind(A_AR2_00[[3]],A_AR2_01[[3]],A_AR2_02[[3]],A_AR2_03[[3]])
age_df_grp3 <- rbind(A_AR3_00[[3]],A_AR3_01[[3]],A_AR3_02[[3]],A_AR3_03[[3]])
age_df_grp4 <- rbind(A_AR5_00[[3]],A_AR5_01[[3]],A_AR5_03[[3]],A_AR5_02[[3]])

age_df1 <- rbind(A_AR1_00[[2]],A_AR1_01[[2]],A_AR1_02[[2]],A_AR1_03[[2]])
age_df2 <- rbind(A_AR2_00[[2]],A_AR2_01[[2]],A_AR2_02[[2]],A_AR2_03[[2]])
age_df3 <- rbind(A_AR3_00[[2]],A_AR3_01[[2]],A_AR3_02[[2]],A_AR3_03[[2]])
age_df4 <- rbind(A_AR5_00[[2]],A_AR5_01[[2]],A_AR5_03[[2]],A_AR5_02[[2]])


age_df_grp1$tau <- "2.02 (empirical long-term immunity)"
age_df_grp2$tau <- "4.0 (high long-term immunity)"
age_df_grp3$tau <- "1.0"
age_df_grp4$tau <- "1.5"
age_df_grp_reg <- rbind(age_df_grp1,age_df_grp2,age_df_grp3,age_df_grp4)

age_df1$tau <- "2.02 (empirical long-term immunity)"
age_df2$tau <- "4.0 (high long-term immunity)"
age_df3$tau <- "1.0"
age_df4$tau <- "1.5"
age_df_reg <- rbind(age_df1,age_df2,age_df3,age_df4)

##################
levels(age_df_reg$tsi) <- list("Pandemic year"="1", "10-19 years"="11", "70-79 years"="71", "150-159 years"="151")
levels(age_df_grp_reg$tsi) <- list("Pandemic year"="1", "10-19 years"="11", "70-79 years"="71", "150-159 years"="151")

pltAS_R1A <- ggplot()+
  geom_line(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_point(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_ribbon(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tau, fill=tau), alpha=0.5)+
  #geom_errorbar(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR, group=tau), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual attack rate")+
  scale_color_brewer("Long-term immunity parameter", palette = "Dark2")+
  scale_fill_brewer("Long-term immunity parameter", palette = "Dark2")+
  theme_bw()+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  theme(legend.position = c(0.2,0.85),
        legend.background = element_rect(color='black'))

pltAS_R1B <- ggplot()+
  geom_line(data = ARD_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_ribbon(data = ARD_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tau, fill=tau), alpha=0.5)+
  #geom_errorbar(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR, group=tau), alpha=0.2, width=0.0)+
  xlab("Time since introduction")+
  ylab("Attack rate")+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  theme(legend.position = c(0.2,0.8),
        legend.background = element_rect(color='black'),
        legend.title = element_blank())



col_custom <- RColorBrewer::brewer.pal(6, "Dark2")[4:6]
pltAS_R2B <- ggplot()+
  geom_line(data=age_df_reg, aes(x=age, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+1.96*sem_AR, group=tsi, color=tsi))+
  geom_ribbon(data=age_df_reg, aes(x=age, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tsi, fill=tsi),alpha=0.5)+
  scale_color_viridis_d("Years since pandemic",option = "turbo")+
  scale_fill_viridis_d("Years since pandemic",option = "turbo")+
  xlab("Age")+
  ylab("Average annual attack rate")+
  facet_wrap(.~tau, nrow=1)+
  theme_bw()+
  theme(legend.position = c(0.91,0.75),
        legend.background = element_rect(colour = 'black'))

########################################################################################
# Comparison between tau

#A_AR4_00 <- age_attack_rate_by_tsi(dat = dat10,pop_size, N_years, N_iter, tsi=1)
#A_AR4_02 <- age_attack_rate_by_decade(dat = dat10,pop_size, N_years, N_iter, tsi=70)#80

A_AR5_00 <- age_attack_rate_by_tsi(dat = dat15,pop_size, N_years, N_iter, tsi=1)
A_AR5_02 <- age_attack_rate_by_decade(dat = dat15,pop_size, N_years, N_iter, tsi=151)#80

A_AR6_00 <- age_attack_rate_by_tsi(dat = dat25,pop_size, N_years, N_iter, tsi=1)
A_AR6_02 <- age_attack_rate_by_decade(dat = dat25,pop_size, N_years, N_iter, tsi=151)#80

A_AR7_00 <- age_attack_rate_by_tsi(dat = dat30,pop_size, N_years, N_iter, tsi=1)
A_AR7_02 <- age_attack_rate_by_decade(dat = dat30,pop_size, N_years, N_iter, tsi=151)#80

A_AR8_00 <- age_attack_rate_by_tsi(dat = dat35,pop_size, N_years, N_iter, tsi=1)
A_AR8_02 <- age_attack_rate_by_decade(dat = dat35,pop_size, N_years, N_iter, tsi=151)#80


age_df1 <- rbind(A_AR1_00[[2]],A_AR1_03[[2]])
age_df2 <- rbind(A_AR2_00[[2]],A_AR2_03[[2]])
age_df3 <- rbind(A_AR3_00[[2]],A_AR3_03[[2]])

#age_df4 <- rbind(A_AR4_00[[2]],A_AR4_02[[2]])
age_df5 <- rbind(A_AR5_00[[2]],A_AR5_02[[2]])
age_df6 <- rbind(A_AR6_00[[2]],A_AR6_02[[2]])
age_df7 <- rbind(A_AR7_00[[2]],A_AR7_02[[2]])
age_df8 <- rbind(A_AR8_00[[2]],A_AR8_02[[2]])
#age_df9 <- rbind(A_AR9_00[[2]],A_AR9_02[[2]])


age_df1$tau <- "2.02 (empirical estimate)"
age_df2$tau <- "4.0"
age_df3$tau <- "1.0"

#age_df4$tau <- "1.0"
age_df5$tau <- "1.5"
age_df6$tau <- "2.5"
age_df7$tau <- "3.0"
age_df8$tau <- "3.5"
age_df <- rbind(age_df1,age_df2,age_df3,
                age_df5,age_df6,
                age_df7,age_df8)

levels(age_df$tsi) <- list("Pandemic year"="1", "150-159 years"="151")

age_df1 <- age_df[age_df$tsi=="150-159 years",]
age_df2 <- age_df[age_df$tsi=="Pandemic year",]

age_df1$mn_AR <- age_df1$mn_AR/age_df2$mn_AR
age_df1$sem_AR <- age_df1$sem_AR/age_df2$mn_AR
age_df1$tau <- as.factor(age_df1$tau)

pltAS_R2C <- ggplot()+
  geom_line(data=age_df1, aes(x=age, y=mn_AR,  color=tau))+
  geom_ribbon(data=age_df1, aes(x=age, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, fill=tau),alpha=0.5)+
  scale_color_brewer("Long-term immunity parameter",palette='Set1')+
  scale_fill_brewer("Long-term immunity parameter",palette="Set1")+
  xlab("Age")+
  ylab("Attack rate 150-159 years since the pandemic\n(proportion of attack rate in pandemic year)")+
  theme_bw()+
  theme(legend.position = c(0.7,0.55),
        legend.background = element_rect(colour = 'black'))

pltAS_R2C

pltAS_R1A<-pltAS_R1A+labs(tag="A")+
  theme(plot.tag.position=c(0.005,0.98),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))
pltAS_R2B<-pltAS_R2B+labs(tag="B")+
  theme(plot.tag.position=c(0.005,0.98),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))
pltAS_R2C<-pltAS_R2C+labs(tag="C")+
  theme(plot.tag.position=c(0.005,0.98),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14))

plt_ASR_col <- plot_grid(pltAS_R1A, pltAS_R2B, nrow=2, rel_heights = c(1,0.6))
plot_grid(plt_ASR_col, pltAS_R2C, ncol=2, rel_widths = c(1,0.5))
ggsave('figures/LongtermImmunity_main.pdf', width=16, height=9.7)
