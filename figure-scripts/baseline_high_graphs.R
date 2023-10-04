# Set location
setwd('/flu-long-term-agent/figure_scripts')

# Load plotting functions
source('plot_functions.R')

# Specify simulation parameters (obtained from main.py)
pop_size <- 80000 
N_years <- 160 
N_iter <- 256 


#Load in data
dat1 <- read.table('input_data/AttackRates/baseline_high.csv', header=F, sep=',')



################################################################################################################################
# Mean dynamics by time figure 
################################################################################################################################
# Overall
AR1 <- attack_rate_over_time(dat = dat1, pop_size, N_years, N_iter)
AR_1 <- AR1[[2]][AR1[[2]]$TSI<=160,]
ARD <- attack_rate_over_decade(dat = dat1, pop_size, N_years, N_iter)
AR_D <- ARD[[2]][ARD[[2]]$TSI<=160,]

# By age-groups
A_AR_00 <- age_attack_rate_by_tsi(dat = dat1,pop_size, N_years, N_iter, tsi=1)
A_AR_01 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=11)
A_AR_02 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=31)
A_AR_03 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=51)
A_AR_04 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=71)
A_AR_05 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=151)

A_AR_00[[2]]$tsi <- "Pandemic year"
A_AR_01[[2]]$tsi <- "10-19 years"
A_AR_02[[2]]$tsi <- "30-39 years"
A_AR_03[[2]]$tsi <- "50-59 years"
A_AR_04[[2]]$tsi <- "70-79 years"
A_AR_05[[2]]$tsi <- "150-159 years"

A_AR_00[[3]]$tsi <- "Pandemic year"
A_AR_01[[3]]$tsi <- "10-19 years"
A_AR_02[[3]]$tsi <- "30-39 years"
A_AR_03[[3]]$tsi <- "50-59 years"
A_AR_04[[3]]$tsi <- "70-79 years"
A_AR_05[[3]]$tsi <- "150-159 years"

age_df <- rbind(A_AR_00[[2]],A_AR_01[[2]],A_AR_01[[2]],A_AR_02[[2]],A_AR_03[[2]],A_AR_04[[2]],A_AR_05[[2]])
age_df$tsi <- factor(age_df$tsi, levels=c("Pandemic year","10-19 years","30-39 years","50-59 years","70-79 years","150-159 years"))
age_df_grp <- rbind(A_AR_00[[3]],A_AR_01[[3]],A_AR_01[[3]],A_AR_02[[3]],A_AR_03[[3]],A_AR_04[[3]],A_AR_05[[3]])
age_df_grp$tsi <- factor(age_df_grp$tsi, levels=c("Pandemic year","10-19 years","30-39 years","50-59 years","70-79 years","150-159 years"))

age_df_comp <- age_df_grp
age_df_comp[age_df_comp$tsi=="10-19 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="10-19 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="30-39 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="30-39 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="50-59 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="50-59 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="70-79 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="70-79 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="150-159 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="150-159 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR

age_df_comp[age_df_comp$tsi=="10-19 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="10-19 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="30-39 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="30-39 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="50-59 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="50-59 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="70-79 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="70-79 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="150-159 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="150-159 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp <- age_df_comp[age_df_comp$tsi!="Pandemic year",]


age_df_comp <- age_df
age_df_comp[age_df_comp$tsi=="5-9 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="5-9 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="10-19 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="10-19 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="30-39 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="30-39 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="50-59 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="50-59 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="70-79 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="70-79 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="150-159 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="150-159 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR

age_df_comp[age_df_comp$tsi=="5-9 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="5-9 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="10-19 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="10-19 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="30-39 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="30-39 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="50-59 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="50-59 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="70-79 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="70-79 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="150-159 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="150-159 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp <- age_df_comp[age_df_comp$tsi!="Pandemic year",]


# Plot of attack rate over time by year
AR_1$TSI <- AR_1$TSI-1
plt1A<-ggplot()+
  geom_line(data = AR_1, aes(x=TSI, y=mn_AR), alpha=0.5)+
  geom_point(data = AR_1, aes(x=TSI, y=mn_AR))+
  geom_ribbon(data = AR_1, aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR), alpha=0.5)+
  #geom_jitter(data=AR1[[1]], aes(x=TSI, y=AR), alpha=0.2, width=0.2, height=0)+
  #geom_errorbar(data=AR_1, aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual attack rate")+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  theme_bw(base_size = 12)

# Plot of attack rate over time by decade
AR_D$TSI <- AR_D$TSI-1

ARD_8yr <- attack_rate_over_eightyr(dat = dat1, pop_size, N_years, N_iter)
ARD_8yr[[2]]$TSI[1] <-2
ts1a <- AR_1[1,]
ts1b <- AR_1[1,]
ts1b$TSI<-1

ts2a <- AR_1[2,]
ts2b <- AR_1[2,]
ts2b$TSI<-2

ts <- rbind(ts1a, ts1b, ts2a, ts2b)
ts$dec <- c(-1,-1,0,0)
AR_D$TSI[seq(2,32,by=2)] <- AR_D$TSI[seq(2,32,by=2)]+1
AR_D_ts <- rbind(AR_D[3:nrow(AR_D),], ARD_8yr[[2]])

plt1B<- ggplot()+
  geom_line(data = AR_D_ts, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec))+
  geom_ribbon(data = AR_D_ts, aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec), alpha=0.5)+
  #geom_jitter(data=AR1[[1]], aes(x=TSI, y=AR), alpha=0.2, width=0.2, height=0)+
  #geom_errorbar(data=ARD[[2]], aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual attack rate")+
  scale_x_continuous(breaks=c(0,20,40,60,80,100, 120, 140,160))+
  theme_bw(base_size = 12)

# Plot of attack rate by age group
plt1C<-ggplot()+
  geom_line(data=age_df, aes(x=age, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tsi, color=tsi))+
  geom_ribbon(data=age_df, aes(x=age, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tsi, fill=tsi),alpha=0.5)+
  scale_color_brewer("Years since pandemic",palette='Dark2')+
  scale_fill_brewer("Years since pandemic",palette="Dark2")+
  xlab("Age (years)")+
  ylab("Average annual attack rate")+
  theme_bw(base_size = 12)+
  theme(legend.position = c(0.79,0.77),
        legend.background = element_rect(colour = 'black'))

# Plot of porportional difference in attack rate by age-group compared to pandemic year
col_custom <- RColorBrewer::brewer.pal(7, "Dark2")[2:7]
plt1D<-ggplot()+
  geom_line(data=age_df_comp, aes(x=age, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tsi, color=tsi))+
  geom_ribbon(data=age_df_comp, aes(x=age, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tsi, fill=tsi),alpha=0.5)+
  scale_color_manual("Years since pandemic",values=col_custom)+
  scale_fill_manual("Years since pandemic",values=col_custom)+
  xlab("Age (years)")+
  ylab("Attack rate (proportion of value in pandemic year)")+
  theme_bw(base_size = 12)+
  theme(legend.position = c(0.27,0.8),
        legend.background = element_rect(colour = 'black'))



plt1A<-plt1A+labs(tag="A")+
  theme(plot.tag.position=c(0.005,0.95))
plt1B<-plt1B+labs(tag="B")+
  theme(plot.tag.position=c(0.02,1))
plt1C<-plt1C+labs(tag="C")+
  theme(plot.tag.position=c(0.02,1),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8))
plt1D<-plt1D+labs(tag="D")+
  theme(plot.tag.position=c(0.02,1),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8))

mid_row <- plot_grid(plt1B, plt1C, plt1D, nrow=1, rel_widths = c(1,1,1))

plt_final1 <- plot_grid(plt1A, mid_row, nrow=2, rel_heights = c(1,1.2))

ggsave('figures/Figure3_high.pdf', width=12, height=8.5)
