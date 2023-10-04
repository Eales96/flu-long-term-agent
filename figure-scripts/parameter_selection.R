# Set location
setwd('C:/Users/EALESO/PycharmProjects/pythonProject/figure_scripts')

# Load plotting functions
source('plot_functions.R')


# Specify simulation parameters (obtained from main.py)
pop_size <- 80000 #160000
N_years <- 40 #160
N_iter <- 20 #128


#Load in data

dat1 <- read.table('input_data/ParamSelection/parameter_selection_ar.csv', header=F, sep=',')
dat2 <- read.table('input_data/ParamSelection/parameter_selection_pk_wk.csv', header=F, sep=',')




pars <- data.frame()
count = 0
for(i in 0:10){
  seas = 0.1 + 0.02*i
  for(j in 0:10){
    count = count+1
    beta = 4.5 + 0.2*j
    pars <- rbind(pars, data.frame(seas=seas, beta = beta, par_set = count))
  }
}




ps_attack_rate <- function(dat, pop_size, N_years, N_iter){
  df <- data.frame()
  
  for(i in seq_len(nrow(dat))){
    
    AR = sum(dat[i,])/pop_size
    TSI = (i-1) %% N_years +1
    sim = (i-1) %/% N_years +1
    par_set = (i-1) %/% (N_iter*N_years) +1
    
    
    row_df <- data.frame(AR = AR,
                         TSI = TSI,
                         sim = sim,
                         par_set=par_set)
    df <- rbind(df, row_df)
  }
  
  return(df)
}


ps_pk_wk_sd <- function(dat, pop_size, N_years, N_iter, min_yr=21, max_yr=40){
  mn_df <- data.frame()
  
  for(i in seq_len(length(unique(pars$par_set)))){
    min_index <- 1+(i-1)*N_iter
    max_index <- i*N_iter
    
    tmp <- as.matrix(dat[min_index:max_index, min_yr:max_yr])
    
    hist(tmp, breaks=20)
    sd(tmp)
    
    mn = mean(tmp)
    lwr = quantile(tmp, c(0.025, 0.975))[[1]]
    upr = quantile(tmp, c(0.025, 0.975))[[2]]
    sd = sd(tmp)
    
    row_df <- data.frame(par_set = (i),
                         mn_pk = mn,
                         lwr_pk = lwr,
                         upr_pk = upr,
                         sd = sd)
    
    mn_df <- rbind(mn_df, row_df)
    
  }
  
  return(mn_df)
}



ps_avg_attack_rate <- function(df, pop_size, N_years, N_iter, min_yr=21, max_yr=40){
  mn_df <- data.frame()
  
  
  for(i in seq_len(length(unique(df$par_set)))){
    
    tmp = df[df$TSI %in% seq(min_yr, max_yr) & df$par_set == i,]
    mn = mean(tmp$AR)
    lwr = quantile(tmp$AR, c(0.025, 0.975))[[1]]
    upr = quantile(tmp$AR, c(0.025, 0.975))[[2]]
    sem = sd(tmp$AR)/sqrt(N_iter)#sqrt(nrow(tmp))
    
    row_df <- data.frame(par_set = (i),
                         mn_AR = mn,
                         lwr_AR = lwr,
                         upr_AR = upr,
                         sem_AR = sem)
    
    mn_df <- rbind(mn_df, row_df)
    
  }
  
  return(mn_df)
}


ar <- ps_attack_rate(dat1, pop_size, N_years, N_iter)
avg_ar <- ps_avg_attack_rate(ar, pop_size, N_years, N_iter, min_yr = 1, max_yr = 40)
pk_wk <- ps_pk_wk_sd(dat2, pop_size, N_years, N_iter, min_yr = 21, max_yr = 40)



avg_ar$beta <- pars$beta
avg_ar$seas <- pars$seas

pk_wk$beta <- pars$beta
pk_wk$seas <- pars$seas

library(mgcv)
mod1<-gam(mn_AR ~ s(beta,seas), data=avg_ar, family = gaussian())
mod2<-gam(sd ~ s(beta,seas), data=pk_wk, family = gaussian())

new_dat<-expand.grid(beta=seq(4.5,6.5,0.01), seas=seq(0.1,0.3,0.001))


new_dat$AR <- predict(mod1, newdata = new_dat)*100
new_dat$pk_wk_sd <- predict(mod2, newdata = new_dat)

library(metR)

plt1<-ggplot()+
  geom_tile(data=new_dat, aes(x=beta, y=seas, fill=AR, z=AR))+
  geom_point(data=pars, aes(x=beta,y=seas))+
  coord_cartesian(xlim=c(4.585,6.41), ylim=c(0.109,0.2915))+
  geom_contour(data=new_dat, aes(x=beta, y=seas, fill=AR, z=AR), breaks=c(15,20,22.5), col='white')+
  geom_label_contour(data=new_dat, aes(x=beta, y=seas, fill=AR, z=AR), breaks=c(15,20,22.5), skip=0, label.placer = label_placer_fraction())+
  theme_bw()+
  scale_fill_viridis_c("Average annual\nattack rate (%)", option="mako")+
  ylab(expression(paste("Seasonality parameter (", beta[1],")")))+
  xlab(expression(paste("Contact rate parameter (", beta[0],")")))+
  geom_point(aes(x=5.5,y=0.25), col='red2', shape=17, size=4)+
  geom_point(aes(x=5.,y=0.25), col='orange2', shape=17, size=4)+
  geom_point(aes(x=5.75,y=0.25), col='orange2', shape=17, size=4)

plt1
plt2<-ggplot()+
  geom_tile(data=new_dat, aes(x=beta, y=seas, fill=pk_wk_sd, z=pk_wk_sd))+
  geom_point(data=pars, aes(x=beta,y=seas))+
  coord_cartesian(xlim=c(4.585,6.41), ylim=c(0.109,0.2915))+
  geom_contour(data=new_dat, aes(x=beta, y=seas, fill=pk_wk_sd, z=pk_wk_sd), breaks=c(2.5,3,3.5, 4, 4.5, 5.0, 5.5, 6, 6.5), col='white')+
  geom_label_contour(data=new_dat, aes(x=beta, y=seas, fill=pk_wk_sd, z=pk_wk_sd), breaks=c(2.5,3,3.5, 4, 4.5, 5.0,5.5,6), skip=0, label.placer = label_placer_fraction())+
  theme_bw()+
  scale_fill_viridis_c("Standard\ndeviation\nin peak\nweek (weeks)", option="mako")+
  ylab(expression(paste("Seasonality parameter (", beta[1],")")))+
  xlab(expression(paste("Contact rate parameter (", beta[0],")")))+
  geom_point(aes(x=5.5,y=0.25), col='red2', shape=17, size=4)+
  geom_point(aes(x=5.,y=0.25), col='orange2', shape=17, size=4)+
  geom_point(aes(x=5.75,y=0.25), col='orange2', shape=17, size=4)


expression(paste("Seasonality parameter (", beta_1,")"))

plt1 <- plt1+labs(tag = "A")+
  theme(plot.tag.position = c(0.02,0.97))

plt2 <- plt2+labs(tag = "B")+
  theme(plot.tag.position = c(0.02,0.97))


library(cowplot)
plot_grid(plt1, plt2, ncol=2)

ggsave('figures/Baseline/parameter_selection_graph.pdf', width=12, height=6)

