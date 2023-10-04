library(ggplot2)
library(RColorBrewer)
library(grid)
library(cowplot)
library(conmat)
library(tidyr)

# Set location
setwd('C:/Users/EALESO/PycharmProjects/pythonProject/figure_scripts')


attack_rate_over_time <- function(dat, pop_size, N_years, N_iter){
  df <- data.frame()
  
  for(i in seq_len(nrow(dat))){
    
    AR = sum(dat[i,])/pop_size
    TSI = (i-1) %% N_years +1
    sim = (i-1) %/% N_years
    
    
    row_df <- data.frame(AR = AR,
                         TSI = TSI,
                         sim = sim)
    df <- rbind(df, row_df)
  }
  
  mn_df <- data.frame()
  
  for(i in seq_len(N_years)){
    
    tmp = df[df$TSI==(i),]
    mn = mean(tmp$AR)
    lwr = quantile(tmp$AR, c(0.025, 0.975))[[1]]
    upr = quantile(tmp$AR, c(0.025, 0.975))[[2]]
    sem = sd(tmp$AR)/sqrt(N_iter)#sqrt(nrow(tmp))
    
    row_df <- data.frame(TSI = (i),
                         mn_AR = mn,
                         lwr_AR = lwr,
                         upr_AR = upr,
                         sem_AR = sem)
    
    mn_df <- rbind(mn_df, row_df)
    
  }
  
  return(list(df, mn_df))
}



attack_rate_over_decade <- function(dat, pop_size, N_years, N_iter){
  df <- data.frame()
  
  for(i in seq_len(nrow(dat))){
    
    AR = sum(dat[i,])/pop_size
    TSI = (i-1) %% N_years +1
    sim = (i-1) %/% N_years
    
    
    row_df <- data.frame(AR = AR,
                         TSI = TSI,
                         sim = sim)
    df <- rbind(df, row_df)
  }
  
  mn_df <- data.frame()
  
  for(i in seq_len(N_years/10)){
    
    tmp = df[df$TSI %in% seq(1+(i-1)*10,10*i,1),]
    mn = mean(tmp$AR)
    lwr = quantile(tmp$AR, c(0.025, 0.975))[[1]]
    upr = quantile(tmp$AR, c(0.025, 0.975))[[2]]
    sem = sd(tmp$AR)/sqrt(nrow(tmp))
    
    row_df1 <- data.frame(TSI = (1+(i-1)*10),
                          mn_AR = mn,
                          lwr_AR = lwr,
                          upr_AR = upr,
                          sem_AR = sem,
                          dec=i)
    row_df2 <- data.frame(TSI = (i*10),
                          mn_AR = mn,
                          lwr_AR = lwr,
                          upr_AR = upr,
                          sem_AR = sem,
                          dec=i)
    
    mn_df <- rbind(mn_df, row_df1, row_df2)
    
  }
  
  return(list(df, mn_df))
}

attack_rate_over_eightyr <- function(dat, pop_size, N_years, N_iter){
  df <- data.frame()
  
  for(i in seq_len(nrow(dat))){
    
    AR = sum(dat[i,])/pop_size
    TSI = (i-1) %% N_years +1
    sim = (i-1) %/% N_years
    
    
    row_df <- data.frame(AR = AR,
                         TSI = TSI,
                         sim = sim)
    df <- rbind(df, row_df)
  }
  
  mn_df <- data.frame()
  
    tmp = df[df$TSI %in% seq(3,10,1),]
    mn = mean(tmp$AR)
    lwr = quantile(tmp$AR, c(0.025, 0.975))[[1]]
    upr = quantile(tmp$AR, c(0.025, 0.975))[[2]]
    sem = sd(tmp$AR)/sqrt(nrow(tmp))
    
    row_df1 <- data.frame(TSI = 3,
                          mn_AR = mn,
                          lwr_AR = lwr,
                          upr_AR = upr,
                          sem_AR = sem,
                          dec=i)
    row_df2 <- data.frame(TSI = 10,
                          mn_AR = mn,
                          lwr_AR = lwr,
                          upr_AR = upr,
                          sem_AR = sem,
                          dec=i)
    
    mn_df <- rbind(mn_df, row_df1, row_df2)

  
  return(list(df, mn_df))
}


age_attack_rate_by_tsi <- function(dat, pop_size, N_years, N_iter, tsi){
  df <- data.frame()
  
  for(i in seq_len(N_iter)){
    index <- (i-1)*N_years + tsi 
    for(j in seq_len(80)){
      AR = dat[index, j] * 80/pop_size
      sim = i
      age = j-1
      
      row_df <- data.frame(AR = AR,
                           age = age,
                           sim = sim)
      df <- rbind(df, row_df)
      
    }
  }
  
  
  mn_df <- data.frame()
  
  for(i in seq_len(80)){
    
    tmp = df[df$age==i-1,]
    mn = mean(tmp$AR)
    lwr = quantile(tmp$AR, c(0.025, 0.975))[[1]]
    upr = quantile(tmp$AR, c(0.025, 0.975))[[2]]
    sem = sd(tmp$AR)/sqrt(nrow(tmp))#sqrt(N_iter)
    
    row_df <- data.frame(age = i-1,
                         mn_AR = mn,
                         lwr_AR = lwr,
                         upr_AR = upr,
                         sem_AR = sem,
                         tsi = as.factor(tsi))
    
    mn_df <- rbind(mn_df, row_df)
    
  }
  
  mn_df_grp <- data.frame()
  
  for(i in seq_len(80/5)){
    
    tmp = df[df$age%in%seq((i-1)*5,-1+(i)*5),]
    mn = mean(tmp$AR)
    
    
    lwr = quantile(tmp$AR, c(0.025, 0.975))[[1]]
    upr = quantile(tmp$AR, c(0.025, 0.975))[[2]]
    sem = sd(tmp$AR)/sqrt(nrow(tmp))#sqrt(N_iter)
    
    row_df1 <- data.frame(age = (i-1)*5,
                          mn_AR = mn,
                          lwr_AR = lwr,
                          upr_AR = upr,
                          sem_AR = sem,
                          tsi = as.factor(tsi))
    row_df2 <- data.frame(age = -1+(i)*5,
                          mn_AR = mn,
                          lwr_AR = lwr,
                          upr_AR = upr,
                          sem_AR = sem,
                          tsi = as.factor(tsi))
    
    mn_df_grp <- rbind(mn_df_grp, row_df1, row_df2)
    
  }
  
  
  return(list(df, mn_df, mn_df_grp))
}



age_attack_rate_by_decade <- function(dat, pop_size, N_years, N_iter, tsi){
  df <- data.frame()
  
  for(i in seq_len(N_iter)){
    index <- (i-1)*N_years + tsi 
    for(j in seq_len(80)){
      AR = dat[index, j] * 80/pop_size
      sim = i
      age = j-1
      
      row_df <- data.frame(AR = AR,
                           age = age,
                           sim = sim)
      df <- rbind(df, row_df)
      
    }
  }
  for(k in 0:9){
    for(i in seq_len(N_iter)){
      index <- (i-1)*N_years + tsi +k
      for(j in seq_len(80)){
        AR = dat[index, j] * 80/pop_size
        sim = i
        age = j-1
        
        row_df <- data.frame(AR = AR,
                             age = age,
                             sim = sim)
        df <- rbind(df, row_df)
        
      }
    }
    
  }
  
  mn_df <- data.frame()
  
  for(i in seq_len(80)){
    
    tmp = df[df$age==i-1,]
    mn = mean(tmp$AR)
    lwr = quantile(tmp$AR, c(0.025, 0.975))[[1]]
    upr = quantile(tmp$AR, c(0.025, 0.975))[[2]]
    sem = sd(tmp$AR)/sqrt(nrow(tmp))
    
    row_df <- data.frame(age = i-1,
                         mn_AR = mn,
                         lwr_AR = lwr,
                         upr_AR = upr,
                         sem_AR = sem,
                         tsi = as.factor(tsi))
    
    mn_df <- rbind(mn_df, row_df)
    
  }
  
  mn_df_grp <- data.frame()
  
  for(i in seq_len(80/5)){
    
    tmp = df[df$age%in%seq((i-1)*5,-1+(i)*5),]
    mn = mean(tmp$AR)
    
    
    lwr = quantile(tmp$AR, c(0.025, 0.975))[[1]]
    upr = quantile(tmp$AR, c(0.025, 0.975))[[2]]
    sem = sd(tmp$AR)/sqrt(nrow(tmp))#sqrt(N_iter)
    
    row_df1 <- data.frame(age = (i-1)*5,
                          mn_AR = mn,
                          lwr_AR = lwr,
                          upr_AR = upr,
                          sem_AR = sem,
                          tsi = as.factor(tsi))
    row_df2 <- data.frame(age = -1+(i)*5,
                          mn_AR = mn,
                          lwr_AR = lwr,
                          upr_AR = upr,
                          sem_AR = sem,
                          tsi = as.factor(tsi))
    
    mn_df_grp <- rbind(mn_df_grp, row_df1, row_df2)
    
  }
  
  
  return(list(df, mn_df, mn_df_grp))
}




age_attack_rate_by_halfdec <- function(dat, pop_size, N_years, N_iter, tsi){
  df <- data.frame()
  
  for(i in seq_len(N_iter)){
    index <- (i-1)*N_years + tsi 
    for(j in seq_len(80)){
      AR = dat[index, j] * 80/pop_size
      sim = i
      age = j-1
      
      row_df <- data.frame(AR = AR,
                           age = age,
                           sim = sim)
      df <- rbind(df, row_df)
      
    }
  }
  for(k in 0:4){
    for(i in seq_len(N_iter)){
      index <- (i-1)*N_years + tsi +k
      for(j in seq_len(80)){
        AR = dat[index, j] * 80/pop_size
        sim = i
        age = j-1
        
        row_df <- data.frame(AR = AR,
                             age = age,
                             sim = sim)
        df <- rbind(df, row_df)
        
      }
    }
    
  }
  
  mn_df <- data.frame()
  
  for(i in seq_len(80)){
    
    tmp = df[df$age==i-1,]
    mn = mean(tmp$AR)
    lwr = quantile(tmp$AR, c(0.025, 0.975))[[1]]
    upr = quantile(tmp$AR, c(0.025, 0.975))[[2]]
    sem = sd(tmp$AR)/sqrt(nrow(tmp))
    
    row_df <- data.frame(age = i-1,
                         mn_AR = mn,
                         lwr_AR = lwr,
                         upr_AR = upr,
                         sem_AR = sem,
                         tsi = as.factor(tsi))
    
    mn_df <- rbind(mn_df, row_df)
    
  }
  
  mn_df_grp <- data.frame()
  
  for(i in seq_len(80/5)){
    
    tmp = df[df$age%in%seq((i-1)*5,-1+(i)*5),]
    mn = mean(tmp$AR)
    
    
    lwr = quantile(tmp$AR, c(0.025, 0.975))[[1]]
    upr = quantile(tmp$AR, c(0.025, 0.975))[[2]]
    sem = sd(tmp$AR)/sqrt(nrow(tmp))#sqrt(N_iter)
    
    row_df1 <- data.frame(age = (i-1)*5,
                          mn_AR = mn,
                          lwr_AR = lwr,
                          upr_AR = upr,
                          sem_AR = sem,
                          tsi = as.factor(tsi))
    row_df2 <- data.frame(age = -1+(i)*5,
                          mn_AR = mn,
                          lwr_AR = lwr,
                          upr_AR = upr,
                          sem_AR = sem,
                          tsi = as.factor(tsi))
    
    mn_df_grp <- rbind(mn_df_grp, row_df1, row_df2)
    
  }
  
  
  return(list(df, mn_df, mn_df_grp))
}



########################################################################################################################
# Correlation between years
########################################################################################################################

correlation_between_adjacent <- function(df, pop_size, N_iter, N_years, yr_dif){
  
  cor_df <- data.frame()
  for(i in seq_len(N_iter)){
    tmp <- df[df$sim==(i-1),]
    
    tmp$AR2 <- -1.0
    tmp$AR2[1:(nrow(tmp)-yr_dif)] <- tmp$AR[(1+yr_dif):nrow(tmp)]
    cor_df <- rbind(cor_df, tmp[tmp$AR2!=-1.0,])
  }
  

  return(cor_df)

}

cor_analysis <- function(dat, pop_size, N_iter, N_years, label){
  
  dfnew <- data.frame()
  for(i in seq_len(nrow(dat))){
    
    AR = sum(dat[i,])/pop_size
    TSI = (i-1) %% N_years +1
    sim = (i-1) %/% N_years
    
    
    row_df <- data.frame(AR = AR,
                         TSI = TSI,
                         sim = sim)
    dfnew <- rbind(dfnew, row_df)
  }
  
  df <- data.frame()
  
  for(i in seq_len(20)){
    
    cor_df <- correlation_between_adjacent(dfnew, pop_size, N_iter, N_years, yr_dif =i)
    cor_val <- cor.test(cor_df[cor_df$TSI>100,]$AR, cor_df[cor_df$TSI>100,]$AR2)
    
    row_df <- data.frame(cor = cor_val$estimate,
                         cor_lwr = cor_val$conf.int[1],
                         cor_upr = cor_val$conf.int[2],
                         p_val = cor_val$p.value,
                         yr_dif = i,
                         label = label)
    
    df <- rbind(df, row_df)
    
    
  }
  
  return(df)
}

autocorrelation_over_time <- function(dat, pop_size, N_iter, N_years){
  
  df <- data.frame()
  for(i in seq_len(nrow(dat))){
    
    AR = sum(dat[i,])/pop_size
    TSI = (i-1) %% N_years +1
    sim = (i-1) %/% N_years
    
    
    row_df <- data.frame(AR = AR,
                         TSI = TSI,
                         sim = sim)
    df <- rbind(df, row_df)
  }
  
  cor_df <- data.frame()
  for(i in seq_len(N_iter)){
    tmp <- df[df$sim==(i-1),]
    
    tmp$AR1 <- -1.0
    tmp$AR2 <- -1.0
    tmp$AR3 <- -1.0
    tmp$AR4 <- -1.0
    tmp$AR5 <- -1.0
    tmp$AR6 <- -1.0
    tmp$AR7 <- -1.0
    tmp$AR8 <- -1.0
    tmp$AR9 <- -1.0
    tmp$AR10 <- -1.0
    tmp$AR11 <- -1.0
    tmp$AR12 <- -1.0
    tmp$AR13 <- -1.0
    tmp$AR14 <- -1.0
    tmp$AR15 <- -1.0
    tmp$AR16 <- -1.0
    tmp$AR17 <- -1.0
    tmp$AR18 <- -1.0
    tmp$AR19 <- -1.0
    tmp$AR20 <- -1.0
    
    tmp$AR1[1:(nrow(tmp)-1)] <- tmp$AR[(1+1):nrow(tmp)]
    tmp$AR2[1:(nrow(tmp)-2)] <- tmp$AR[(1+2):nrow(tmp)]
    tmp$AR3[1:(nrow(tmp)-3)] <- tmp$AR[(1+3):nrow(tmp)]
    tmp$AR4[1:(nrow(tmp)-4)] <- tmp$AR[(1+4):nrow(tmp)]
    tmp$AR5[1:(nrow(tmp)-5)] <- tmp$AR[(1+5):nrow(tmp)]
    tmp$AR6[1:(nrow(tmp)-6)] <- tmp$AR[(1+6):nrow(tmp)]
    tmp$AR7[1:(nrow(tmp)-7)] <- tmp$AR[(1+7):nrow(tmp)]
    tmp$AR8[1:(nrow(tmp)-8)] <- tmp$AR[(1+8):nrow(tmp)]
    tmp$AR9[1:(nrow(tmp)-9)] <- tmp$AR[(1+9):nrow(tmp)]
    tmp$AR10[1:(nrow(tmp)-10)] <- tmp$AR[(1+10):nrow(tmp)]
    tmp$AR11[1:(nrow(tmp)-11)] <- tmp$AR[(1+11):nrow(tmp)]
    tmp$AR12[1:(nrow(tmp)-12)] <- tmp$AR[(1+12):nrow(tmp)]
    tmp$AR13[1:(nrow(tmp)-13)] <- tmp$AR[(1+13):nrow(tmp)]
    tmp$AR14[1:(nrow(tmp)-14)] <- tmp$AR[(1+14):nrow(tmp)]
    tmp$AR15[1:(nrow(tmp)-15)] <- tmp$AR[(1+15):nrow(tmp)]
    tmp$AR16[1:(nrow(tmp)-16)] <- tmp$AR[(1+16):nrow(tmp)]
    tmp$AR17[1:(nrow(tmp)-17)] <- tmp$AR[(1+17):nrow(tmp)]
    tmp$AR18[1:(nrow(tmp)-18)] <- tmp$AR[(1+18):nrow(tmp)]
    tmp$AR19[1:(nrow(tmp)-19)] <- tmp$AR[(1+19):nrow(tmp)]
    tmp$AR20[1:(nrow(tmp)-20)] <- tmp$AR[(1+20):nrow(tmp)]
    
    
    
    cor_df <- rbind(cor_df, tmp)
  }
  
  auto_cor_df <- data.frame()
  
  
  for(i in seq_len(N_years)){
    tmp<-cor_df[cor_df$TSI%in%c(i),]
    
    for(j in seq_len(20)){
      
      cor_val <- cor.test(tmp[,1], tmp[,(3+j)])
      
      row_df <- data.frame(cor = cor_val$estimate,
                           cor_lwr = cor_val$conf.int[1],
                           cor_upr = cor_val$conf.int[2],
                           p_val = cor_val$p.value,
                           year = i,
                           dif  = j)
      
      auto_cor_df <- rbind(auto_cor_df, row_df)
      
      
    }
    
    
  }
  
  return(auto_cor_df)
  
}


################################################################################################################################
# Dynamics by cohort figure 
################################################################################################################################


dynamics_by_cohort <- function(dat, pop_size, N_years, N_iter){
  
  df <- data.frame()
  
  for(i in seq_len(79+N_years)){
    print(i)
    birth_year = i-79
    
    min_yr = max(1, birth_year)
    max_yr = min(N_years, birth_year+79)
    
    for(l in seq_len(80/5)){
      ar <- c()
      yrs <- 0
      for(m in seq_len(5)){
        age <- (l - 1)*5  + (m-1)
        
        j <- age+birth_year+1
        
        for(k in seq_len(N_iter)){
          
          if(j<=N_years & j>0){
            yrs <- yrs+1
            ar <- c(ar, dat[j+(k-1)*N_years, age+1])
            
          }
          
          
          
        }
      }
      yrs <- yrs/N_iter
      
      if(yrs==0){
        row_df<-data.frame(birth_year = birth_year,
                           age_grp = l,
                           ar = 0,
                           lwr_ar = 0,
                           upr_ar = 0,
                           sem_ar = 0,
                           yrs = yrs)
      } else{
        
        ar <- yrs * ar * 80 / (pop_size)
        mn_ar <- mean(ar)
        lwr_ar = quantile(ar, c(0.025, 0.975))[[1]]
        upr_ar = quantile(ar, c(0.025, 0.975))[[2]]
        sem_ar<-sd(ar)/sqrt(length(ar))#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        
        row_df <- data.frame(birth_year = birth_year,
                             age_grp = l,
                             ar = mn_ar,
                             lwr_ar = lwr_ar,
                             upr_ar = upr_ar,
                             sem_ar = sem_ar,
                             yrs = yrs)
        
      }
      
      df <- rbind(df, row_df)
      
      
    }
    
    
  }
  
  return(df)
}


dynamics_by_cohort_age_range <- function(dat, pop_size, N_years, N_iter, min_age, max_age, age_grp){
  
  df <- data.frame()
  
  for(i in seq_len(79+N_years)){
    print(i)
    birth_year = i-79
    
    ar <- c()
    yrs <- 0
    for(m in seq_len(max_age-min_age+1)){
      age <- min_age-1+m
      
      j <- age+birth_year+1
      
      for(k in seq_len(N_iter)){
        
        if(j<=N_years & j>0){
          yrs <- yrs+1
          ar <- c(ar, dat[j+(k-1)*N_years, age+1])
          
        }
        
        
        
      }
    }
    yrs <- yrs/N_iter
    
    if(yrs==0){
      row_df<-data.frame(birth_year = birth_year,
                         age_grp = age_grp,
                         ar = 0,
                         lwr_ar = 0,
                         upr_ar = 0,
                         sem_ar = 0,
                         yrs = yrs)
    } else{
      
      ar <- yrs * ar * 80 / (pop_size)
      mn_ar <- mean(ar)
      lwr_ar = quantile(ar, c(0.025, 0.975))[[1]]
      upr_ar = quantile(ar, c(0.025, 0.975))[[2]]
      sem_ar<-sd(ar)/sqrt(length(ar))#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      
      row_df <- data.frame(birth_year = birth_year,
                           age_grp = age_grp,
                           ar = mn_ar,
                           lwr_ar = lwr_ar,
                           upr_ar = upr_ar,
                           sem_ar = sem_ar,
                           yrs = yrs)
      
    }
    
    df <- rbind(df, row_df)
    
  
  }
  
  return(df)
}



