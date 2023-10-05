setwd('flu-long-term-agent/global-drift-model')

#Load packages
library(rstan)

# Settings for rstan
mc.cores = parallel::detectCores()
rstan_options(auto_write = TRUE)


dat<-read.csv('antigenic_data.csv')

num_data = length(dat$year)
num_years = max(dat$year)-min(dat$year)+1

input = list(num_data = num_data,
             yr = as.integer(dat$year+1-min(dat$year)),
             ag_x = dat$ag1,
             ag_y = dat$ag2,
             num_years = num_years)


scode <- "

data {
  int num_data;			// number of data points
  int yr[num_data];		// year of each data point
  vector[num_data] ag_x;	// year of each data point
  vector[num_data] ag_y;	// year of each data point
  
  int num_years;		// number of years total

}


parameters {
  real<lower=0> stepsizex[num_years-1];
  real stepsizey[num_years-1];
  real mu_x_init;
  real mu_y_init;
  real<lower=0> lambda;
  real<lower=0> theta;
  real<lower=0> sigma;
}


transformed parameters {
  vector[num_years] mu_x;
  vector[num_years] mu_y;
  
  vector[num_data] diff_x;
  vector[num_data] diff_y;
  
  mu_x[1] = mu_x_init;
  mu_y[1] = mu_y_init;
  for (i in 2:num_years)
    mu_x[i] = mu_x[i-1]+stepsizex[i-1];

  for (i in 2:num_years)
    mu_y[i] = mu_y[i-1]+stepsizey[i-1];

  for (i in 1:num_data)
    diff_x[i]  = mu_x[yr[i]] - ag_x[i];
  for (i in 1:num_data)
    diff_y[i]  = mu_y[yr[i]] - ag_y[i];

}

model {
  // Priors
  stepsizex ~ exponential(lambda);
  stepsizey ~ normal(0, theta);
  diff_x ~ normal(0, sigma);
  diff_y ~ normal(0, sigma);


}

"
writeLines(scode, "global_drift.stan")

fit_model <- rstan::stan('global_drift.stan',
                         iter=1000,
                         warmup =500,
                         chains=1,
                         control = list(adapt_delta=0.95,
                                        max_treedepth = 10),
                         data = input)


post <- rstan::extract(fit_model)

mean(post$lambda)
mean(post$theta)
mean(post$sigma)

saveRDS(post, 'posterior.rds')
