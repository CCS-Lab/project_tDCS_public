data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N];
  int<lower=1, upper=4> cue[N, T];
  int<lower=-1, upper=1> pressed[N, T];
  real outcome[N, T];
}

transformed data {
  vector[4] initV;
  initV = rep_vector(0.0, 4);
}

parameters {
  // declare as vectors for vectorizing
  vector[7] mu_pr;
  vector<lower=0>[7] sigma;
  vector[N] xi_pr;         // noise
  vector[N] ep_pr;         // learning rate
  vector[N] epRew_pr;         // learning rate
  vector[N] b_pr;          // go bias
  vector[N] pi_pr;         // pavlovian bias
  vector[N] rhoRew_pr;     // rho reward, inv temp
  vector[N] rhoPun_pr;     // rho punishment, inv temp
}

transformed parameters {
  vector<lower=0, upper=1>[N] xi;
  vector<lower=0, upper=1>[N] ep;
  vector<lower=0, upper=1>[N] epRew;
  vector[N] b;
  vector[N] pi;
  vector<lower=0>[N] rhoRew;
  vector<lower=0>[N] rhoPun;

  vector[N] log_lik;

  for (i in 1:N) {
    xi[i]     = Phi_approx(mu_pr[1] + sigma[1] * xi_pr[i]);
    ep[i]     = Phi_approx(mu_pr[2] + sigma[2] * ep_pr[i]);
    epRew[i]  = Phi_approx(mu_pr[3] + sigma[3] * epRew_pr[i]);
  }
  b      = mu_pr[4] + sigma[4] * b_pr; // vectorization
  pi     = mu_pr[5] + sigma[5] * pi_pr;
  rhoRew = exp(mu_pr[6] + sigma[6] * rhoRew_pr);
  rhoPun = exp(mu_pr[7] + sigma[7] * rhoPun_pr);
  
  {
    for (i in 1:N) {
      vector[4] wv_g;  // action weight for go
      vector[4] wv_ng; // action weight for nogo
      vector[4] qv_g;  // Q value for go
      vector[4] qv_ng; // Q value for nogo
      vector[4] sv;    // stimulus value
      vector[4] pGo;   // prob of go (press)
      
      real lr;
      
      int cue_t;
      int pressed_t;
      real outcome_t;
      
      log_lik[i] = 0;
      
      wv_g  = initV;
      wv_ng = initV;
      qv_g  = initV;
      qv_ng = initV;
      sv    = initV;
  
      for (t in 1:Tsubj[i]) {
        cue_t = cue[i, t];
        outcome_t = outcome[i, t];
        pressed_t = pressed[i, t];
        
        wv_g[cue_t]  = qv_g[cue_t] + b[i] + pi[i] * sv[cue_t];
        wv_ng[cue_t] = qv_ng[cue_t];  // qv_ng is always equal to wv_ng (regardless of action)
        pGo[cue_t]   = inv_logit(wv_g[cue_t] - wv_ng[cue_t]);
        {  // noise
          pGo[cue_t]   *= (1 - xi[i]);
          pGo[cue_t]   += xi[i]/2;
        }
        
        log_lik[i] += bernoulli_lpmf( pressed_t | pGo[ cue_t ] );
  
        // after receiving feedback, add instrumental learning bias
        if ((outcome_t > 0) && (pressed_t ==1)) {
          lr = epRew[i];
          } else if ((outcome_t < 0) && (pressed_t == 0)) {
            lr = ep[i];
            } else{
              lr = ep[i];
              }
        
        // after receiving feedback, update sv[t + 1]
        if (outcome_t >= 0) {
          sv[cue_t] += lr * (rhoRew[i] * outcome_t - sv[cue_t]);
        } else {
          sv[cue_t] += lr * (rhoPun[i] * outcome_t - sv[cue_t]);
        }
  
        // update action values
        if (pressed_t) { // update go value
          if (outcome_t >=0) {
            qv_g[cue_t] += lr * (rhoRew[i] * outcome_t - qv_g[cue_t]);
          } else {
            qv_g[cue_t] += lr * (rhoPun[i] * outcome_t - qv_g[cue_t]);
          }
        } else { // update no-go value
          if (outcome_t >=0) {
            qv_ng[cue_t] += lr * (rhoRew[i] * outcome_t - qv_ng[cue_t]);
          } else {
            qv_ng[cue_t] += lr * (rhoPun[i] * outcome_t - qv_ng[cue_t]);
          }
        }
      } // end of t loop
    } // end of i loop
  }
}

model {
// gng_m4: RW(rew/pun) + noise + bias + pi model (M5 in Cavanagh et al 2013 J Neuro)
  // hyper parameters
  mu_pr[1]  ~ normal(0, 1.0);
  mu_pr[2]  ~ normal(0, 1.0);
  mu_pr[3]  ~ normal(0, 1.0);
  mu_pr[4]  ~ normal(0, 10.0);
  mu_pr[5]  ~ normal(0, 10.0);
  mu_pr[6]  ~ normal(0, 1.0);
  mu_pr[7]  ~ normal(0, 1.0);
  sigma[1:3] ~ normal(0, 0.2);
  sigma[4:5] ~ cauchy(0, 1.0);
  sigma[6:7] ~ normal(0, 0.2);
  //sigma ~ cauchy(0, 1.0);

  // individual parameters w/ Matt trick
  xi_pr     ~ normal(0, 1.0);
  ep_pr     ~ normal(0, 1.0);
  epRew_pr  ~ normal(0, 1.0);
  b_pr      ~ normal(0, 1.0);
  pi_pr     ~ normal(0, 1.0);
  rhoRew_pr ~ normal(0, 1.0);
  rhoPun_pr ~ normal(0, 1.0);
  
  target += sum(log_lik);
}

generated quantities {
  real<lower=0, upper=1> mu_xi;
  real<lower=0, upper=1> mu_ep;
  real<lower=0, upper=1> mu_epRew;
  real mu_b;
  real mu_pi;
  real<lower=0> mu_rhoRew;
  real<lower=0> mu_rhoPun;

  mu_xi     = Phi_approx(mu_pr[1]);
  mu_ep     = Phi_approx(mu_pr[2]);
  mu_epRew  = Phi_approx(mu_pr[3]);
  mu_b      = mu_pr[4];
  mu_pi     = mu_pr[5];
  mu_rhoRew = exp(mu_pr[6]);
  mu_rhoPun = exp(mu_pr[7]);
}
