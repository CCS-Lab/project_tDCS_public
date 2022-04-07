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
  vector[7] mu;
  vector<lower=0, upper=20>[7] sigma;
  
  vector<lower=-8,  upper=8>[N] xi;         // noise
  vector<lower=-8,  upper=8>[N] ep;         // learning rate
  vector<lower=-8,  upper=8>[N] b;          // go bias
  vector<lower=-8,  upper=8>[N] pi;         // pavlovian bias
  vector<lower=-10,  upper=10>[N] rhoRew;     // rho reward, inv temp
  vector<lower=-10,  upper=10>[N] rhoPun;     // rho punishment, inv temp
  vector<lower=-8,  upper=8>[N] k;          // instrumental learning bias
}

transformed parameters {
  
  vector[N] log_lik;
  
  {
    for (i in 1:N) {
      vector[4] wv_g;  // action weight for go
      vector[4] wv_ng; // action weight for nogo
      vector[4] qv_g;  // Q value for go
      vector[4] qv_ng; // Q value for nogo
      vector[4] sv;    // stimulus value
      vector[4] pGo;   // prob of go (press)
      
      real x; real rhoR; real rhoP;
      real ep_0; real epRew; real epPun;
      real lr;
      
      x     = inv_logit(xi[i]);
      rhoR  = exp(rhoRew[i]);
      rhoP  = exp(rhoPun[i]);
      ep_0    = inv_logit(ep[i]);
      epPun   = inv_logit(ep[i]-k[i]);
      epRew   = ep_0 + (ep_0 - epPun);
      
      log_lik[i] = 0;
      
      wv_g  = initV;
      wv_ng = initV;
      qv_g  = initV;
      qv_ng = initV;
      sv    = initV;
  
      for (t in 1:Tsubj[i]) {
        int cue_t;
        int pressed_t;
        real outcome_t;
        
        cue_t = cue[i, t];
        outcome_t = outcome[i, t];
        pressed_t = pressed[i, t];
        
        wv_g[cue_t]  = qv_g[cue_t] + b[i] + pi[i] * sv[cue_t];
        wv_ng[cue_t] = qv_ng[cue_t];  // qv_ng is always equal to wv_ng (regardless of action)
        pGo[cue_t]   = inv_logit(wv_g[cue_t] - wv_ng[cue_t]);
        {  // noise
          pGo[cue_t]   *= (1 - x);
          pGo[cue_t]   += x/2;
        }
        
        log_lik[i] += bernoulli_lpmf( pressed_t | pGo[ cue_t ] );
  
        // after receiving feedback, add instrumental learning bias
        if ((outcome_t > 0) && (pressed_t ==1)) {
          lr = epRew;
          } else if ((outcome_t < 0) && (pressed_t == 0)) {
            lr = epPun;
            } else{
              lr = ep_0;
              }
        
        // after receiving feedback, update sv[t + 1]
        if (outcome_t >= 0) {
          sv[cue_t] += lr * (rhoR * outcome_t - sv[cue_t]);
        } else {
          sv[cue_t] += lr * (rhoP * outcome_t - sv[cue_t]);
        }
  
        // update action values
        if (pressed_t) { // update go value
          if (outcome_t >=0) {
            qv_g[cue_t] += lr * (rhoR * outcome_t - qv_g[cue_t]);
          } else {
            qv_g[cue_t] += lr * (rhoP * outcome_t - qv_g[cue_t]);
          }
        } else { // update no-go value
          if (outcome_t >=0) {
            qv_ng[cue_t] += lr * (rhoR * outcome_t - qv_ng[cue_t]);
          } else {
            qv_ng[cue_t] += lr * (rhoP * outcome_t - qv_ng[cue_t]);
          }
        }
      } // end of t loop
    } // end of i loop
  }
}

model {
// gng_m4: RW(rew/pun) + noise + bias + pi model (M5 in Cavanagh et al 2013 J Neuro)
  // hyper parameters
  mu[1]  ~ normal(0, 1.0);
  mu[2]  ~ normal(0, 2.0);
  mu[3]  ~ normal(0, 3.0);
  mu[4]  ~ normal(0, 3.0);
  mu[5]  ~ normal(2, 3.0);
  mu[6]  ~ normal(2, 3.0);
  mu[7]  ~ normal(0, 2.0);
  sigma ~ cauchy(0, 2.0);
  
  // individual parameters w/ Matt trick
  xi     ~ normal(mu[1], sigma[1]);
  ep     ~ normal(mu[2], sigma[2]);
  b      ~ normal(mu[3], sigma[3]);
  pi     ~ normal(mu[4], sigma[4]);
  rhoRew ~ normal(mu[5], sigma[5]);
  rhoPun ~ normal(mu[6], sigma[6]);
  k      ~ normal(mu[7], sigma[7]);
  
  target += sum(log_lik);
}
