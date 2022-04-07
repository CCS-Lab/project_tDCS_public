data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1> B;
  int<lower=1, upper=T> Tsubj[N,2];
  real outcome[N, T, 2];
  int<lower=-1, upper=1> pressed[N, T, 2];
  int<lower=1, upper=4> cue[N, T, 2];
}
transformed data {
  vector[4] initV;
  initV = rep_vector(0.0, 4);
} 
parameters {
  // declare as vectors for vectorizing
  vector[8] mu_p;  
  vector<lower=0>[8] sigma; 
  vector[N] xi_pr;        // noise 
  vector[N] ep_pr;        // learning rate 
  vector[N] b_0_pr;       // baseline go bias 
  vector[N] b_delta_pr;   // change go bias 
  vector[N] pi_0_pr;      // base pavlovian bias
  vector[N] pi_delta_pr;  // change pavlovian bias 
  vector[N] rhoRew_pr;    // rho reward, inv temp
  vector[N] rhoPun_pr;    // rho punishment, inv temp 
}
transformed parameters {
  vector<lower=0,upper=1>[N] xi;
  vector<lower=0,upper=1>[N] ep;
  vector[N] b_0; 
  vector[N] b_delta; 
  vector[N] pi_0;
  vector[N] pi_delta;
  vector<lower=0>[N] rhoRew;
  vector<lower=0>[N] rhoPun;
  
  vector[N] log_lik;
     
  for (i in 1:N) {
    xi[i] = Phi_approx( mu_p[1] + sigma[1] * xi_pr[i] );
    ep[i] = Phi_approx( mu_p[2] + sigma[2] * ep_pr[i] );
  }
  b_0 = mu_p[3] + sigma[3] * b_0_pr; // vectorization
  b_delta  = mu_p[4] + sigma[4] * b_delta_pr;
  pi_0  = mu_p[5] + sigma[5] * pi_0_pr;
  pi_delta  = mu_p[6] + sigma[6] * pi_delta_pr;
  rhoRew = exp( mu_p[7] + sigma[7] * rhoRew_pr );
  rhoPun = exp( mu_p[8] + sigma[8] * rhoPun_pr );
  
  {
    for (i in 1:N) {
      vector[4] wv_g;  // action wegith for go
      vector[4] wv_ng; // action wegith for nogo
      vector[4] qv_g;  // Q value for go
      vector[4] qv_ng; // Q value for nogo
      vector[4] sv;    // stimulus value 
      vector[4] pGo;   // prob of go (press)
      
      real go_bias;
      real pav_bias;
      int cue_t;
      real outcome_t;
      
      log_lik[i] = 0;
      
      for (blk in 1:B) {
        wv_g  = initV;
        wv_ng = initV;
        qv_g  = initV;
        qv_ng = initV;
        sv    = initV;
        
        if (blk==1) {
          go_bias = b_0[i];
          pav_bias = pi_0[i];
        } else if (blk==2) {
          go_bias = b_0[i] + b_delta[i];
          pav_bias = pi_0[i] + pi_delta[i];
        }
        
        for (t in 1:Tsubj[i,blk])  {
          cue_t = cue[i,t,blk];
          outcome_t = outcome[i,t,blk];
          
          wv_g[ cue_t ]  = qv_g[ cue_t ] + go_bias + pav_bias * sv[ cue_t ];
          wv_ng[ cue_t ] = qv_ng[ cue_t ];  // qv_ng is always equal to wv_ng (regardless of action)      
          pGo[ cue_t ]   = inv_logit( wv_g[ cue_t ] - wv_ng[ cue_t ] ); 
          pGo[ cue_t ]   = pGo[ cue_t ] * (1 - xi[i]) + xi[i]/2;  // noise
          log_lik[i] += bernoulli_lpmf( pressed[i,t,blk] | pGo[ cue_t ] );
          
          // after receiving feedback, update sv[t+1]
          if (outcome_t >= 0) {
            sv[ cue_t ] = sv[ cue_t ] + ep[i] * ( rhoRew[i] * outcome_t - sv[ cue_t ] );
          } else {
            sv[ cue_t ] = sv[ cue_t ] + ep[i] * ( rhoPun[i] * outcome_t - sv[ cue_t ] );
          }
  
          // update action values
          if (pressed[i,t,blk]) { // update go value 
            if (outcome_t >=0) {
              qv_g[ cue_t ]  = qv_g[ cue_t ] + ep[i] * ( rhoRew[i] * outcome_t - qv_g[ cue_t ]);
            } else {
              qv_g[ cue_t ]  = qv_g[ cue_t ] + ep[i] * ( rhoPun[i] * outcome_t - qv_g[ cue_t ]);
            }
          } else { // update no-go value  
            if (outcome_t >=0) {
              qv_ng[ cue_t ] = qv_ng[ cue_t ] + ep[i] * ( rhoRew[i] * outcome_t - qv_ng[ cue_t ]);  
            } else{
              qv_ng[ cue_t ] = qv_ng[ cue_t ] + ep[i] * ( rhoPun[i] * outcome_t - qv_ng[ cue_t ]);  
            }
          }  
        } // end of t loop
      } // end of b loop
    } // end of i loop
  }
}
model {  
// gng_m4: RW(rew/pun) + noise + bias + pi model (M5 in Cavanagh et al 2013 J Neuro)
  // hyper parameters
  mu_p[1]  ~ normal(0, 1.0); 
  mu_p[2]  ~ normal(0, 1.0); 
  mu_p[3]  ~ normal(0, 10.0); 
  mu_p[4]  ~ normal(0, 1.0); 
  mu_p[5]  ~ normal(0, 10.0); 
  mu_p[6]  ~ normal(0, 1.0); 
  mu_p[7]  ~ normal(0, 1.0); 
  mu_p[8]  ~ normal(0, 1.0); 
  
  sigma[1:2] ~ normal(0, 0.2);
  sigma[3] ~ cauchy(0, 1.0);
  sigma[4] ~ normal(0, 0.2);
  sigma[5] ~ cauchy(0, 1.0);
  sigma[6] ~ normal(0, 0.2);
  sigma[7:8] ~ normal(0, 0.2);
  
  // individual parameters w/ Matt trick
  xi_pr       ~ normal(0, 1.0);   
  ep_pr       ~ normal(0, 1.0);   
  b_0_pr      ~ normal(0, 1.0); 
  b_delta_pr  ~ normal(0, 1.0); 
  pi_0_pr     ~ normal(0, 1.0); 
  pi_delta_pr ~ normal(0, 1.0); 
  rhoRew_pr   ~ normal(0, 1.0);
  rhoPun_pr   ~ normal(0, 1.0);
  
  target += sum(log_lik);
}
generated quantities {
  real<lower=0, upper=1> mu_xi;
  real<lower=0, upper=1> mu_ep;
  real mu_b_0;
  real mu_b_delta;
  real mu_pi_0;
  real mu_pi_delta;
  real<lower=0> mu_rhoRew;
  real<lower=0> mu_rhoPun;
  
  mu_xi  = Phi_approx(mu_p[1]);
  mu_ep  = Phi_approx(mu_p[2]);
  mu_b_0   = mu_p[3];
  mu_b_delta  = mu_p[4];
  mu_pi_0  = mu_p[5];
  mu_pi_delta  = mu_p[6];
  mu_rhoRew = exp(mu_p[7]);
  mu_rhoPun = exp(mu_p[8]); 
}
