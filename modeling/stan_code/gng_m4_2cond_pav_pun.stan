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
  vector[N] b_pr;       // baseline go bias 
  vector[N] pi_rew_pr;      // base pavlovian bias
  vector[N] pi_0_pun_pr;      // base pavlovian bias
  vector[N] pi_delta_pun_pr;  // change pavlovian bias 
  vector[N] rhoRew_pr;    // rho reward, inv temp
  vector[N] rhoPun_pr;    // rho punishment, inv temp 
}
transformed parameters {
  vector<lower=0,upper=1>[N] xi;
  vector<lower=0,upper=1>[N] ep;
  vector[N] b; 
  vector[N] pi_rew;
  vector[N] pi_0_pun;
  vector[N] pi_delta_pun;
  vector<lower=0>[N] rhoRew;
  vector<lower=0>[N] rhoPun;
  
  vector[N] log_lik;
     
  for (i in 1:N) {
    xi[i] = Phi_approx( mu_p[1] + sigma[1] * xi_pr[i] );
    ep[i] = Phi_approx( mu_p[2] + sigma[2] * ep_pr[i] );
  }
  b             = mu_p[3] + sigma[3] * b_pr; // vectorization
  pi_rew        = mu_p[4] + sigma[4] * pi_rew_pr;
  pi_0_pun      = mu_p[5] + sigma[5] * pi_0_pun_pr;
  pi_delta_pun  = mu_p[6] + sigma[6] * pi_delta_pun_pr;
  rhoRew        = exp( mu_p[7] + sigma[7] * rhoRew_pr );
  rhoPun        = exp( mu_p[8] + sigma[8] * rhoPun_pr );
  
  {
    for (i in 1:N) {
      vector[4] wv_g;  // action wegith for go
      vector[4] wv_ng; // action wegith for nogo
      vector[4] qv_g;  // Q value for go
      vector[4] qv_ng; // Q value for nogo
      vector[4] sv;    // stimulus value 
      vector[4] pGo;   // prob of go (press)
      
      real go_bias;
      real pav_bias_rew;
      real pav_bias_pun;
      
      int cue_t;
      int pressed_t;
      real outcome_t;
      
      log_lik[i] = 0;
      
      for (blk in 1:B) {
        wv_g  = initV;
        wv_ng = initV;
        qv_g  = initV;
        qv_ng = initV;
        sv    = initV;

        go_bias = b[i];
        pav_bias_rew = pi_rew[i];
          
        if (blk==1) {
          pav_bias_pun = pi_0_pun[i];
        } else if (blk==2) {
          pav_bias_pun = pi_0_pun[i] + pi_delta_pun[i];
        }
        
        for (t in 1:Tsubj[i,blk])  {
          cue_t = cue[i,t,blk];
          outcome_t = outcome[i,t,blk];
          pressed_t = pressed[i,t,blk];
          
          if (cue_t < 3){ // I set cue 1, 2 --> go_reward, nogo_reward.  
            wv_g[ cue_t ]  = qv_g[ cue_t ] + go_bias + pav_bias_rew * sv[ cue_t ];
            wv_ng[ cue_t ] = qv_ng[ cue_t ];  // qv_ng is always equal to wv_ng (regardless of action)  
          } else {
            wv_g[ cue_t ]  = qv_g[ cue_t ] + go_bias + pav_bias_pun * sv[ cue_t ];
            wv_ng[ cue_t ] = qv_ng[ cue_t ];  // qv_ng is always equal to wv_ng (regardless of action)  
          }
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
          if (pressed_t) { // update go value 
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
  mu_p[1]  ~ normal(0, 1.0); // xi
  mu_p[2]  ~ normal(0, 1.0); // ep
  mu_p[3]  ~ normal(0, 10.0); // b
  mu_p[4]  ~ normal(0, 10.0); // pi_rew
  mu_p[5]  ~ normal(0, 10.0); // pi_pun_0
  mu_p[6]  ~ normal(0, 1.0); // pi_pun_delta
  mu_p[7]  ~ normal(0, 1.0); // rhoRew
  mu_p[8]  ~ normal(0, 1.0); // rhoPun
  sigma[1:2]  ~ normal(0, 0.2);
  sigma[3:5]  ~ cauchy(0, 1.0);
  sigma[6:8]  ~ normal(0, 0.2);
  
  // individual parameters w/ Matt trick
  xi_pr            ~ normal(0, 1.0);   
  ep_pr            ~ normal(0, 1.0);   
  b_pr             ~ normal(0, 1.0); 
  pi_rew_pr        ~ normal(0, 1.0); 
  pi_0_pun_pr      ~ normal(0, 1.0); 
  pi_delta_pun_pr  ~ normal(0, 1.0); 
  rhoRew_pr        ~ normal(0, 1.0);
  rhoPun_pr        ~ normal(0, 1.0);
  
  target += sum(log_lik);
}
generated quantities {
  real<lower=0, upper=1> mu_xi;
  real<lower=0, upper=1> mu_ep;
  real mu_b;
  real mu_pi_rew;
  real mu_pi_0_pun;
  real mu_pi_delta_pun;
  real<lower=0> mu_rhoRew;
  real<lower=0> mu_rhoPun;
  
  mu_xi            = Phi_approx(mu_p[1]);
  mu_ep            = Phi_approx(mu_p[2]);
  mu_b             = mu_p[3];
  mu_pi_rew        = mu_p[4];
  mu_pi_0_pun      = mu_p[5];
  mu_pi_delta_pun  = mu_p[6];
  mu_rhoRew        = exp(mu_p[7]);
  mu_rhoPun        = exp(mu_p[8]); 
}
