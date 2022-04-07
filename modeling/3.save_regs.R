library(rstan)

load('/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_pi_rp_1stonly_reg.RData')

ext_sham <- extract(fit_m4_pi_rp_sham)
ext_anode <- extract(fit_m4_pi_rp_anode)

SAVE_DIR = '/data2/project_BRL/project_tDCS/behav/modeling'

for (reg in c('Qgo', 'Qnogo', 'Wgo', 'Wnogo', 'SV')){
  tmp <- colMeans(ext_sham[[reg]])
  write.csv(tmp, file = str_glue('{SAVE_DIR}/regs/{reg}_pi-rp_sham.csv', row.names=TRUE))
}

for (reg in c('Qgo', 'Qnogo', 'Wgo', 'Wnogo', 'SV')){
  tmp <- colMeans(ext_anode[[reg]])
  write.csv(tmp, file = str_glue('{SAVE_DIR}/regs/{reg}_pi-rp_anode.csv', row.names=TRUE))
}
