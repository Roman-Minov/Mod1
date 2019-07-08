library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2") 

eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
eddypro = eddypro[-1, ]
eddypro = select(eddypro, -(roll))
eddypro = eddypro %>% mutate_if(is.character, factor)

names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "L_")

glimpse(eddypro)

eddypro = drop_na(eddypro)
eddypro = filter(eddypro, DOY >= 59 & DOY < 151)
eddypro = filter(eddypro, daytime==TRUE)
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]
eddypro_numeric

row_numbers = 1:length(eddypro_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
test = row_numbers[-teach]
teaching_tbl = eddypro_numeric[teach,]
testing_tbl = eddypro_numeric[test,]

mod1 = lm(h2o_flux~ (.) , data = teaching_tbl)

summary(mod1)
anova(mod1)
plot(mod1)

mod2 = lm ( h2o_flux ~ DOY + Tau + qc_Tau + H + LE + rand_err_LE
            + rand_err_h2o_flux + h2o_time_lag + sonic_temperature
            + water_vapor_density + e + u_rot + w_rot + max_speed
            + u_star_ + L + x_30_perc_ + un_Tau + Tau_scf + un_H
            + H_scf + un_LE + un_h2o_flux + w_spikes + ts_spikes
            + co2_spikes + h2o_signal_strength_7200
            + flowrate, data = teaching_tbl)

summary(mod2)
anova(mod2)
anova(mod2, mod1)
plot(mod2) 

mod3 = lm ( h2o_flux ~ DOY + Tau + qc_Tau + H + LE + rand_err_LE
            + rand_err_h2o_flux + sonic_temperature
            + water_vapor_density + u_rot + w_rot + max_speed
            + u_star_ + L + x_30_perc_ + un_Tau + Tau_scf + un_H
            + H_scf + un_LE + un_h2o_flux, data = teaching_tbl)

summary(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3)

cor_teaching_tbl = select(teaching_tbl, DOY, Tau, qc_Tau, H, LE, rand_err_LE,
                          rand_err_h2o_flux, sonic_temperature, water_vapor_density,
                          u_rot, w_rot, max_speed, u_star_, L, x_30_perc_,
                          un_Tau, Tau_scf, un_H, H_scf, un_LE, un_h2o_flux)

cor_td = cor(cor_teaching_tbl) %>% as.data.frame

qplot(h2o_flux , h2o_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
qplot(h2o_flux , h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

qplot(DOY, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(max_speed, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

