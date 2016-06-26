# 
#  explore.rate <- 45:6
#  
#  explore.land <- c(1920,1983,2048,2117,2189,2265,2345,2429,2518,2612,2711,2816,2928,3047,
#            3173,3308,3453,3608,3774,3954,4149,4360,4589,4841,5116,5420,5757,6132,
#            6553,7028,7569,8189,8909,9755,10761,11979,13484,15389,17882,21280)
#  
#  explore.gov <- rep('O', length(explore.rate))
#  
#  republic.land <- c(1915,1967,2021,2077,2135,2196,2259,2325,2394,2466,2541,2620,
#                     2703,2789,2881,2977,3077,3184,3297,3416,3542,3676,3818,3970,
#                     4132,4305,4491,4691,4907,5141,5394,5669,5971,6302,6666,7071,
#                     7521,8026,8596,9245,9989,10853,11868,13075,14537,16342,18629,21620)
#  republic.rate <- 54:7
#  
#  republic.gov <- rep('R', length(republic.rate))
#  
#  explore.rates <- data.frame(cbind(append(explore.rate, republic.rate), append(explore.land, republic.land), append(explore.gov, republic.gov)))
#  colnames(explore.rates) <- c("rate", "land", "gov")
#  write.table(explore.rates, "explore_rates.csv", sep=",", row.names = FALSE)

explore.rates <- tbl_df(read.table("explore_rates.csv", sep=",", header = TRUE))

