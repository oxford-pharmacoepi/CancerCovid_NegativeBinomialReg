# Testing the model assumptions of the data to determine the best model to run

# Dispersion in the data

# overall - events. It seems that the variance is substantially higher than the mean (var = 2321.386; mean = 175.158)
mean(IR.overall$events)
var(IR.overall$events)

mean(IR.overall$ir_m)
var(IR.overall$ir_m)

# Aggregated summary of mean and variance of events for females grouped by age group and outcome
# some of the categories do not show overdispersion, and this may be why the glm.nb model is failing
agg_summary_female <- IR.age_female %>% group_by(outcome, denominator_age_group) %>% 
  summarise(mean_events=mean(events), var_events=var(events))

count_female <- IR.age_female %>% group_by(outcome, denominator_age_group) %>% tally() %>% collect()


            
# Aggregated summary of mean and variance of events for males grouped by age group and outcome
agg_summary_male <- IR.age_male %>% group_by(outcome, denominator_age_group) %>% 
  summarise(mean_events=mean(events), var_events=var(events))




