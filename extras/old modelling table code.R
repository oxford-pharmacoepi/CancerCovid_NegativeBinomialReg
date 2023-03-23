
###TABLE--------- 
tab1 <- tab1 %>% mutate(strata="overall")
tab_sex <-tab_sex %>% rename(strata=denominator_sex)
tab_age_sex <-tab_age_sex %>% mutate(strata=paste0(denominator_age_group, ",", denominator_sex))%>%
  dplyr::select(-denominator_age_group, -denominator_sex)
# tab_ses <-tab_ses %>%rename(strata=medea)

table <- rbind(tab1, tab_sex ,tab_age_sex) %>% arrange(outcome, desc(strata), desc(value)) 

table <- table %>% relocate("value", .after= "outcome") %>% relocate("strata", .after= "outcome")

#table_undx <- table %>%filter(value=="Underdx")%>%
#  rename(Lockdown_undx="Lockdown")%>%
#  rename(Post_lockdown1_undx="Post-lockdown1")%>%
#  rename(Second_lockdown_undx="Second lockdown")%>%
#  rename(Third_lockdown_undx="Third lockdown")%>%
#  rename(Easing_of_restrictions_undx="Easing of restrictions")%>%
#  rename(Legal_restrictions_removed_undx="Legal restrictions removed")%>%
#  rename(Post_lockdown_undx="Post")%>%
#  rename(Total_undx="Total")%>%
#  dplyr::select(-value)

#table_red <- table %>%filter(value!="Underdx")%>%
#  rename(Lockdown_reduction="Lockdown")%>%
#  rename(Post_lockdown1_reduction="Post-lockdown1")%>%
#  rename(Second_lockdown_reduction="Second lockdown")%>%
# rename(Third_lockdown_reduction="Third lockdown")%>%
#  rename(Easing_of_restrictions_reduction="Easing of restrictions")%>%
#  rename(Legal_restrictions_removed_reduction="Legal restrictions removed")%>%
#  rename(Post_lockdown_reduction="Post")%>%
#  rename(Total_reduction="Total")%>%
#  dplyr::select(-value)

#table <- merge(table_undx, table_red)
#table1 <- table[, c("outcome", "strata", "Lockdown_undx","Lockdown_reduction",
#                   "Post_lockdown1_undx",  "Post_lockdown1_reduction", 
#                    "Second_lockdown_undx", "Second_lockdown_reduction",
#                    "Third_lockdown_undx","Third_lockdown_reduction",
#                    "Easing_of_restrictions_undx",  "Easing_of_restrictions_reduction",
#                    "Legal_restrictions_removed_undx", "Legal_restrictions_removed_reduction",
#                    "Post_lockdown_undx", "Post_lockdown_reduction",
#                    "Total_undx", "Total_reduction")]

#table1$strata <- factor(table1$strata, levels=c(
# "overall", "Female","Male", "0;150,Male","0;150,Female","20;39,Female","20;39,Male","40;59,Female","40;59,Male",
# "60;79,Female","60;79,Male","80;150,Female","80;150,Male"))

#table1 <- table1 %>%arrange(outcome, strata)