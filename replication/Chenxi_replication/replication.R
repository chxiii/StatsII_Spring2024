df_long_all <- 
  bind_rows(dplyr::filter(df_long) |> mutate(group = "All"),
    dplyr::filter(df_long, vaccinated == 0) |> mutate(group = "Unvaccinated"),
    dplyr::filter(df_long, vaccinated == 1) |> mutate(group = "Vaccinated"))
custom.coef.map =  list(
  "severity" = "Pandemic severity",
  "stringency" = "Policy stringency",
  "universality" = "Policy universality",
  "severity:stringency" = "Severity * Stringency",
  "severity:universality" = "Severity * Universality",
  "universality:stringency" = "Stringency * Universality",
  "severity:stringency:universality" = "Triple interaction")

fig_1_models <- list(All = "All", Unvaccinated= "Unvaccinated", Vaccinated = "Vaccinated") |>
  lapply(function(g)
    list(rating = lm_robust(rating ~ severity*universality*stringency, fixed_effects = ~ ID, 
            data = df_long_all, subset = group == g, se_type = "stata"), 
         choice = lm_robust(choice ~ severity*universality*stringency,  fixed_effects = ~ ID, 
            data = df_long_all, subset = group == g, se_type = "stata"), 
         trust = lm_robust(trust ~ severity*universality*stringency,  fixed_effects = ~ ID, 
            data = df_long_all, subset = group == g, se_type = "stata")))

summary(fig_1_models[["Unvaccinated"]][["rating"]])

fig_main <- fig_1_data %>% 
  ggplot(aes(x = estimate, y = term, color = group, shape=group)) +
  geom_point(size = 2.5,position=position_dodge(width=0.5)) + 
  geom_errorbarh(aes(y = term, xmin =conf.low, xmax = conf.high),
                 size=0.5, alpha = 0.5, height = 0.2, position=position_dodge(width=0.5)) +
  facet_grid(~ outcome , scales = "free") + theme_bw() +
  scale_y_discrete(limits=rev)+
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_text(size = 14)) +
  theme(axis.text.y =element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.text=element_text(size=14))+
  geom_vline(xintercept = 0, linetype="dotted", color = "black") + theme(legend.position="bottom")
pdf("fig_1.pdf", width = 12, height = 5)

fig_1_models$Unvaccinated$vaccination <- 
  lm_robust(vaccine_probability ~ severity*universality*stringency,  fixed_effects = ~ ID, 
            data = df_long_all, subset = group == "Unvaccinated", se_type = "stata")

contri_model_1 <- glm(choice ~ severity*universality*stringency, family="binomial", data=df_long_all)
summary(contri_model_1)
contri_model_2 <- glm(choice ~ severity+universality+stringency, family="binomial", data=df_long_all)
summary(contri_model_2)
anova(contri_model_1, contri_model_2, test = "Chisq")
