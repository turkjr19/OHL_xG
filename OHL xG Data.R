library(dplyr)
library(readr)
library(janitor)

ohl_201718 <- read_csv("2017-18 OHL Skaters.csv") %>% mutate(season = "201718")
ohl_201617 <- read_csv("2016-17 OHL Skaters.csv") %>% mutate(season = "201617")
ohl_201516 <- read_csv("2015-16 OHL Skaters.csv") %>% mutate(season = "201516")

ohl_data <- bind_rows(ohl_201718, ohl_201617, ohl_201516) %>%
  clean_names()
  

avg_pct <- ohl_data %>%
  select(hd_g, hd_sh, md_g, md_sh, ld_g, ld_sh) %>% 
  summarize_all(funs(sum)) %>%
  summarize(hd_shot_pct = (hd_g / hd_sh), 
            md_shot_pct = (md_g / md_sh),
            ld_shot_pct = (ld_g / ld_sh))

xg_data <- ohl_data %>%
  mutate(x_goals = (avg_pct$hd_shot_pct * hd_sh) + (avg_pct$md_shot_pct * md_sh) + (avg_pct$ld_shot_pct * ld_sh),
         x_goals_60 = ((x_goals * 60) / e_toi)) %>%
  filter(x_goals_60 != "Inf") %>%
  select(name, season, pos, team, age, gp, x_goals, x_goals_60, everything()) %>%
  arrange(desc(x_goals_60))


#scatterplot
ggplot(aes(x = x_goals, y = g), data = xg_data) +
  geom_point() +
  geom_smooth(method = 'lm', col = "red") +
  labs(title = 'xGoals to Goals in the OHL')

#model
fit <- lm(g ~ x_goals, data = xg_data)
summary(fit)

fit2 <- lm(g ~ x_goals + pos, data = xg_data)
summary(fit2)

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(lm(g ~ x_goals, data = xg_data))

