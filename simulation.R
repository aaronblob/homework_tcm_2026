

# - You have resources
# - You try to minimize costs
# - Process of choosing has costs
#   - For yourself, costs = effort + time + social
#   - For recommendations, costs = effort + time + social
# - You receive resources from choice



library(tidyverse)
N <- 1000
results <- tibble(pid = rep(seq(1,N), each=2),
                  R = rep(rnorm(N, 50, 15), each=2),
                  target = rep(c("Strategy \nused", "Strategy \nrecommended"), N),
                  choice = NA)

strategies <- c("satisficing", "maximizing")
P_satis <- c(.8)

for (p_satis in P_satis) {
  for (row in 1:nrow(results)) {
    if (results[row, "target"]=="Strategy \nused") {
      results[row, "choice"] <- sample(size=1, x=strategies, prob=c(p_satis, 1 - p_satis))
    } else if (results[row, "target"]=="Strategy \nrecommended") {
      results[row, "choice"] <- sample(size=1, x=strategies, prob=c(p_satis - .4, 1 - p_satis + .4))
    }
  }
}

results %>% 
  group_by(target, choice) %>% 
  summarise(p = n() / nrow(results)*2)

results %>% 
  group_by(target, choice) %>% 
  summarise(p = n() / nrow(results)*2) %>% 
  ggplot(aes(target, p, fill=choice)) +
  geom_col(position=position_dodge()) +
  labs(title="Pr(Satisficing, self) = 0.80 \nPr(Satisficing, other) = 0.40", y="Probability", x=" ", fill="Strategy") +
  coord_cartesian(ylim=c(0,1)) +
  scale_fill_manual(values=viridis::viridis(10)[c(1,10)]) +
  theme_bw()




for (n_sim in 1:n_sims) {
  for_whom <- sample(size=1, x=self_other, prob=c(.5, .5))
  
  if (for_whom=="self") {
    p <- .8
    q <- 1 - p
    results <- bind_rows(results,
                         bind_cols(sim_id = n_sim,
                                   choice=sample(size=1, x=choice, prob=c(p, q)),
                                   target=for_whom))
    
  } else if (for_whom=="other") {
    
    similarity <- runif(1, -3, 1)
    p <- plogis(similarity)
    q <- 1 - p
    results <- bind_rows(results,
                         bind_cols(sim_id = n_sim,
                                   choice=sample(size=1, x=choice, prob=c(p, q)),
                                   target=for_whom))
  }
}

results

results %>% 
  mutate(target = factor(target, levels=c("self", "other")),
         choice = factor(choice, levels=c("maximize", "satisfy"))) %>% 
  ggplot(aes(target, fill=choice)) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values=viridis::viridis(10)[c(1,10)]) +
  theme_bw()








sample(x=c("A", "B"), size=1, prob=c(.5,.5))




