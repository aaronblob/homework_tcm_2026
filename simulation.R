# Simulation script

# - You have resources
# - You try to minimize costs
# - Process of choosing has costs
#   - For yourself, costs = effort + time + social
#   - For recommendations, costs = effort + time + social
# - You receive resources from choice

# Setup
# install.packages("tidyverse")
library(tidyverse)

N           <- 1:100 # Number of participants
n_self      <- 1:2   # Number of choices made
n_recommend <- 1:2   # Number of choices made by others for which you give a recommendation


# Simulation 1 -----
# Different probabilities of choosing satisficing
vector_p_choose_satis <- seq(0, 1, by=.01)
results_sim1 <-  tibble(expand.grid(pid = N,                          # Participant ID
                                    cid = (n_self + n_recommend) / 2, # Choice ID
                                    p_choose_satis = vector_p_choose_satis)) %>% 
  mutate(choice_self      = NA,
         choice_recommend = NA)

for (row in 1:nrow(results_sim1)) {
  # Simulate choice as draw from binomial distribution (1 = satisficing)
  results_sim1[row, "choice_self"] <- rbinom(1, 1, pull(results_sim1[row, "p_choose_satis"]))
}

# Results
results_sim1 %>% 
  ggplot(aes(p_choose_satis, choice_self)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(subtitle="The frequency of choosing satisficing varies with its probability", 
       x="Probability of choosing satisficing", y="Proportion of choices following satisficing") +
  coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
  theme_bw()



# Simulation 2 -----
# Simulating probabilities of choosing satisficing and recommending maximizing
vector_p_choose_satis <- seq(0, 1, by=.01)
results_sim2 <-  tibble(expand.grid(pid = N,                          # Participant ID
                                    cid = (n_self + n_recommend) / 2, # Choice ID
                                    p_choose_satis = vector_p_choose_satis)) %>% 
  mutate(p_recommend_satis = 1 - p_choose_satis,
         choice_self      = NA,
         choice_recommend = NA)

for (row in 1:nrow(results_sim2)) {
  # Simulate choice as draw from binomial distribution (1 = satisficing)
  results_sim2[row, "choice_self"] <- rbinom(1, 1, pull(results_sim2[row, "p_choose_satis"]))
  results_sim2[row, "choice_recommend"] <- rbinom(1, 1, pull(results_sim2[row, "p_recommend_satis"]))
}

# Results
results_sim2 %>% 
  pivot_longer(cols=c("choice_self", "choice_recommend"), names_to="Type of choice", values_to="choice") %>% 
  mutate(`Type of choice` = ifelse(`Type of choice`=="choice_self", "Choice made yourself", "Choice recommended")) %>% 
  ggplot(aes(p_choose_satis, choice, color=`Type of choice`)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(subtitle="The frequency of choosing satisficing varies with its probability", 
       x="Probability of choosing/recommending satisficing", y="Proportion of (recommended) choices following satisficing") +
  coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
  scale_color_manual(values=c("blue3", "red3")) +
  theme_bw()

results_sim2 %>% 
  filter(p_choose_satis %in% c(.5, .65, .8)) %>% 
  mutate(p_choose_satis = as.factor(p_choose_satis)) %>% 
  group_by(p_choose_satis) %>% 
  summarise(p_choice_satis = sum(choice_self) / n(),
            p_recommend_satis = sum(choice_recommend) / n()) %>% 
  pivot_longer(cols=c("p_choice_satis", "p_recommend_satis")) %>% 
  mutate(name = ifelse(name == "p_choice_satis", "Satisficing", "Maximizing")) %>% 
  mutate(name = factor(name, levels=c("Satisficing", "Maximizing"))) %>% 
  ggplot(aes(name, value, fill=name)) +
  geom_col(position=position_dodge()) +
  geom_text(tibble(p_choose_satis=c(.5,.65,.8)), mapping=aes(x=.7, y=1, label=paste0("italic(p)[italic(s)]==", p_choose_satis)), parse=T, inherit.aes=F, size=5) +
  labs(x="Strategy selected", y="Proportion") +
  scale_fill_manual(values=viridis::viridis(10)[c(10,1)]) +
  theme_bw() +
  facet_wrap(~p_choose_satis, ncol=3) +
  theme(text = element_text(size=15), 
        legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggsave("output/plots/figure_basic.pdf", device=pdf, width=12, height=6)



# Simulation 3 -----
# Costs and rewards play a role in strategy selection
costs_s_self <- 0.5
costs_m_self <- 2
costs_s_recommend <- costs_m_recommend <- 0.1

alpha_s_self <- 2
beta_s_self  <- 4
alpha_m_self <- 4
beta_m_self  <- 2

alpha_recommend <- 1
beta_recommend  <- 1

# Theoretical distribution
tibble(value = c(dbeta(seq(0,1,.001), alpha_s, beta_s), dbeta(seq(0,1,.001), alpha_m, beta_m)),
       Strategy = c(rep("Satisficing", length(seq(0,1,.001))), rep("Maximizing", length(seq(0,1,.001)))),
       x = c(seq(0,1,.001), seq(0,1,.001))) %>% 
  ggplot(aes(x, value, color=Strategy)) +
  geom_path(linewidth=2) + 
  labs(x="Estimated reward", y="Density") +
  coord_cartesian(ylim=c(0,3), xlim=c(0,1)) +
  scale_color_manual(values=viridis::viridis(10)[c(1,10)]) +
  theme_bw() +
  theme(text = element_text(size=15), 
        legend.position=c(.15,.88))

ggsave("output/plots/figure_beta_theoretical.pdf", device=pdf, width=6, height=5)

# Create results object
results_sim3 <-  tibble(expand.grid(pid = N,                               # Participant ID
                                    cid = (n_self + n_recommend) / 2)) %>% # Choice ID
  
  mutate(costs_s_self      = costs_s_self, # Parameters, see above
         costs_m_self      = costs_m_self,
         costs_s_recommend = costs_s_recommend,
         costs_m_recommend = costs_m_recommend,
         alpha_s_self      = alpha_s_self,
         alpha_m_self      = alpha_m_self,
         beta_s_self       = beta_s_self,
         beta_m_self       = beta_m_self,
         alpha_recommend   = alpha_recommend,
         beta_recommend    = beta_recommend,

         p_choose_satis    = NA, # Outcomes
         p_recommend_satis = NA,
         choice_self       = NA,
         choice_recommend  = NA)

for (row in 1:nrow(results_sim3)) {
  # Simulate choice
  
  # Strategy selection for oneself
  C_s_self <- pull(results_sim3[row, "costs_s_self"])
  C_m_self <- pull(results_sim3[row, "costs_m_self"])
  R_s_self <- rbeta(1, pull(results_sim3[row, "alpha_s_self"]), pull(results_sim3[row, "beta_s_self"]))
  R_m_self <- rbeta(1, pull(results_sim3[row, "alpha_m_self"]), pull(results_sim3[row, "beta_m_self"]))
  
  U_s <- R_s_self - C_s_self
  U_m  <- R_m_self - C_m_self
  
  p_choose_satisficing = 1 / (1 + exp(-.5 * (U_s - U_m)))
  
  results_sim3[row, "choice_self"] <- rbinom(1, 1, p_choose_satisficing)
  results_sim3[row, "p_choose_satis"] <- p_choose_satisficing
  
  # Strategy selection for recommendation
  C_s_recommend <- pull(results_sim3[row, "costs_s_recommend"])
  C_m_recommend <- pull(results_sim3[row, "costs_m_recommend"])
  R_s_recommend <- rbeta(1, pull(results_sim3[row, "alpha_recommend"]), pull(results_sim3[row, "beta_recommend"]))
  R_m_recommend  <- rbeta(1, pull(results_sim3[row, "alpha_recommend"]), pull(results_sim3[row, "beta_recommend"]))
  
  U_s <- R_s_recommend - C_s_recommend
  U_m  <- R_m_recommend - C_m_recommend
  
  p_recommend_satisficing = 1 / (1 + exp(-.5 * (U_s - U_m)))
  
  results_sim3[row, "choice_recommend"]  <- rbinom(1, 1, p_recommend_satisficing)
  results_sim3[row, "p_recommend_satis"] <- p_recommend_satisficing
  
}

# Strategy selections
results_sim3 %>% 
  pivot_longer(cols=c("choice_self", "choice_recommend"), names_to = "target", values_to = "strategy") %>% 
  group_by(strategy, target) %>% 
  summarise(n = n() / 200) %>% 
  mutate(strategy = ifelse(strategy==1, "Satisficing", "Maximizing"),
         target = ifelse(target=="choice_self", "Strategy used", "Strategy recommended")) %>% 
  mutate(strategy = factor(strategy, levels=c("Satisficing", "Maximizing")),
         target = factor(target, levels=c("Strategy used", "Strategy recommended"))) %>% 
  ggplot(aes(target, n, fill=strategy)) +
  geom_col(position=position_dodge()) +
  labs(fill="Strategy", x="Type of situation", y="Proportion") +
  coord_cartesian(ylim=c(0,1)) +
  scale_fill_manual(values=viridis::viridis(10)[c(10,1)]) +
  theme_bw() +
  theme(text = element_text(size=15), 
        legend.position=c(.15,.88),
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggsave("output/plots/figure_costs_rewards_main.pdf", device=pdf, width=6, height=5)



# Simulating differing costs and rewards
