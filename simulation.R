# Simulation script

# Setup
# install.packages("tidyverse")
library(tidyverse)

N           <- 1:100 # Number of participants
n_self      <- 1:2   # Number of choices made
n_recommend <- 1:2   # Number of choices made by others for which you give a recommendation


# Test: Simulate different probabilities of choosing satisficing
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



# Assumption 1: Probabilistic strategy selection -----
# Simulating probabilities of choosing satisficing and recommending maximizing
vector_p_choose_satis <- seq(0, 1, by=.01)
results_sim2 <-  tibble(expand.grid(pid = N,                          # Participant ID
                                    cid = (n_self + n_recommend) / 2, # Choice ID
                                    p_choose_satis = vector_p_choose_satis)) %>% 
  mutate(p_recommend_satis = 1 - p_choose_satis,
         choice_self       = NA,
         choice_recommend  = NA)

set.seed(1)
for (row in 1:nrow(results_sim2)) {
  
  # Simulate choice as draw from binomial distribution (1 = satisficing)
  results_sim2[row, "choice_self"] <- rbinom(1, 1, pull(results_sim2[row, "p_choose_satis"]))
  results_sim2[row, "choice_recommend"] <- rbinom(1, 1, pull(results_sim2[row, "p_recommend_satis"]))
}

# Plot frequency of strategy selection
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



# Assumption 2: Strategy selection as a cost-return trade-off -----
# Costs and Returns play a role in strategy selection

# Theoretical distribution
tibble(value = c(dbeta(seq(0,1,.001), 2, 4), dbeta(seq(0,1,.001), 4, 2),
                 dbeta(seq(0,1,.001), 1, 1), dbeta(seq(0,1,.001), 1, 1)),
       Strategy = c(rep("Satisficing", length(seq(0,1,.001))), rep("Maximizing", length(seq(0,1,.001))),
                    rep("Satisficing", length(seq(0,1,.001))), rep("Maximizing", length(seq(0,1,.001)))),
       Target = c(rep("Strategy used", length(seq(0,1,.001))*2), rep("Strategy recommended", length(seq(0,1,.001))*2)),
       x = c(seq(0,1,.001), seq(0,1,.001), seq(0,1,.001), seq(0,1,.001))) %>% 
  mutate(Strategy = factor(Strategy, levels=c("Satisficing", "Maximizing")),
         Target = factor(Target, levels=c("Strategy used", "Strategy recommended"))) %>% 
  ggplot(aes(x, value, color=Strategy)) +
  geom_path(linewidth=2) + 
  labs(x="Estimated Return", y="Density") +
  coord_cartesian(ylim=c(0,3), xlim=c(0,1)) +
  scale_color_manual(values=viridis::viridis(10)[c(10,1)]) +
  facet_wrap(~Target, ncol=2) +
  theme_bw() +
  theme(text = element_text(size=15), 
        strip.background = element_blank(),
        strip.text = element_text(face="bold", hjust=0),
        legend.position=c(.925,.88))

ggsave("output/plots/figure_beta_theoretical.pdf", device=pdf, width=10, height=5)

# Create results object and set parameters
results_sim3 <-  tibble(expand.grid(pid = N,                               # Participant ID
                                    cid = (n_self + n_recommend) / 2)) %>% # Choice ID
  
  mutate(costs_s_self      = 0.5, # Parameters
         costs_m_self      = 2,
         costs_s_recommend = 0.1,
         costs_m_recommend = 0.1,
         alpha_s_self      = 2,
         alpha_m_self      = 4,
         beta_s_self       = 4,
         beta_m_self       = 2,
         alpha_recommend   = 1,
         beta_recommend    = 1,

         p_choose_satis    = NA, # Outcomes
         p_recommend_satis = NA,
         choice_self       = NA,
         choice_recommend  = NA)

set.seed(2)
for (row in 1:nrow(results_sim3)) {

  # Strategy selection for oneself
  # Costs
  C_s_self <- pull(results_sim3[row, "costs_s_self"])
  C_m_self <- pull(results_sim3[row, "costs_m_self"])
  
  # Draw Returns from Beta
  R_s_self <- rbeta(1, pull(results_sim3[row, "alpha_s_self"]), pull(results_sim3[row, "beta_s_self"]))
  R_m_self <- rbeta(1, pull(results_sim3[row, "alpha_m_self"]), pull(results_sim3[row, "beta_m_self"]))
  
  # Calculate utilities
  U_s <- R_s_self - C_s_self
  U_m  <- R_m_self - C_m_self
  
  # Calculate probability of selecting satisficing strategy
  results_sim3[row, "p_choose_satis"] = 1 / (1 + exp(-.5 * (U_s - U_m)))
  
  # Draw selected strategy
  results_sim3[row, "choice_self"] <- rbinom(1, 1, pull(results_sim3[row, "p_choose_satis"]))
  
  # Strategy selection for recommendation to others
  # Costs
  C_s_recommend <- pull(results_sim3[row, "costs_s_recommend"])
  C_m_recommend <- pull(results_sim3[row, "costs_m_recommend"])
  
  # Draw Returns from Beta
  R_s_recommend <- rbeta(1, pull(results_sim3[row, "alpha_recommend"]), pull(results_sim3[row, "beta_recommend"]))
  R_m_recommend  <- rbeta(1, pull(results_sim3[row, "alpha_recommend"]), pull(results_sim3[row, "beta_recommend"]))
  
  # Calculate utilities
  U_s <- R_s_recommend - C_s_recommend
  U_m  <- R_m_recommend - C_m_recommend
  
  # Calculate probability of recommending satisficing strategy
  results_sim3[row, "p_recommend_satis"] <- 1 / (1 + exp(-.5 * (U_s - U_m)))
  
  # Draw recommended strategy
  results_sim3[row, "choice_recommend"]  <- rbinom(1, 1, pull(results_sim3[row, "p_recommend_satis"]))
}

# Plot frequency of strategy selection
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

ggsave("output/plots/figure_costs_returns_main.pdf", device=pdf, width=6, height=5)



# Assumption 3: Social returns govern strategy selection for recommendation -----
# Returns of recommending a strategy depend on social reputation

# Simulate theoretically implied Return structure using different numbers of positive outcomes
tibble(n_pos = c(rep(1, length(seq(0,1,.001))), rep(1, length(seq(0,1,.001))),
                 rep(2, length(seq(0,1,.001))), rep(2, length(seq(0,1,.001))),
                 rep(5, length(seq(0,1,.001))), rep(5, length(seq(0,1,.001))),
                 rep(7, length(seq(0,1,.001))), rep(7, length(seq(0,1,.001)))),
       Return = c(dbeta(seq(0,1,.001), 1, 10 - 1), dbeta(seq(0,1,.001), 10 - 1, 1),
                  dbeta(seq(0,1,.001), 2, 10 - 2), dbeta(seq(0,1,.001), 10 - 2, 2),
                  dbeta(seq(0,1,.001), 5, 10 - 5), dbeta(seq(0,1,.001), 10 - 5, 5),
                  dbeta(seq(0,1,.001), 7, 10 - 7), dbeta(seq(0,1,.001), 10 - 7, 7)),
       Strategy = c(rep("Satisficing", length(seq(0,1,.001))), rep("Maximizing", length(seq(0,1,.001))),
                    rep("Satisficing", length(seq(0,1,.001))), rep("Maximizing", length(seq(0,1,.001))),
                    rep("Satisficing", length(seq(0,1,.001))), rep("Maximizing", length(seq(0,1,.001))),
                    rep("Satisficing", length(seq(0,1,.001))), rep("Maximizing", length(seq(0,1,.001)))),
       x = c(seq(0,1,.001), seq(0,1,.001),
             seq(0,1,.001), seq(0,1,.001),
             seq(0,1,.001), seq(0,1,.001),
             seq(0,1,.001), seq(0,1,.001))) %>% 
  mutate(Strategy = factor(Strategy, levels=c("Satisficing", "Maximizing")),
         n_pos = factor(n_pos, levels=c("1", "2", "5", "7"))) %>% 
  ggplot(aes(x, Return, color=Strategy, alpha=n_pos, linetype=n_pos)) +
  geom_path(linewidth=1.5) +
  scale_color_manual(values=viridis::viridis(10)[c(10,1)]) +
  scale_alpha_manual(values=c(1,.8,.6,.4)) +
  facet_wrap(~Strategy, ncol=2) +
  labs(x = "Estimated return", y="Density") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=15),
        strip.background = element_blank(),
        strip.text.x = element_text(face="bold", hjust=0) #, legend.position=c(.925,.88))
  )

ggsave("output/plots/figure_theoretical_returns_social.pdf", device=pdf, width=10, height=5)



# Simulation
# Create results object and set parameters
results_sim4 <-  tibble(expand.grid(pid = N,                               # Participant ID
                                    cid = (n_self + n_recommend) / 2)) %>% # Choice ID
  
  mutate(costs_s_self      = 0.5, # Parameters, see above
         costs_m_self      = 2,
         costs_s_recommend = 0.1,
         costs_m_recommend = 0.1,
         
         alpha_s_self      = 2,
         alpha_m_self      = 4,
         beta_s_self       = 4,
         beta_m_self       = 2,
         
         theta             = 0.9,
         n_pos             = NA,  # Will be estimated later
         alpha_s_recommend = NA,
         alpha_m_recommend = NA,
         beta_s_recommend  = NA,
         beta_m_recommend  = NA,
         
         p_choose_satis    = NA, # Outcomes
         p_recommend_satis = NA,
         choice_self       = NA,
         choice_recommend  = NA)

set.seed(3)
for (row in 1:nrow(results_sim4)) {

  # Strategy selection for oneself (same as for simulation 3)
  # Costs
  C_s_self <- pull(results_sim4[row, "costs_s_self"])
  C_m_self <- pull(results_sim4[row, "costs_m_self"])
  
  # Draw Returns from Beta
  R_s_self <- rbeta(1, pull(results_sim4[row, "alpha_s_self"]), pull(results_sim4[row, "beta_s_self"]))
  R_m_self <- rbeta(1, pull(results_sim4[row, "alpha_m_self"]), pull(results_sim4[row, "beta_m_self"]))
  
  # Calculate utilities
  U_s <- R_s_self - C_s_self
  U_m  <- R_m_self - C_m_self
  
  # Calculate probability of selecting satisficing strategy
  results_sim4[row, "p_choose_satis"] <- 1 / (1 + exp(-.5 * (U_s - U_m)))
  
  # Draw selected strategy
  results_sim4[row, "choice_self"] <- rbinom(1, 1, pull(results_sim4[row, "p_choose_satis"]))
  
  
  # Strategy selection for recommendation to others
  # Costs
  C_s_recommend <- pull(results_sim4[row, "costs_s_recommend"])
  C_m_recommend <- pull(results_sim4[row, "costs_m_recommend"])
  
  # Simulate number of retrieved positive outcomes
  results_sim4[row, "n_pos"] <- sum(rbinom(10, 1, 1 - pull(results_sim4[row, "theta"])))
  
  # Draw Returns from Beta using number of retrieved positive outcomes as determinants of Return structure
  R_s_recommend <- rbeta(1, pull(results_sim4[row, "n_pos"]), 10 - pull(results_sim4[row, "n_pos"]))
  R_m_recommend  <- rbeta(1, 10 - pull(results_sim4[row, "n_pos"]), pull(results_sim4[row, "n_pos"]))
  
  # Calculate utilities
  U_s <- R_s_recommend - C_s_recommend
  U_m  <- R_m_recommend - C_m_recommend
  
  # Calculate probability of recommending satisficing strategy
  results_sim4[row, "p_recommend_satis"] <- 1 / (1 + exp(-.5 * (U_s - U_m)))
  
  # Draw recommended strategy
  results_sim4[row, "choice_recommend"]  <- rbinom(1, 1, pull(results_sim4[row, "p_recommend_satis"]))
}

# Plot frequency of strategy selection
results_sim4 %>% 
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

ggsave("output/plots/figure_social_sensitivity_main.pdf", device=pdf, width=6, height=5)
