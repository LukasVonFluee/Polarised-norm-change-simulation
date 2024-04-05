################################################################################
# This is the code for the simulation model presented in the paper
# "From endless possibilities to shared sustainable social norms" by
# Lukas von Flüe and Sonja Vogt.
# It is an adapted version of the code used for the following paper:
# "When norm change hurts" (2024) by
# Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.
################################################################################
rm(list=ls())
################################################################################
################################################################################

# Gini function
# References: Gini (1912), Schmidt & Wichardt (2019):

# The argument of the function, x, will consist of an array including the 
# agents' payoffs.

my_gini <- function(x) {
  n <- length(x)  
  
  sum_x <- sum(x) 
  
  sorted_x <- sort(x)  
  
  i <- 1:n
  numerator <- sum(2 * i * sorted_x)
  denominator <- n * sum_x
  
  gini <- numerator / denominator - (n + 1) / n
  
  return(gini)
}

################################################################################

# Test parameter values

# t_max <- 10
# 
# N <- 1000
# G <- 2
# 
# n <- 10
# 
# b <- 0
# d <- 1    
# g <- 0 # payoff for choosing SQ for targeted agents who respond
# 
# target<-0
# phi <- 0.75
# 
# a <- 0.75
# h <- 2
# 
# alpha <- 2.75
# beta <- 2
# 
# init_alt_fract <- 0
# 
# s <- 1
# 
# s_new <- 0.5

################################################################################
################################################################################
################################################################################

# The parameter values and combinations can be changed further below.
# Search for "param_combinations".

################################################################################
################################################################################
################################################################################

# Coordination game payoff matrix:

#       SQ      Alt
# SQ    a+x_i   b+x_i
# Alt   a       d
# where: a+x_i > a, b+x_i<d, for each agent.

################################################################################
################################################################################

# Simulation function:

################################################################################
################################################################################

# Notes:

# An intervention is implemented in the first period because we are only 
# interested in dynamics after the intervention. The first period represents
# the steady state before the intervention, namely an equilibrium with SQ as
# the prevalent social norm. SQ is assumed to be the equilibrium norm because
# it risk dominates Alt.
# Agents respond with a probability = 1-(x_i/(d-b)) to the intervention. 
# Note, q=x_i/(d-b) is simply the fraction of agents choosing Alt that makes an 
# agent indifferent between choosing Alt and SQ. Further, in the paper it is 
# assumed that an agent switches to Alt if the expected payoff of Alt is >= the
# expected payoff of SQ.

sim <- function (N=1000, n=10, t_max=100, G=2, s=1, s_new=0.5, alpha=2.75, beta=2, target=1, phi=0.75, a=0.75, h=2, b=0, d=1, g=0) {
  
  # Initialize data frame in which we store coordination results
  summary_results <- data.frame(freq_coord_sq=rep(0,t_max), 
                                freq_coord_alt=rep(0,t_max),
                                miscoordination=rep(0,t_max),
                                freq_sq=rep(0,t_max),
                                freq_alt=rep(0,t_max),
                                freq_sq_group1=rep(0,t_max),
                                freq_alt_group1=rep(0,t_max),
                                freq_sq_group2=rep(0,t_max),
                                freq_alt_group2=rep(0,t_max),
                                avg_payoff_sq=rep(0,t_max),
                                avg_payoff_alt=rep(0,t_max),
                                avg_payoff=rep(0,t_max),
                                gini_coefficient=rep(0,t_max),
                                exp_sq=rep(0,t_max),
                                exp_alt=rep(0,t_max))
  
  # Before intervention, we assume everyone chooses SQ:
  summary_results$freq_coord_sq[1] <- N/2
  summary_results$freq_coord_alt[1] <- 0
  summary_results$miscoordination[1] <- 0
  summary_results$freq_sq[1] <- N
  summary_results$freq_alt[1] <- 0
  
  # Record both groups individually
  
  summary_results$freq_sq_group1[1] <- N/G
  summary_results$freq_alt_group1[1] <- 0
  
  summary_results$freq_sq_group2[1] <- N/G
  summary_results$freq_alt_group2[1] <- 0
  
  # Initialize agents (note, we assume agents start from an equilibrium where 
  # everyone chooses SQ (choice=0)).
  # "x_i" = idiosyncratic x value
  # "respond" = register whether an agent responded to intervention or not
  # "exp_sq" = expected payoff for choosing SQ
  # "exp_alt" = expected payoff for choosing Alt
  # "choice" = choice between SQ and Alt, where choice=0 means choosing SQ
  # "payoff" = payoff after playing coordination game in a given period
  
  agent <- data.frame(x_i = rep(0, N), group = rep(0, N), respond = rep(0, N), 
                      exp_sq = rep(0, N), exp_alt = rep(0), 
                      choice = rep(0, N), payoff = rep(0, N), q=rep(0, N))
  
  x_values <- rbeta(N, shape1 = alpha, shape2 = beta) * (d - b)
  agent$x_i <- x_values
  
  # Density plot of x_i values:
  # library(ggplot2)
  # ggplot(data = data.frame(x = agent$x_i), aes(x)) +
  #   geom_density() +
  #   labs(title = "Density Plot of x_i", x = "x_i", y = "Density") +
  #   theme_minimal()  # Applying minimalistic theme
  
  # Density plot with customized axis titles and increased font size
  # ggplot(data = data.frame(x = agent$x_i), aes(x)) +
  #   geom_density() +
  #   labs(title = "", x = expression(x[i] ~ "values"), y = "Density") +
  #   theme_minimal() +
  #   theme(axis.title.x = element_text(size = 14, face = "bold"),  # Customize x-axis title
  #         axis.title.y = element_text(size = 14, face = "bold"))  # Customize y-axis title
  
  # # Initialize array to record agents' traits over all periods:
  # num_traits <- length(agent[1,])   # number of traits
  # agent_output <- array(0, dim = c(N, num_traits, t_max))
  # 
  # # rename columns
  # dimnames(agent_output) <- list(
  #   agent = as.character(1:N),
  #   trait = c("x_i", "respond", "exp_sq", "exp_alt", "choice", "payoff", "q"),
  #   period = 1:t_max
  # )
  
  # Check the dimnames
  # dimnames(agent_output)
  
  # Before t=1, we have an SQ equilibrium, and thus; agent$q=0
  # In t=1, agents play the coordination game given the belief q=0.
  
  t <- 1
  
  # Beliefs (could also just define q<-0 because we start from an equilibrium where
  # everyone chooses SQ: q is the belief of agent i that the next agent i will
  # get paired off with chooses Alt, and this is based on the distribution of 
  # Alt choices of the previous round. 
  # However, we want to always use the same formula to calculate belief q:
  agent$q <- ifelse(agent$choice==0, sum(agent$choice)/(N-1), ((sum(agent$choice)-1)/(N-1)))
  
  # Expected payoffs based on q=0:
  agent$exp_sq <- ((1-agent$q)*(a + agent$x_i)) + (agent$q*(b+agent$x_i))
  agent$exp_alt <- (1-agent$q)*a + agent$q*d
  
  # Record average expected payoffs of this period:
  summary_results$exp_sq[t] <- sum(agent$exp_sq)/N
  summary_results$exp_alt[t] <- sum(agent$exp_alt)/N
  
  # Determine choice based on expected payoffs. Note: An agent chooses Alt if
  # exp_alt >= exp_sq:
  agent$choice <- ifelse(agent$exp_alt>=agent$exp_sq, 1, 0)
  
  # Create array of randomized indices to prepare for random matching
  shuffled_indices <- sample(N)
  
  # Split shuffled indices into two halves which represent the two parties 
  # getting paired off in dyadic groups
  player_1 <- shuffled_indices[1:(N/2)]
  player_2 <- shuffled_indices[((N/2)+1):N]
  
  # Retrieve choice values for each half
  choice_player_1 <- agent$choice[player_1]
  choice_player_2 <- agent$choice[player_2]
  
  # Compare choice values and record frequency of coordination
  both_sq <- choice_player_1 == 0 & choice_player_2 == 0
  both_alt <- choice_player_1 == 1 & choice_player_2 == 1
  freq_coord_sq <- sum(both_sq)
  freq_coord_alt <- sum(both_alt)
  
  # Store results in 'summary_results' data frame:
  summary_results$freq_coord_sq[t] <- freq_coord_sq
  summary_results$freq_coord_alt[t] <- freq_coord_alt
  summary_results$miscoordination[t] <- (N/2) - freq_coord_sq - freq_coord_alt
  
  # Calculate realized payoffs:
  payoff_player_1 <- numeric(N/2)
  payoff_player_2 <- numeric(N/2)
  
  # If both coordinate on SQ, payoff is: a + x_i 
  payoff_player_1[both_sq] <- a + agent$x_i[player_1][both_sq]
  payoff_player_2[both_sq] <- a + agent$x_i[player_2][both_sq]
  
  # If both coordinate on Alt, payoff is 'd' for non-targeted agents, as well as 
  # for agents who were targeted but didn't respond:
  payoff_player_1[both_alt] <- d
  payoff_player_2[both_alt] <- d
  
  # Payoffs when player 1 chooses SQ and player 2 chooses Alt:
  payoff_player_1[choice_player_1 == 0 & choice_player_2 == 1] <- b + agent$x_i[player_1][choice_player_1 == 0 & choice_player_2 == 1] 
  payoff_player_2[choice_player_1 == 0 & choice_player_2 == 1] <- a
  
  # Payoffs when player 1 chooses Alt and player 2 chooses SQ:
  payoff_player_1[choice_player_1 == 1 & choice_player_2 == 0] <- a
  payoff_player_2[choice_player_1 == 1 & choice_player_2 == 0] <- b + agent$x_i[player_2][choice_player_1 == 1 & choice_player_2 == 0] 
  
  # Register payoffs in agent data frame
  agent[player_1,"payoff"] <- payoff_player_1
  agent[player_2,"payoff"] <- payoff_player_2
  
  # Record summary results
  
  # Record frequencies of choices:
  summary_results$freq_sq[t] <- length(agent[which(agent$choice==0),"choice"])
  summary_results$freq_alt[t] <- length(agent[which(agent$choice==1),"choice"])
  
  # Record average payoffs:
  num_agents_sq <- length(agent[which(agent$choice==0),"choice"])
  summary_results$avg_payoff_sq[t] <- ifelse(num_agents_sq>0,sum(agent[which(agent$choice==0),"payoff"]) / num_agents_sq,NA) 
  
  num_agents_alt <- length(agent[which(agent$choice==1),"choice"])
  summary_results$avg_payoff_alt[t] <- ifelse(num_agents_alt>0,sum(agent[which(agent$choice==1),"payoff"]) / num_agents_alt,NA) 
  
  summary_results$avg_payoff[t] <- sum(agent[,"payoff"])/N
  
  # Calculate and store Gini coefficient:
  summary_results$gini_coefficient[t] <- my_gini(agent[,"payoff"])
  
  #############################################################################  
  #############################################################################  
  
  # INTERVENTION
  
  #############################################################################  
  #############################################################################  
  
  # Intervention (note, if target=0, we target amenable, else resistant)
  if (target==0) {
    # print("target=0")
    # We order agents in increasing order according to their x_i values, and 
    # target N*phi agents with lowest x_i values:
    agent <- agent[order(agent$x_i), ]
    # Response to intervention is probabilistic and proportional to threshold.
    # Note, an agent's threshold value is given by; agent$x_i/(d-b)
    # We make response a decreasing function of threshold values and define
    # the probability to switch as; 1-(agent$x_i/(d-b))
    prob_draw <- runif(N*phi)
    agent$respond[1:(N*phi)] <- ifelse(prob_draw<=(1-(agent$x_i[1:(N*phi)]/(d-b))), 1, 0)  
  } else if(target==1) {
    # print("target=1")
    # If we target most resistant, we order in decreasing order:
    agent <- agent[order(agent$x_i, decreasing = TRUE), ]
    prob_draw <- runif(N*phi)
    agent$respond[1:(N*phi)] <- ifelse(prob_draw<=(1-(agent$x_i[1:(N*phi)]/(d-b))), 1, 0)  
  }
  
  #############################################################################  
  # Now that intervention happened, we know that certain agents responded. 
  # For those agents who repond to intervention, choosing SQ
  # now will yield a payoff=g in every future period, and choosing Alt will 
  # yield a payoff=h, where h>g. 
  # The payoff structures for the other agents do not change.
  # Agents will now play the coordination game for (t_max - 1) periods.
  
  #############################################################################  
  
  # In addition to code for "When norm change hurts" paper, I introduce here
  # a network structure, where agents are ordered according to their x_i values
  # and similar agents are put into same groups.
  
  # Create network
  
  # If we have more than 1 group, we structure agents into groups according to their x_i values
  # Specifically, we order agents according to their x_i values and put similar agents into same groups
  # We do this ordered structuring rather than randomly assigning agents to groups because we want to simulate homophily
  # At a later point, I could also generalize the function such that a parameter specifies whether we do random assignment to groups or homophily based structuring
  if (G>1) {
    agent <- agent[order(agent$x_i, decreasing = TRUE), ]
    agent$group <- rep(1:G, each = N/G)
  } else {agent$group <- 1}
  
  #############################################################################  
  
  for (t in 2:t_max) {
    
    # Determine belief q based on choice distribution of previous period.
    # Specifically, before period t=(t_max/2), agents form beliefs based on a sample of n observations from their own group (because we set s=1).
    # From t=(t_max/2), agents are structured in G groups, and they form their beliefs based on samples of size n, 
    # where they sample within groups with probability s_new and with probability 1-s_new they sample from outgroup.
    
    # Matrix to store choices for all agents
    sampled_choices <- matrix(NA, nrow = N, ncol = n)
    
    for (i in 1:N) {
      if (t < (t_max / 2)) {
        # Use original sampling probability for all periods before t_max/2
        sample_from_own_group <- runif(n) < s
      } else {
        # Use new sampling probability after t_max/2
        sample_from_own_group <- runif(n) < s_new
      }
      
      own_group_agents <- which(agent$group == agent$group[i])
      other_group_agents <- which(agent$group != agent$group[i])
      
      # Sample from own group
      if (any(sample_from_own_group)) {
        own_samples <- sample(own_group_agents, sum(sample_from_own_group), replace = TRUE)
        sampled_choices[i, sample_from_own_group] <- agent$choice[own_samples]
      }
      
      # Sample from other groups
      if (any(!sample_from_own_group)) {
        other_samples <- sample(other_group_agents, sum(!sample_from_own_group), replace = TRUE)
        sampled_choices[i, !sample_from_own_group] <- agent$choice[other_samples]
      }
    }
    
    # Calculate q for each agent
    agent$q <- rowSums(sampled_choices == 1, na.rm = TRUE) / n
    
    # Calculate expected payoffs:
    
    # Note: agent$respond=1 refers to those agents who were targeted by 
    # intervention and actually responded. For them, we have that:
    # exp_sq <- g and exp_alt <- h
    agent[which(agent$respond==1),"exp_sq"] <- g
    agent[which(agent$respond==1),"exp_alt"] <- h
    
    # Expected payoffs for all other agents:
    agent[which(agent$respond==0),"exp_sq"] <- ((1-agent[which(agent$respond==0),"q"])*(a + agent[which(agent$respond==0),"x_i"])) + (agent[which(agent$respond==0),"q"]*(b+agent[which(agent$respond==0),"x_i"]))
    agent[which(agent$respond==0),"exp_alt"] <- (1-agent[which(agent$respond==0),"q"])*a + agent[which(agent$respond==0),"q"]*d
    
    # Record average expected payoffs of this period:
    summary_results$exp_sq[t] <- sum(agent$exp_sq)/N
    summary_results$exp_alt[t] <- sum(agent$exp_alt)/N
    
    # Determine choice based on expected payoffs. Note: An agent chooses Alt if
    # exp_alt >= exp_sq:
    agent$choice <- ifelse(agent$exp_alt>=agent$exp_sq, 1, 0)
    
    # Match agents to play coordination game and assign corresponding payoffs:
    
    if (t < (t_max / 2)) {
      
      # Note, up until t=(t_max/2), agents will be paired within groups with probability s=1
      # Hence, agents will only be paired within groups
      # We keep the option of manipulating s before t=(t_max/2)
      
      # Initialize a vector to accumulate indices of agents selected for within-group matching
      selected_for_within_group <- integer(0)
      
      # Initialize frequency counters for within-group coordination outside the loop
      freq_coord_sq_within_total <- 0
      freq_coord_alt_within_total <- 0
      
      # Step 1: Within-group matching based on s_new
      for (g in 1:G) {
        
        group_indices <- which(agent$group == g)
        within_group_selected <- sample(group_indices, size = floor(length(group_indices) * s))
        
        # Accumulate selected indices
        selected_for_within_group <- c(selected_for_within_group, within_group_selected)
        
        shuffled_within_selected <- sample(within_group_selected)
        
        # Split and pair
        player_1_within <- shuffled_within_selected[1:(length(shuffled_within_selected) / 2)]
        player_2_within <- shuffled_within_selected[((length(shuffled_within_selected) / 2) + 1):length(shuffled_within_selected)]
        
        # Calculate coordination, miscoordination, and payoffs for these matches
        
        # Retrieve choice values for each half
        choice_player_1 <- agent$choice[player_1_within]
        choice_player_2 <- agent$choice[player_2_within]
        
        # Compare choice values and record frequency of coordination
        both_sq <- choice_player_1 == 0 & choice_player_2 == 0
        both_alt <- choice_player_1 == 1 & choice_player_2 == 1
        
        # Accumulate coordination frequencies from both groups
        freq_coord_sq_within_total <- freq_coord_sq_within_total + sum(both_sq)
        freq_coord_alt_within_total <- freq_coord_alt_within_total + sum(both_alt)
        
        # Calculate payoffs:
        payoff_player_1 <- numeric(length(player_1_within))
        payoff_player_2 <- numeric(length(player_2_within))
        
        # If both coordinate on SQ, payoff is: a + x_i 
        payoff_player_1[both_sq] <- a + agent$x_i[player_1_within][both_sq]
        payoff_player_2[both_sq] <- a + agent$x_i[player_2_within][both_sq]
        
        # If both coordinate on Alt, payoff is 'd' for non-targeted agents and 
        # for agents who were targeted but didn't switch:
        payoff_player_1[both_alt] <- d
        payoff_player_2[both_alt] <- d
        
        # If player 1 chooses SQ and player 2 chooses Alt:
        payoff_player_1[choice_player_1 == 0 & choice_player_2 == 1] <- b + agent$x_i[player_1_within][choice_player_1 == 0 & choice_player_2 == 1] 
        payoff_player_2[choice_player_1 == 0 & choice_player_2 == 1] <- a
        
        # If player 1 chooses Alt and player 2 chooses SQ:
        payoff_player_1[choice_player_1 == 1 & choice_player_2 == 0] <- a
        payoff_player_2[choice_player_1 == 1 & choice_player_2 == 0] <- b + agent$x_i[player_2_within][choice_player_1 == 1 & choice_player_2 == 0] 
        
        # Register payoffs in agent data frame
        agent[player_1_within,"payoff"] <- payoff_player_1
        agent[player_2_within,"payoff"] <- payoff_player_2
        
        # Note: All agents that initially responded to intervention always choose
        # Alt and receive h in each period:
        agent[which(agent$respond==1),"payoff"] <- h
        
      }
      
      # Record summary results
      
      # Add up the coordination/miscoordination from within and across group matchings
      
      # When adding to the summary results, use these accumulated totals
      summary_results$freq_coord_sq[t] <- freq_coord_sq_within_total 
      summary_results$freq_coord_alt[t] <- freq_coord_alt_within_total 
      
      summary_results$miscoordination[t] <- (N/2) - summary_results$freq_coord_sq[t] - summary_results$freq_coord_alt[t]
      
      # Record frequencies of choices:
      summary_results$freq_sq[t] <- length(agent[which(agent$choice==0),"choice"])
      summary_results$freq_alt[t] <- length(agent[which(agent$choice==1),"choice"])
      
      summary_results$freq_sq_group1[t] <- length(agent[which(agent$choice==0 & agent$group==1),"choice"])
      summary_results$freq_alt_group1[t] <- length(agent[which(agent$choice==1 & agent$group==1),"choice"])
      
      summary_results$freq_sq_group2[t] <- length(agent[which(agent$choice==0 & agent$group==2),"choice"])
      summary_results$freq_alt_group2[t] <- length(agent[which(agent$choice==1 & agent$group==2),"choice"])

      # Record average payoffs:
      num_agents_sq <- length(agent[which(agent$choice==0),"choice"])
      summary_results$avg_payoff_sq[t] <- ifelse(num_agents_sq>0,sum(agent[which(agent$choice==0),"payoff"]) / num_agents_sq,NA) 
      
      num_agents_alt <- length(agent[which(agent$choice==1),"choice"])
      summary_results$avg_payoff_alt[t] <- ifelse(num_agents_alt>0,sum(agent[which(agent$choice==1),"payoff"]) / num_agents_alt,NA) 
      
      summary_results$avg_payoff[t] <- sum(agent[,"payoff"])/N
      
      # Calculate and store Gini coefficient:
      summary_results$gini_coefficient[t] <- my_gini(agent[,"payoff"])
      
    } else {
      
      # Note, starting from t=(t_max/2), agents will be paired within groups with probability s_new
      # and across groups with probability 1-s_new
      
      # Initialize a vector to accumulate indices of agents selected for within-group matching
      selected_for_within_group <- integer(0)
      
      # Initialize frequency counters for within-group coordination outside the loop
      freq_coord_sq_within_total <- 0
      freq_coord_alt_within_total <- 0
      
      # Step 1: Within-group matching based on s_new
      for (g in 1:G) {
        
        group_indices <- which(agent$group == g)
        within_group_selected <- sample(group_indices, size = floor(length(group_indices) * s_new))
        
        # Accumulate selected indices
        selected_for_within_group <- c(selected_for_within_group, within_group_selected)
        
        shuffled_within_selected <- sample(within_group_selected)
        
        # Split and pair
        player_1_within <- shuffled_within_selected[1:(length(shuffled_within_selected) / 2)]
        player_2_within <- shuffled_within_selected[((length(shuffled_within_selected) / 2) + 1):length(shuffled_within_selected)]
        
        # Calculate coordination, miscoordination, and payoffs for these matches
        
        # Retrieve choice values for each half
        choice_player_1 <- agent$choice[player_1_within]
        choice_player_2 <- agent$choice[player_2_within]
        
        # Compare choice values and record frequency of coordination
        both_sq <- choice_player_1 == 0 & choice_player_2 == 0
        both_alt <- choice_player_1 == 1 & choice_player_2 == 1
        
        # Accumulate coordination frequencies from both groups
        freq_coord_sq_within_total <- freq_coord_sq_within_total + sum(both_sq)
        freq_coord_alt_within_total <- freq_coord_alt_within_total + sum(both_alt)
        
        # Calculate payoffs:
        payoff_player_1 <- numeric(length(player_1_within))
        payoff_player_2 <- numeric(length(player_2_within))
        
        # If both coordinate on SQ, payoff is: a + x_i 
        payoff_player_1[both_sq] <- a + agent$x_i[player_1_within][both_sq]
        payoff_player_2[both_sq] <- a + agent$x_i[player_2_within][both_sq]
        
        # If both coordinate on Alt, payoff is 'd' for non-targeted agents and 
        # for agents who were targeted but didn't switch:
        payoff_player_1[both_alt] <- d
        payoff_player_2[both_alt] <- d
        
        # If player 1 chooses SQ and player 2 chooses Alt:
        payoff_player_1[choice_player_1 == 0 & choice_player_2 == 1] <- b + agent$x_i[player_1_within][choice_player_1 == 0 & choice_player_2 == 1] 
        payoff_player_2[choice_player_1 == 0 & choice_player_2 == 1] <- a
        
        # If player 1 chooses Alt and player 2 chooses SQ:
        payoff_player_1[choice_player_1 == 1 & choice_player_2 == 0] <- a
        payoff_player_2[choice_player_1 == 1 & choice_player_2 == 0] <- b + agent$x_i[player_2_within][choice_player_1 == 1 & choice_player_2 == 0] 
        
        # Register payoffs in agent data frame
        agent[player_1_within,"payoff"] <- payoff_player_1
        agent[player_2_within,"payoff"] <- payoff_player_2
        
        # Note: All agents that initially responded to intervention always choose
        # Alt and receive h in each period:
        agent[which(agent$respond==1),"payoff"] <- h
        
      }
      
      # Step 2: Across-group matching based on 1 - s_new
      # Identify the remaining agents who haven't been matched within their group
      
      # Step 2: Across-group matching
      remaining_group_1 <- setdiff(which(agent$group == 1), selected_for_within_group)
      remaining_group_2 <- setdiff(which(agent$group == 2), selected_for_within_group)
      
      # Ensure equal number for fair pairing
      min_length <- min(length(remaining_group_1), length(remaining_group_2))
      player_1_across <- sample(remaining_group_1, min_length)
      player_2_across <- sample(remaining_group_2, min_length)
      
      # Calculate coordination, miscoordination, and payoffs for these matches
      
      # Retrieve choice values for each half
      choice_player_1 <- agent$choice[player_1_across]
      choice_player_2 <- agent$choice[player_2_across]
      
      # Compare choice values and record frequency of coordination
      both_sq <- choice_player_1 == 0 & choice_player_2 == 0
      both_alt <- choice_player_1 == 1 & choice_player_2 == 1
      freq_coord_sq_across <- sum(both_sq)
      freq_coord_alt_across <- sum(both_alt)
      
      # Calculate payoffs:
      payoff_player_1 <- numeric(length(player_1_across))
      payoff_player_2 <- numeric(length(player_2_across))
      
      # If both coordinate on SQ, payoff is: a + x_i 
      payoff_player_1[both_sq] <- a + agent$x_i[player_1_across][both_sq]
      payoff_player_2[both_sq] <- a + agent$x_i[player_2_across][both_sq]
      
      # If both coordinate on Alt, payoff is 'd' for non-targeted agents and 
      # for agents who were targeted but didn't switch:
      payoff_player_1[both_alt] <- d
      payoff_player_2[both_alt] <- d
      
      # If player 1 chooses SQ and player 2 chooses Alt:
      payoff_player_1[choice_player_1 == 0 & choice_player_2 == 1] <- b + agent$x_i[player_1_across][choice_player_1 == 0 & choice_player_2 == 1] 
      payoff_player_2[choice_player_1 == 0 & choice_player_2 == 1] <- a
      
      # If player 1 chooses Alt and player 2 chooses SQ:
      payoff_player_1[choice_player_1 == 1 & choice_player_2 == 0] <- a
      payoff_player_2[choice_player_1 == 1 & choice_player_2 == 0] <- b + agent$x_i[player_2_across][choice_player_1 == 1 & choice_player_2 == 0] 
      
      # Register payoffs in agent data frame
      agent[player_1_across,"payoff"] <- payoff_player_1
      agent[player_2_across,"payoff"] <- payoff_player_2
      
      # Note: All agents that initially responded to intervention always choose
      # Alt and receive h in each period:
      agent[which(agent$respond==1),"payoff"] <- h
      
      # Record summary results
      
      # Add up the coordination/miscoordination from within and across group matchings
      
      # When adding to the summary results, use these accumulated totals
      summary_results$freq_coord_sq[t] <- freq_coord_sq_within_total + freq_coord_sq_across
      summary_results$freq_coord_alt[t] <- freq_coord_alt_within_total + freq_coord_alt_across
      
      summary_results$miscoordination[t] <- (N/2) - summary_results$freq_coord_sq[t] - summary_results$freq_coord_alt[t]
      
      # Record frequencies of choices:
      summary_results$freq_sq[t] <- length(agent[which(agent$choice==0),"choice"])
      summary_results$freq_alt[t] <- length(agent[which(agent$choice==1),"choice"])
      
      summary_results$freq_sq_group1[t] <- length(agent[which(agent$choice==0 & agent$group==1),"choice"])
      summary_results$freq_alt_group1[t] <- length(agent[which(agent$choice==1 & agent$group==1),"choice"])
      
      summary_results$freq_sq_group2[t] <- length(agent[which(agent$choice==0 & agent$group==2),"choice"])
      summary_results$freq_alt_group2[t] <- length(agent[which(agent$choice==1 & agent$group==2),"choice"])
      
      # Record average payoffs:
      num_agents_sq <- length(agent[which(agent$choice==0),"choice"])
      summary_results$avg_payoff_sq[t] <- ifelse(num_agents_sq>0,sum(agent[which(agent$choice==0),"payoff"]) / num_agents_sq,NA) 
      
      num_agents_alt <- length(agent[which(agent$choice==1),"choice"])
      summary_results$avg_payoff_alt[t] <- ifelse(num_agents_alt>0,sum(agent[which(agent$choice==1),"payoff"]) / num_agents_alt,NA) 
      
      summary_results$avg_payoff[t] <- sum(agent[,"payoff"])/N
      
      # Calculate and store Gini coefficient:
      summary_results$gini_coefficient[t] <- my_gini(agent[,"payoff"])
      
    }
    
  }
  
  # Return the required data frames, and some parameter values in a list:
  return(list(summary_results=summary_results, # agent_output=agent_output, 
              N=N, t_max=t_max, alpha=alpha, beta=beta, target=target, phi=phi, a=a, h=h, s_new=s_new, n=n))
  
}

################################################################################
################################################################################

# Parameter values:

param_combinations <- expand.grid(
  alpha = c(2.75),
  beta = c(2),
  target = c(0,1),
  phi = c(0.75),
  a = c(0.75),
  h = c(2),
  s_new = c(0.5, 0.6),
  n = c(25, 50, 100)
)

################################################################################
################################################################################

# RUN SIMULATION:

################################################################################
################################################################################

# Run simulations n_sim times per parameter combination to account for the random 
# x values drawn from the beta distribution, which could significantly influence 
# the results of a given simulation.
# It also accounts for another random process in the simulation function, namely
# where we draw random values from a uniform distribution to determine which 
# agents actually respond to the intervention.

N <- 1000
t_max <- 100
n_sim <- 1000 # nr. of simulation runs

results_list <- list()  # to store results for each parameter combination

# Go through all the different parameter combinations
for (i in 1:nrow(param_combinations)) {
  
  current_params <- param_combinations[i, ]
  
  avg_summary_results <- data.frame(freq_coord_sq=rep(0,t_max), 
                                    low_ci_freq_coord_sq=rep(0,t_max),
                                    high_ci_freq_coord_sq=rep(0,t_max),
                                    freq_coord_alt=rep(0,t_max),
                                    low_ci_freq_coord_alt=rep(0,t_max),
                                    high_ci_freq_coord_alt=rep(0,t_max),
                                    miscoordination=rep(0,t_max),
                                    low_ci_miscoordination=rep(0,t_max),
                                    high_ci_miscoordination=rep(0,t_max),
                                    freq_sq=rep(0,t_max),
                                    low_ci_freq_sq=rep(0,t_max),
                                    high_ci_freq_sq=rep(0,t_max),
                                    freq_alt=rep(0,t_max),
                                    low_ci_freq_alt=rep(0,t_max),
                                    high_ci_freq_alt=rep(0,t_max),
                                    freq_sq_group1=rep(0,t_max),
                                    low_ci_freq_sq_group1=rep(0,t_max),
                                    high_ci_freq_sq_group1=rep(0,t_max),
                                    freq_alt_group1=rep(0,t_max),
                                    low_ci_freq_alt_group1=rep(0,t_max),
                                    high_ci_freq_alt_group1=rep(0,t_max),
                                    freq_sq_group2=rep(0,t_max),
                                    low_ci_freq_sq_group2=rep(0,t_max),
                                    high_ci_freq_sq_group2=rep(0,t_max),
                                    freq_alt_group2=rep(0,t_max),
                                    low_ci_freq_alt_group2=rep(0,t_max),
                                    high_ci_freq_alt_group2=rep(0,t_max),
                                    avg_payoff_sq=rep(0,t_max),
                                    avg_payoff_alt=rep(0,t_max),
                                    avg_payoff=rep(0,t_max),
                                    low_ci_avg_payoff=rep(0,t_max),
                                    high_ci_avg_payoff=rep(0,t_max),
                                    gini_coefficient=rep(0,t_max),
                                    low_ci_gini_coefficient=rep(0,t_max),
                                    high_ci_gini_coefficient=rep(0,t_max),
                                    exp_sq=rep(0,t_max),
                                    exp_alt=rep(0,t_max))
  

  
  # Create data frame to store the following measures for each individual simulation run:
  # freq_sq, freq_alt, avg_payoff, gini_coefficient (others may or may not be necessary)
  # We do so, to use this data later to calculate confidence intervals:
  
  freq_coord_sq_n_sim <- matrix(0,t_max,n_sim)
  freq_coord_alt_n_sim <- matrix(0,t_max,n_sim)
  miscoordination_n_sim <- matrix(0,t_max,n_sim)
  freq_sq_n_sim <- matrix(0,t_max,n_sim)
  freq_alt_n_sim <- matrix(0,t_max,n_sim)
  
  freq_sq_group1_n_sim <- matrix(0,t_max,n_sim)
  freq_alt_group1_n_sim <- matrix(0,t_max,n_sim)
  
  freq_sq_group2_n_sim <- matrix(0,t_max,n_sim)
  freq_alt_group2_n_sim <- matrix(0,t_max,n_sim)
  
  # avg_payoff_sq_n_sim <- matrix(0,t_max,n_sim)
  # avg_payoff_alt_n_sim <- matrix(0,t_max,n_sim)
  avg_payoff_n_sim <- matrix(0,t_max,n_sim)
  gini_coefficient_n_sim <- matrix(0,t_max,n_sim)
  # exp_sq_n_sim <- matrix(0,t_max,n_sim)
  # exp_alt_n_sim <- matrix(0,t_max,n_sim)
  
  # Run n_sim simulations:
  for (j in 1:n_sim) {
    results <- sim(N=N, t_max=t_max,
                   alpha=current_params$alpha,
                   beta=current_params$beta,
                   target=current_params$target, 
                   phi=current_params$phi, 
                   a=current_params$a, 
                   h=current_params$h,
                   s_new=current_params$s_new,
                   n=current_params$n)
    
    # Density plot of x values:
    # ggplot(data = data.frame(x = results$agent_output[,"x_i",1]), aes(x)) +
    #   geom_density() +
    #   labs(title = "Density Plot of x_i", x = "x_i", y = "Density")
    
    N <- N
    t_max <- t_max
    alpha <- current_params$alpha
    beta <- current_params$beta
    target <- current_params$target 
    phi <- current_params$phi 
    a <- current_params$a 
    h <- current_params$h
    s_new <- current_params$s_new
    n <- current_params$n
    
    # Record following measures for all n_sim simulation runs
    freq_sq_n_sim[,j] <- results$summary_results$freq_sq
    freq_alt_n_sim[,j] <- results$summary_results$freq_alt
    
    freq_sq_group1_n_sim[,j] <- results$summary_results$freq_sq_group1
    freq_alt_group1_n_sim[,j] <- results$summary_results$freq_alt_group1
    
    freq_sq_group2_n_sim[,j] <- results$summary_results$freq_sq_group2
    freq_alt_group2_n_sim[,j] <- results$summary_results$freq_alt_group2
    
    freq_coord_sq_n_sim[,j] <- results$summary_results$freq_coord_sq
    freq_coord_alt_n_sim[,j] <- results$summary_results$freq_coord_alt
    miscoordination_n_sim[,j] <- results$summary_results$miscoordination
    avg_payoff_n_sim[,j] <- results$summary_results$avg_payoff
    gini_coefficient_n_sim[,j] <- results$summary_results$gini_coefficient
    
  }
  
  # After running n_sim simulations for a given parameter combination, we take
  # averages of all the measures:
  avg_summary_results$freq_coord_sq <- rowMeans(freq_coord_sq_n_sim)
  avg_summary_results$freq_coord_alt <- rowMeans(freq_coord_alt_n_sim)
  avg_summary_results$miscoordination <- rowMeans(miscoordination_n_sim)
  avg_summary_results$freq_sq <- rowMeans(freq_sq_n_sim)
  avg_summary_results$freq_alt <- rowMeans(freq_alt_n_sim)
  
  avg_summary_results$freq_sq_group1 <- rowMeans(freq_sq_group1_n_sim)
  avg_summary_results$freq_alt_group1 <- rowMeans(freq_alt_group1_n_sim)
  
  avg_summary_results$freq_sq_group2 <- rowMeans(freq_sq_group2_n_sim)
  avg_summary_results$freq_alt_group2 <- rowMeans(freq_alt_group2_n_sim)
  
  avg_summary_results$avg_payoff <- rowMeans(avg_payoff_n_sim)
  avg_summary_results$gini_coefficient <- rowMeans(gini_coefficient_n_sim)
  
  # Now that we have the mean values, averaged over the n_sim simulation runs, 
  # we can calculate the bootstrapped confidence intervals:
  
  all_measures <- list()
  
  all_measures[[1]] <- freq_sq_n_sim
  all_measures[[2]] <- freq_alt_n_sim
  all_measures[[3]] <- freq_coord_sq_n_sim
  all_measures[[4]] <- freq_coord_alt_n_sim
  all_measures[[5]] <- miscoordination_n_sim
  all_measures[[6]] <- avg_payoff_n_sim
  all_measures[[7]] <- gini_coefficient_n_sim
  
  all_measures[[8]] <- freq_sq_group1_n_sim
  all_measures[[9]] <- freq_alt_group1_n_sim
  
  all_measures[[10]] <- freq_sq_group2_n_sim
  all_measures[[11]] <- freq_alt_group2_n_sim
  
  
  all_ci_values <- array(0, dim = c(t_max, 2, length(all_measures))) 
  
  
  # rename columns
  dimnames(all_ci_values) <- list(
    period = 1:t_max,
    ci_values = c("lower_bound", "upper_bound"),
    measures = c("freq_sq_n_sim", "freq_alt_n_sim", "freq_coord_sq_n_sim", "freq_coord_alt_n_sim", "miscoordination_n_sim", "avg_payoff_n_sim", "gini_coefficient_n_sim", "freq_sq_group1_n_sim", "freq_alt_group1_n_sim", "freq_sq_group2_n_sim", "freq_alt_group2_n_sim")
  )
  
  for (k in 1:length(all_measures)) {
    
    # Number of bootstrap replicates
    R <- 999
    
    # Create an empty matrix to store the confidence intervals
    conf_intervals <- matrix(NA, nrow = t_max, ncol = 2)
    
    for (period in 1:t_max) {
      # Go through the "all_measures" list and for each measure, extract the
      # data for current period
      data_to_bootstrap <- all_measures[[k]][period, ]
      
      # Initialize an empty vector to store bootstrap means
      bootstrap_means <- numeric(R)
      
      # Perform bootstrapping R times
      for (l in 1:R) {
        # Resample the data with replacement
        resampled_data <- sample(data_to_bootstrap, replace = TRUE)
        
        # Calculate the mean for the resampled data
        bootstrap_means[l] <- mean(resampled_data)
      }
      
      mean_and_boot <- rep(0,1+R)
      
      mean_and_boot[1:R] <- bootstrap_means
      mean_and_boot[R+1] <- rowMeans(all_measures[[k]])[period]
      
      ordered_means <- sort(mean_and_boot)
      
      index_low <- floor(0.025 * length(ordered_means))
      lower_bound <- ordered_means[index_low]
      
      index_high <- ceiling(0.975 * length(ordered_means))
      upper_bound <- ordered_means[index_high]
      
      # Store the confidence interval in the "all_ci_values" matrix
      all_ci_values[period,1,k] <- lower_bound
      all_ci_values[period,2,k] <- upper_bound
      
    }
    
    # print(all_ci_values)
    
  }  
  
  # Record confidence intervals in avg_summary_results data frame
  avg_summary_results$low_ci_freq_sq <- all_ci_values[,"lower_bound","freq_sq_n_sim"]
  avg_summary_results$high_ci_freq_sq <- all_ci_values[,"upper_bound","freq_sq_n_sim"]
  avg_summary_results$low_ci_freq_alt <- all_ci_values[,"lower_bound","freq_alt_n_sim"]
  avg_summary_results$high_ci_freq_alt <- all_ci_values[,"upper_bound","freq_alt_n_sim"]
  
  avg_summary_results$low_ci_freq_sq_group1 <- all_ci_values[,"lower_bound","freq_sq_group1_n_sim"]
  avg_summary_results$high_ci_freq_sq_group1 <- all_ci_values[,"upper_bound","freq_sq_group1_n_sim"]
  avg_summary_results$low_ci_freq_alt_group1 <- all_ci_values[,"lower_bound","freq_alt_group1_n_sim"]
  avg_summary_results$high_ci_freq_alt_group1 <- all_ci_values[,"upper_bound","freq_alt_group1_n_sim"]
  
  avg_summary_results$low_ci_freq_sq_group2 <- all_ci_values[,"lower_bound","freq_sq_group2_n_sim"]
  avg_summary_results$high_ci_freq_sq_group2 <- all_ci_values[,"upper_bound","freq_sq_group2_n_sim"]
  avg_summary_results$low_ci_freq_alt_group2 <- all_ci_values[,"lower_bound","freq_alt_group2_n_sim"]
  avg_summary_results$high_ci_freq_alt_group2 <- all_ci_values[,"upper_bound","freq_alt_group2_n_sim"]
  
  avg_summary_results$low_ci_freq_coord_sq <- all_ci_values[,"lower_bound","freq_coord_sq_n_sim"]
  avg_summary_results$high_ci_freq_coord_sq <- all_ci_values[,"upper_bound","freq_coord_sq_n_sim"]
  avg_summary_results$low_ci_freq_coord_alt <- all_ci_values[,"lower_bound","freq_coord_alt_n_sim"]
  avg_summary_results$high_ci_freq_coord_alt <- all_ci_values[,"upper_bound","freq_coord_alt_n_sim"]
  avg_summary_results$low_ci_miscoordination <- all_ci_values[,"lower_bound","miscoordination_n_sim"]
  avg_summary_results$high_ci_miscoordination <- all_ci_values[,"upper_bound","miscoordination_n_sim"]
  avg_summary_results$low_ci_avg_payoff <- all_ci_values[,"lower_bound","avg_payoff_n_sim"]
  avg_summary_results$high_ci_avg_payoff <- all_ci_values[,"upper_bound","avg_payoff_n_sim"]
  avg_summary_results$low_ci_gini_coefficient <- all_ci_values[,"lower_bound","gini_coefficient_n_sim"] 
  avg_summary_results$high_ci_gini_coefficient <- all_ci_values[,"upper_bound","gini_coefficient_n_sim"] 
  
  # Store averaged results and parameter values of given parameter combination in results_list:
  results_list[[i]] <- list(summary_results = avg_summary_results, # agent_output = all_agent_output, 
                            n_sim=n_sim, N=N, t_max=t_max, # num_traits=num_traits, 
                            alpha=alpha, beta=beta, target=target, phi=phi, a=a, h=h, s_new=s_new, n=n)
}

################################################################################
################################################################################
# Create names for files according to parameter combinations:
name_combination <- function(row) {
  paste0("alpha_", row["alpha"],
         "beta_", row["beta"],
         "_target_", row["target"], 
         "_phi_", row["phi"], 
         "_a_", row["a"],
         "_h_", row["h"],
         "_s_new_", row["s_new"],
         "_n_", row["n"])
}

names(results_list) <- apply(param_combinations, 1, name_combination)

# Replace '.' and '_' with '' (remove them)
cleaned_names <- lapply(names(results_list), function(name) {
  return(gsub("[._]", "", name))
})

cleaned_names <- unlist(cleaned_names)

# get current working directory
results_dir <- getwd() 

# Iterate through the results_list and save each result separately in 
# subdirectory corresponding to parameter combinations:
for(i in 1:length(results_list)) {
  # Name for the subdirectory (and the file) based on the names of results_list
  folder_and_file_name <- cleaned_names[i]
  
  # Check if directory exists and create if it doesn't
  if (!dir.exists(folder_and_file_name)) {
    dir.create(folder_and_file_name)
  }
  
  # File name with .RData extension
  file_name <- paste0(folder_and_file_name, ".RData")
  
  # Full path to where the file will be saved (inside the subdirectory)
  file_path <- file.path(results_dir, folder_and_file_name, file_name)
  
  # Convert list item to an environment
  e <- list2env(list(result = results_list[[i]]))
  
  # save the environment
  save(list = "result", envir = e, file = file_path)
}

################################################################################
################################################################################

# session information
xfun::session_info()

# cite R
citation()
