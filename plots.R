###################################################################################
# This is an adapted version of the code used for the following paper:
# Title: Analysis of threshold simulation model for paper: "When norm change hurts"
# Authors: Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.
###################################################################################

# Parameters specifying payoffs in coordination game that we don't vary:
b <- 0
d <- 1    
g <- 0 

G <- 2 # parameter specifying number of groups

###################################################################################
# The first part of this script produces several plots per parameter combination
# The second part produces combined plots where amenable and resistant targets
# are compared to each other with respect to four different measures
###################################################################################

# Part 1: Separate Plots

###################################################################################
# Load data with file names corresponding to parameter combinations
###################################################################################

base_dir <- getwd()

# List all subdirectories
subdirectories <- list.dirs(path = base_dir, recursive = FALSE)

# Loop through each subdirectory and load and process the .RData files
for (subdir in subdirectories) {
  # Get the list of .RData files in the current subdirectory
  rdata_files <- list.files(path = subdir, pattern = "\\.RData$", full.names = TRUE)
  
  # Initialize a list to store the results for the current subdirectory
  subdirectory_results <- list()
  
  # Load each .RData file
  for (rdata_file in rdata_files) {
    # Load the file into the current environment
    load(rdata_file)
    
    # Extract just the parameter combination name from the full path
    param_name <- basename(dirname(rdata_file))
    
    # Store result in subdirectory_results list
    subdirectory_results[[param_name]] <- result
  }
  
  ###################################################################################
  
  # Plotting parameters:
  
  windowWidth <- 12
  windowHeight <- 11
  
  cexEquil <- 2
  lwdEquil <- 2
  
  cexNum <- 1.5
  cexWord <- 2
  
  ###################################################################################
  ###################################################################################
  
  # Plots
  
  ###################################################################################
  ###################################################################################
  
  for (param_name in names(subdirectory_results)) {
    current_output <- subdirectory_results[[param_name]]
    

    ###################################################################################
    # Plotting frequencies of coordination and miscoordination
    ###################################################################################
    
    # Create the filename for this parameter combination
    filename <- file.path(subdir, "freq_coordination.pdf")
    
    pdf(width = windowWidth, height = windowHeight, file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create the initial line plot with coordination on SQ
    plot(1:current_output$t_max, current_output$summary_results$freq_coord_sq, type = "l",  lwd = 3,
         xlab = "Period", ylab = "Frequency",
         # main = "Frequency of (mis)coordination",
         col = "blue", ylim = c(0, max(current_output$summary_results$freq_coord_sq, na.rm = TRUE)),
         cex.axis = cexNum,cex.lab = cexWord)
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_coord_sq, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_coord_sq, col = "blue", lty = 2)
    
    # # Add coordination on Alt
    lines(1:current_output$t_max, current_output$summary_results$freq_coord_alt, col = "darkgreen", lty = 1, lwd = 3)
    
    # # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_coord_alt, col = "darkgreen", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_coord_alt, col = "darkgreen", lty = 2)
    
    # # Add miscoordination
    lines(1:current_output$t_max, current_output$summary_results$miscoordination, col = "red", lty = 1, lwd = 3)
    
    # # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_miscoordination, col = "red", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_miscoordination, col = "red", lty = 2)
    
    # # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI SQ", "Alt", "95% CI Alt", "Miscoordination", "95% CI Miscoord."), 
    #        col = c("blue","blue", "darkgreen", "darkgreen", "red", "red"), lty = c(1, 2, 1, 2, 1, 2))
    #  
    # # Close the graphics device and save the plot as a PDF file
    dev.off()
    
    ###################################################################################
    # Plotting choice fractions
    ###################################################################################
    current_output$summary_results$fract_alt <- current_output$summary_results$freq_alt / current_output$N
    current_output$summary_results$fract_sq <- current_output$summary_results$freq_sq / current_output$N
    current_output$summary_results$fract_low_ci_freq_sq <- current_output$summary_results$low_ci_freq_sq / current_output$N
    current_output$summary_results$fract_high_ci_freq_sq <- current_output$summary_results$high_ci_freq_sq / current_output$N
    current_output$summary_results$fract_low_ci_freq_alt <- current_output$summary_results$low_ci_freq_alt / current_output$N
    current_output$summary_results$fract_high_ci_freq_alt <- current_output$summary_results$high_ci_freq_alt / current_output$N
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "fract_choices.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create the initial line plot with SQ choices
    plot(1:current_output$t_max, current_output$summary_results$fract_sq, type = "l", lwd = 3,
         xlab = "Period", ylab = "Fraction",
         # main = "Fraction of choices",
         col = "blue", ylim = c(0, 1),
         cex.axis = cexNum,cex.lab = cexWord)
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_sq, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_sq, col = "blue", lty = 2)
    
    # Add Alt choices
    lines(1:current_output$t_max, current_output$summary_results$fract_alt, col = "darkgreen", lty = 1, lwd = 3)
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_alt, col = "darkgreen", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_alt, col = "darkgreen", lty = 2)
    
    # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI", "Alt", "95% CI"), 
    #        col = c("blue", "blue", "darkgreen", "darkgreen"), lty = c(1, 2, 1, 2))
    
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    ###################################################################################
    # Plotting average payoffs
    ###################################################################################
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "average_payoff.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create plot with average payoffs
    plot(1:current_output$t_max, current_output$summary_results$avg_payoff, type = "l", lwd = 3,
         xlab = "Period", ylab = "Average payoff",
         main = "Average payoffs",
         col = "blue", ylim = c(0, current_output$h),
         cex.axis = cexNum,cex.lab = cexWord)
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_avg_payoff, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_avg_payoff, col = "blue", lty = 2)
    
    # # If we only show overall avg payoffs:
    # legend("topright", legend = c("Average payoff", "95% CI"), col = c("blue", "blue"), lty = c(1, 2))
    # 
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    ###################################################################################
    # Plotting Gini Coefficients
    ###################################################################################
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "gini_coefficient.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create plot with gini coefficients
    plot(1:current_output$t_max, current_output$summary_results$gini_coefficient, type = "l", lwd = 3,
         xlab = "Period", ylab = "Gini Coefficient",
         # main = "Gini Coefficient",
         col = "blue", ylim = c(0, 1),
         cex.axis = cexNum,cex.lab = cexWord)
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
    
    # # If we only show overall avg payoffs:
    # legend("topright", legend = c("Gini Coefficient", "95% CI"), col = c("blue", "blue"), lty = c(1, 2))
    # 
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
  }
}




###################################################################################
###################################################################################

# Part 2: Combined plots

###################################################################################
###################################################################################

# List all subdirectories
subdirectories <- list.dirs(path = base_dir, recursive = FALSE)

# Initialize a list to store the loaded .RData files
loaded_data_list <- list()

# Loop through each subdirectory and load the corresponding .RData files
for (subdir in subdirectories) {
  # Get the list of .RData files in the current subdirectory
  rdata_files <- list.files(path = subdir, pattern = "\\.RData$", full.names = TRUE)
  
  # Extract just the parameter combination name from the subdirectory
  param_name <- basename(subdir)
  
  # Load each .RData file
  for (rdata_file in rdata_files) {
    # Load the file into the current environment
    load(rdata_file)
    
    # Store the loaded data in the list with the subdirectory name as the list element name
    loaded_data_list[[param_name]] <- result
  }
}

###################################################################################

# Gather all amenable treatments and all resistant treatments in separate lists
amenable_list <- list()
resistant_list <- list()

for (param_name in names(loaded_data_list)) {
  if (grepl("target0", param_name)) {
    amenable_list[[param_name]] <- loaded_data_list[[param_name]]
  } else if (grepl("target1", param_name)) {
    resistant_list[[param_name]] <- loaded_data_list[[param_name]]
  }
}

###################################################################################
###################################################################################

if (!dir.exists("combined_plots")) {
  dir.create("combined_plots")
}
setwd("combined_plots")


for (i in 1:length(amenable_list)) {
  
  # Plotting parameters:
  
  windowWidth <- 20
  windowHeight <- 15
  
  lineWidth <- 3
  
  cexNum <- 2
  cexWord <- 2.25
  
  # Retrieve alpha parameter value of current parameter combination, store it under "label", to use it in mtext() in plots
  
  label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
  
  ###################################################################################
  
  # Plot 1: Fraction of Alt choices (upper left)
  
  ###################################################################################
  
  pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                   "_phi", resistant_list[[i]]$phi, 
                   "_a", resistant_list[[i]]$a, 
                   "_h", resistant_list[[i]]$h,
                   "_s_new", resistant_list[[i]]$s_new,
                   "_n", resistant_list[[i]]$n,
                   ".pdf", sep = "") , width = windowWidth, height = windowHeight)
  
  par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
  
  # Have to calculate fractions first because in simulation we recorded frequencies
  
  # Amenable
  
  amenable_list[[i]]$summary_results$fract_alt <- amenable_list[[i]]$summary_results$freq_alt / amenable_list[[i]]$N
  amenable_list[[i]]$summary_results$fract_sq <- amenable_list[[i]]$summary_results$freq_sq / amenable_list[[i]]$N
  amenable_list[[i]]$summary_results$fract_low_ci_freq_sq <- amenable_list[[i]]$summary_results$low_ci_freq_sq / amenable_list[[i]]$N
  amenable_list[[i]]$summary_results$fract_high_ci_freq_sq <- amenable_list[[i]]$summary_results$high_ci_freq_sq / amenable_list[[i]]$N
  amenable_list[[i]]$summary_results$fract_low_ci_freq_alt <- amenable_list[[i]]$summary_results$low_ci_freq_alt / amenable_list[[i]]$N
  amenable_list[[i]]$summary_results$fract_high_ci_freq_alt <- amenable_list[[i]]$summary_results$high_ci_freq_alt / amenable_list[[i]]$N
  
  # Resistant:
  
  resistant_list[[i]]$summary_results$fract_alt <- resistant_list[[i]]$summary_results$freq_alt / resistant_list[[i]]$N
  resistant_list[[i]]$summary_results$fract_sq <- resistant_list[[i]]$summary_results$freq_sq / resistant_list[[i]]$N
  resistant_list[[i]]$summary_results$fract_low_ci_freq_sq <- resistant_list[[i]]$summary_results$low_ci_freq_sq / resistant_list[[i]]$N
  resistant_list[[i]]$summary_results$fract_high_ci_freq_sq <- resistant_list[[i]]$summary_results$high_ci_freq_sq / resistant_list[[i]]$N
  resistant_list[[i]]$summary_results$fract_low_ci_freq_alt <- resistant_list[[i]]$summary_results$low_ci_freq_alt / resistant_list[[i]]$N
  resistant_list[[i]]$summary_results$fract_high_ci_freq_alt <- resistant_list[[i]]$summary_results$high_ci_freq_alt / resistant_list[[i]]$N
  
  # plot par:
  par(fig = c(0,0.45,0.5,1))
  
  # Alt choices amenable
  plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
       xlab = "Period", ylab = "Fraction of Alt choices",
       # main = "a) Alt choices",
       col = "darkgreen", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  # Add confidence intervals
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "darkgreen", lty = 2)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "darkgreen", lty = 2)
  
  # Second parameter combination with dashed lines (Resistant)
  
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals for Alt (Resistant)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "red", lty = 2)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "red", lty = 2)
  
  # Add a legend with labels
  # legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))
  
  ###################################################################################
  
  # Plot 2: Miscoordination (Upper right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0.5,1),new = T)
  
  # Create the initial line plot with frequencies of miscoordination for amenable targets
  plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$miscoordination, type = "l",
       xlab = "Period", ylab = "Frequency of miscoordination",
       # main = "b) Coordination on SQ and Alt",
       col = "darkgreen", ylim = c(0, amenable_list[[i]]$N/2),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('b',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  # Add confidence intervals (amenable)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$low_ci_miscoordination, col = "darkgreen", lty = 2, lwd = 0.75)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$high_ci_miscoordination, col = "darkgreen", lty = 2, lwd = 0.75)
  
  # Miscoord resistant
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$miscoordination, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals (resistant)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$low_ci_miscoordination, col = "red", lty = 2, lwd = 0.75)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$high_ci_miscoordination, col = "red", lty = 2, lwd = 0.75)
  
  # Add a legend with labels
  # legend("topright", legend = c("SQ Amenable", "95% CI", "Alt Amenable", "95% CI", "SQ Resistant", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("lightgreen", "lightgreen", "darkgreen", "darkgreen", "pink", "pink", "red", "red"),
  #        lty = c(1, 2, 1, 2, 1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 3: Average payoffs (Lower left)
  
  ###################################################################################
  
  par(fig = c(0,0.45,0,0.5),new = T)
  
  # Create the initial line plot with Average payoffs for amenable targets
  plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$avg_payoff, type = "l",
       xlab = "Period", ylab = "Average payoff",
       # main = "c) Average payoffs",
       col = "darkgreen", ylim = c(0, amenable_list[[i]]$h),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('c',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord) 
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  # Add confidence intervals (amenable)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$low_ci_avg_payoff, col = "darkgreen", lty = 2, lwd = 0.75)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$high_ci_avg_payoff, col = "darkgreen", lty = 2, lwd = 0.75)
  
  # Average payoffs for resistant targets
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$avg_payoff, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals (resistant)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$low_ci_avg_payoff, col = "red", lty = 2, lwd = 0.75)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$high_ci_avg_payoff, col = "red", lty = 2, lwd = 0.75)
  
  # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 4: Gini coefficient (Lower right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0,0.5),new = T)
  
  # Create the initial line plot with Gini coefficients for amenable targets
  plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$gini_coefficient, type = "l",
       xlab = "Period", ylab = "Gini coefficient",
       # main = "d) Gini coefficient",
       col = "darkgreen", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('d',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  # Add confidence intervals (amenable)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$low_ci_gini_coefficient, col = "darkgreen", lty = 2)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$high_ci_gini_coefficient, col = "darkgreen", lty = 2)
  
  # Gini coefficients for resistant
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$gini_coefficient, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals (resistant)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$low_ci_gini_coefficient, col = "red", lty = 2)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$high_ci_gini_coefficient, col = "red", lty = 2)
  
  # # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  alpha_val <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha))
  beta_val <- bquote(italic(beta) == .(2))
  
  par(fig = c(0.9,1,0,1),new = T)
  par(mar=c(0,0,0,0))
  plot(0:1,0:1,xlab='',ylab='',type = 'l',lty = 'blank',axes=F)
  text(0.05, 0.52, alpha_val, cex = cexWord, adj = 0)
  text(0.05, 0.48, beta_val, cex = cexWord, adj = 0)
  
  
  dev.off()
  
  
  #########################################################################################################################
  #########################################################################################################################
  # SINGLE PLOT: Alt choice fractions for amenable and resistant targets as single plot (see subplot (a) in combined plot)
  #########################################################################################################################
  #########################################################################################################################
  
  for (i in 1:length(amenable_list)) {
    
    # Adjusted Plotting Parameters for a Single Main Plot
    windowWidth <- 12
    windowHeight <- 11
    lineWidth <- 2
    cexNum <- 1.5
    cexWord <- 2
    
    # Retrieve alpha parameter value for labeling
    label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
    
    # Open a PDF file for the plot
    pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                     "_phi", resistant_list[[i]]$phi, 
                     "_a", resistant_list[[i]]$a, 
                     "_h", resistant_list[[i]]$h,
                     "_s_new", resistant_list[[i]]$s_new,
                     "_n", resistant_list[[i]]$n,
                     ".pdf", sep = ""), width = windowWidth, height = windowHeight)
    
    # Adjust margins and plotting parameters
    par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(2.5, 0.7, 0))
    
    # Calculate fractions and confidence intervals for both amenable and resistant lists
    amenable_list[[i]]$summary_results$fract_alt <- amenable_list[[i]]$summary_results$freq_alt / amenable_list[[i]]$N
    amenable_list[[i]]$summary_results$fract_low_ci_freq_alt <- amenable_list[[i]]$summary_results$low_ci_freq_alt / amenable_list[[i]]$N
    amenable_list[[i]]$summary_results$fract_high_ci_freq_alt <- amenable_list[[i]]$summary_results$high_ci_freq_alt / amenable_list[[i]]$N
    
    resistant_list[[i]]$summary_results$fract_alt <- resistant_list[[i]]$summary_results$freq_alt / resistant_list[[i]]$N
    resistant_list[[i]]$summary_results$fract_low_ci_freq_alt <- resistant_list[[i]]$summary_results$low_ci_freq_alt / resistant_list[[i]]$N
    resistant_list[[i]]$summary_results$fract_high_ci_freq_alt <- resistant_list[[i]]$summary_results$high_ci_freq_alt / resistant_list[[i]]$N
    
    
    # Main Plot for Fraction of Alt Choices
    plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
         xlab = "period", ylab = "fraction of Alt choices", col = "darkgreen", ylim = c(0, 1),
         cex.axis = cexNum, cex.lab = cexWord)
    
    # Add lines for the resistant parameter combination
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt, col = "red", lty = 1, lwd = lineWidth)
    
    # Optionally, add a legend
    legend("bottomright", legend = c("Amenable", "Resistant"), col = c("darkgreen", "red"), lty = 1, cex = cexNum)
    
    # Add confidence intervals for amenable
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "darkgreen", lty = 2)
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "darkgreen", lty = 2)
    
    # Add confidence intervals for resistant
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "red", lty = 2)
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "red", lty = 2)
    
    # Close the PDF device
    dev.off()
  }
  
  
  #########################################################################################################################
  #########################################################################################################################
  # SINGLE PLOT: Alt choice fractions for amenable and resistant targets GROUP 1
  #########################################################################################################################
  #########################################################################################################################
   
  for (i in 1:length(amenable_list)) {
    
    # Adjusted Plotting Parameters for a Single Main Plot
    windowWidth <- 12
    windowHeight <- 11
    lineWidth <- 2
    cexNum <- 1.5
    cexWord <- 2
    
    # Retrieve alpha parameter value for labeling
    label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
    
    # Open a PDF file for the plot
    pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                     "_phi", resistant_list[[i]]$phi, 
                     "_a", resistant_list[[i]]$a, 
                     "_h", resistant_list[[i]]$h,
                     "_s_new", resistant_list[[i]]$s_new,
                     "_n", resistant_list[[i]]$n,
                     "group1.pdf", sep = ""), width = windowWidth, height = windowHeight)
    
    # Adjust margins and plotting parameters
    par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(2.5, 0.7, 0))
    
    # Calculate fractions and confidence intervals for group1 both amenable and resistant lists
    amenable_list[[i]]$summary_results$fract_alt_group1 <- amenable_list[[i]]$summary_results$freq_alt_group1 / (amenable_list[[i]]$N/G)
    amenable_list[[i]]$summary_results$fract_low_ci_freq_alt_group1 <- amenable_list[[i]]$summary_results$low_ci_freq_alt_group1 / (amenable_list[[i]]$N/G)
    amenable_list[[i]]$summary_results$fract_high_ci_freq_alt_group1 <- amenable_list[[i]]$summary_results$high_ci_freq_alt_group1 / (amenable_list[[i]]$N/G)
    
    resistant_list[[i]]$summary_results$fract_alt_group1 <- resistant_list[[i]]$summary_results$freq_alt_group1 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group1 <- resistant_list[[i]]$summary_results$low_ci_freq_alt_group1 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group1 <- resistant_list[[i]]$summary_results$high_ci_freq_alt_group1 / (resistant_list[[i]]$N/G)
    
    # Main Plot for Fraction of Alt Choices
    plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_alt_group1, type = "l", lwd = lineWidth,
         xlab = "period", ylab = "fraction of Alt choices in group 1", col = "darkgreen", ylim = c(0, 1),
         cex.axis = cexNum, cex.lab = cexWord)
    
    # Add lines for the resistant parameter combination
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt_group1, col = "red", lty = 1, lwd = lineWidth)
    
    # Optionally, add a legend
    legend("bottomright", legend = c("Amenable", "Resistant"), col = c("darkgreen", "red"), lty = 1, cex = cexNum)
    
    # Add confidence intervals for amenable
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt_group1, col = "darkgreen", lty = 2)
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt_group1, col = "darkgreen", lty = 2)
    
    # Add confidence intervals for resistant
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group1, col = "red", lty = 2)
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group1, col = "red", lty = 2)
    
    # Close the PDF device
    dev.off()
  }
  
  
  #########################################################################################################################
  #########################################################################################################################
  # SINGLE PLOT: Alt choice fractions for amenable and resistant targets GROUP 2
  #########################################################################################################################
  #########################################################################################################################
  
  for (i in 1:length(amenable_list)) {
    
    # Adjusted Plotting Parameters for a Single Main Plot
    windowWidth <- 12
    windowHeight <- 11
    lineWidth <- 2
    cexNum <- 1.5
    cexWord <- 2
    
    # Retrieve alpha parameter value for labeling
    label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
    
    # Open a PDF file for the plot
    pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                     "_phi", resistant_list[[i]]$phi, 
                     "_a", resistant_list[[i]]$a, 
                     "_h", resistant_list[[i]]$h,
                     "_s_new", resistant_list[[i]]$s_new,
                     "_n", resistant_list[[i]]$n,
                     "group2.pdf", sep = ""), width = windowWidth, height = windowHeight)
    
    # Adjust margins and plotting parameters
    par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(2.5, 0.7, 0))
    
    # Calculate fractions and confidence intervals for group2 both amenable and resistant lists
    amenable_list[[i]]$summary_results$fract_alt_group2 <- amenable_list[[i]]$summary_results$freq_alt_group2 / (amenable_list[[i]]$N/G)
    amenable_list[[i]]$summary_results$fract_low_ci_freq_alt_group2 <- amenable_list[[i]]$summary_results$low_ci_freq_alt_group2 / (amenable_list[[i]]$N/G)
    amenable_list[[i]]$summary_results$fract_high_ci_freq_alt_group2 <- amenable_list[[i]]$summary_results$high_ci_freq_alt_group2 / (amenable_list[[i]]$N/G)
    
    resistant_list[[i]]$summary_results$fract_alt_group2 <- resistant_list[[i]]$summary_results$freq_alt_group2 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group2 <- resistant_list[[i]]$summary_results$low_ci_freq_alt_group2 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group2 <- resistant_list[[i]]$summary_results$high_ci_freq_alt_group2 / (resistant_list[[i]]$N/G)
    
    # Main Plot for Fraction of Alt Choices
    plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_alt_group2, type = "l", lwd = lineWidth,
         xlab = "period", ylab = "fraction of Alt choices in group 2", col = "darkgreen", ylim = c(0, 1),
         cex.axis = cexNum, cex.lab = cexWord)
    
    # Add lines for the resistant parameter combination
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt_group2, col = "red", lty = 1, lwd = lineWidth)
    
    # Optionally, add a legend
    legend("bottomright", legend = c("Amenable", "Resistant"), col = c("darkgreen", "red"), lty = 1, cex = cexNum)
    
    # Add confidence intervals for amenable
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt_group2, col = "darkgreen", lty = 2)
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt_group2, col = "darkgreen", lty = 2)
    
    # Add confidence intervals for resistant
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group2, col = "red", lty = 2)
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group2, col = "red", lty = 2)
    
    # Close the PDF device
    dev.off()
  }

  
  
  
  #########################################################################################################################
  #########################################################################################################################
  # SINGLE PLOT: Alt choice fractions for ONLY RESISTANT targets as single plot (see subplot (a) in combined plot)
  #########################################################################################################################
  #########################################################################################################################
  
  for (i in 1:length(amenable_list)) {
    
    # Adjusted Plotting Parameters for a Single Main Plot
    windowWidth <- 12
    windowHeight <- 11
    lineWidth <- 2
    cexNum <- 1.5
    cexWord <- 2
    
    # Retrieve alpha parameter value for labeling
    label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
    
    # Open a PDF file for the plot
    pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                     "_phi", resistant_list[[i]]$phi, 
                     "_a", resistant_list[[i]]$a, 
                     "_h", resistant_list[[i]]$h,
                     "_s_new", resistant_list[[i]]$s_new,
                     "_n", resistant_list[[i]]$n,
                     "res.pdf", sep = ""), width = windowWidth, height = windowHeight)
    
    # Adjust margins and plotting parameters
    par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(2.5, 0.7, 0))
    
    # Calculate fractions and confidence intervals for both amenable and resistant lists
    resistant_list[[i]]$summary_results$fract_alt <- resistant_list[[i]]$summary_results$freq_alt / resistant_list[[i]]$N
    resistant_list[[i]]$summary_results$fract_low_ci_freq_alt <- resistant_list[[i]]$summary_results$low_ci_freq_alt / resistant_list[[i]]$N
    resistant_list[[i]]$summary_results$fract_high_ci_freq_alt <- resistant_list[[i]]$summary_results$high_ci_freq_alt / resistant_list[[i]]$N
    
    
    # Main Plot for Fraction of Alt Choices
    plot(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt, col = "red", type = "l", lwd = lineWidth,
         xlab = "period", ylab = "fraction of Alt choices", ylim = c(0, 1),
         cex.axis = cexNum, cex.lab = cexWord)

    # Optionally, add a legend
    # legend("bottomright", legend = c("Resistant"), col = c("red"), lty = 1, cex = cexNum)
    
    # Add confidence intervals for amenable
    # lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "darkgreen", lty = 2)
    # lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "darkgreen", lty = 2)
    
    # Add confidence intervals for resistant
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "red", lty = 2)
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "red", lty = 2)
    
    # Close the PDF device
    dev.off()
  }
  
  
  #########################################################################################################################
  #########################################################################################################################
  # SINGLE PLOT: Alt choice fractions for resistant targets GROUP 1
  #########################################################################################################################
  #########################################################################################################################
  
  for (i in 1:length(amenable_list)) {
    
    # Adjusted Plotting Parameters for a Single Main Plot
    windowWidth <- 12
    windowHeight <- 11
    lineWidth <- 2
    cexNum <- 1.5
    cexWord <- 2
    
    # Retrieve alpha parameter value for labeling
    label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
    
    # Open a PDF file for the plot
    pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                     "_phi", resistant_list[[i]]$phi, 
                     "_a", resistant_list[[i]]$a, 
                     "_h", resistant_list[[i]]$h,
                     "_s_new", resistant_list[[i]]$s_new,
                     "_n", resistant_list[[i]]$n,
                     "group1_res.pdf", sep = ""), width = windowWidth, height = windowHeight)
    
    # Adjust margins and plotting parameters
    par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(2.5, 0.7, 0))

    resistant_list[[i]]$summary_results$fract_alt_group1 <- resistant_list[[i]]$summary_results$freq_alt_group1 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group1 <- resistant_list[[i]]$summary_results$low_ci_freq_alt_group1 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group1 <- resistant_list[[i]]$summary_results$high_ci_freq_alt_group1 / (resistant_list[[i]]$N/G)
    
    # Main Plot for Fraction of Alt Choices
    plot(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt_group1, col = "red", type = "l", lwd = lineWidth,
         xlab = "period", ylab = "fraction of Alt choices in group 1", ylim = c(0, 1),
         cex.axis = cexNum, cex.lab = cexWord)

    # Optionally, add a legend
    # legend("bottomright", legend = c("Amenable", "Resistant"), col = c("darkgreen", "red"), lty = 1, cex = cexNum)
    
    # Add confidence intervals for amenable
    # lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt_group1, col = "darkgreen", lty = 2)
    # lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt_group1, col = "darkgreen", lty = 2)
    
    # Add confidence intervals for resistant
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group1, col = "red", lty = 2)
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group1, col = "red", lty = 2)
    
    # Close the PDF device
    dev.off()
  }
  
  
  #########################################################################################################################
  #########################################################################################################################
  # SINGLE PLOT: Alt choice fractions for amenable and resistant targets GROUP 2
  #########################################################################################################################
  #########################################################################################################################
  
  for (i in 1:length(amenable_list)) {
    
    # Adjusted Plotting Parameters for a Single Main Plot
    windowWidth <- 12
    windowHeight <- 11
    lineWidth <- 2
    cexNum <- 1.5
    cexWord <- 2
    
    # Retrieve alpha parameter value for labeling
    label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
    
    # Open a PDF file for the plot
    pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                     "_phi", resistant_list[[i]]$phi, 
                     "_a", resistant_list[[i]]$a, 
                     "_h", resistant_list[[i]]$h,
                     "_s_new", resistant_list[[i]]$s_new,
                     "_n", resistant_list[[i]]$n,
                     "group2_res.pdf", sep = ""), width = windowWidth, height = windowHeight)
    
    # Adjust margins and plotting parameters
    par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(2.5, 0.7, 0))
    
    resistant_list[[i]]$summary_results$fract_alt_group2 <- resistant_list[[i]]$summary_results$freq_alt_group2 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group2 <- resistant_list[[i]]$summary_results$low_ci_freq_alt_group2 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group2 <- resistant_list[[i]]$summary_results$high_ci_freq_alt_group2 / (resistant_list[[i]]$N/G)
    
    # Main Plot for Fraction of Alt Choices
    plot(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt_group2, col = "red", type = "l", lwd = lineWidth,
         xlab = "period", ylab = "fraction of Alt choices in group 2", ylim = c(0, 1),
         cex.axis = cexNum, cex.lab = cexWord)

    # Optionally, add a legend
    # legend("bottomright", legend = c("Amenable", "Resistant"), col = c("darkgreen", "red"), lty = 1, cex = cexNum)
    
    # Add confidence intervals for amenable
    # lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "darkgreen", lty = 2)
    # lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "darkgreen", lty = 2)
    
    # Add confidence intervals for resistant
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group2, col = "red", lty = 2)
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group2, col = "red", lty = 2)
    
    # Close the PDF device
    dev.off()
  }
  
}



setwd('..')

###################################################################################
# Calculate Gini differences
###################################################################################

# In the Gini coefficient subplot, we can see the difference in Gini coefficient
# between amenable and resistant target.
# Here, we want to add a quantitative measure to retrieve the largest difference 
# of the Gini coefficient when comparing amenable and resistant target for a 
# given parameter combination of alpha, a, h, and phi.

# # Display of decimals:
# options(scipen = 999)
# 
# gini_diffs <- rep(NA, length(amenable_list))
# 
# for (i in 1:length(amenable_list)) {
#   
#   gini_diffs[i] <- amenable_list[[i]]$summary_results$gini_coefficient[amenable_list[[i]]$t_max] - resistant_list[[i]]$summary_results$gini_coefficient[amenable_list[[i]]$t_max]
#   
# }
# 
# # Take absolute values
# absolute_gini_diffs <- abs(gini_diffs)
# 
# # Plot density
# 
# plot(1:length(absolute_gini_diffs), absolute_gini_diffs)
# 
# sorted_gini_diff <- sort(absolute_gini_diffs)
# 
# plot(1:length(absolute_gini_diffs), sorted_gini_diff)
# 
# hist(absolute_gini_diffs)
# 
# # Average difference
# average_gini_diff <- mean(absolute_gini_diffs)
# print(average_gini_diff)
# 
# # Largest difference:
# max_gini_diff <- max(absolute_gini_diffs)
# print(max_gini_diff)



