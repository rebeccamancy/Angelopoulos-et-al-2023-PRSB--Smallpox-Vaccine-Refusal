#######################################################################################
#         Function to compile fracreg outputs into a format for easy plotting
#
# In two steps: First, we compile output for time-invariant variables, and then compile
# output for the time-varying variables
#######################################################################################
require(gtools) # for stars from pvals

compile_fracreg_outputs <- function(Experience_var_name, Count_rate, Deaths_cases, 
                                    Model, With_controls, 
                                    margins_output, margins_output_years, clustered_se,
                                    descriptive_stats, COV_descriptive_stats, mean_COV,
                                    Rescaled_vars = F) {
  
  # ---- Compile output for the time-invariant variables ----
  # Model output coefficients etc.  
  clustered_se_df <- data.frame(Variable_Name = rownames(clustered_se), 
                                Estimate = clustered_se[,1], 
                                SE_estimate = clustered_se[,2],
                                Pval_estimate = clustered_se[,4])
  rownames(clustered_se_df) <- NULL
  # Join with margins information
  margins_output_df <- as.data.frame(summary(margins_output)[c("factor","AME","SE","lower","upper")])
  rownames(margins_output_df) <- NULL
  
  join_model_info <- left_join(clustered_se_df, margins_output_df, by = c("Variable_Name" = "factor"))
  
  # Add descriptive statistics (mean and sd) for each of the variables in the model structure
  join_output <- left_join(join_model_info, descriptive_stats[c("var","mean","sd")], by = c("Variable_Name" = "var"))
  # Calculate SD_change_effect
  join_output$SD_change_effect <- (join_output$AME * join_output$sd) / mean_COV
  # Create tidy version: compile all information above, with meaningful variable names and
  #    information on whether they are counts versus rates, the model type, etc. for plotting later
  time_invar_df <- data.frame (Experience_variable = Experience_var_name, 
                               Count_rate = Count_rate,
                               Deaths_cases = Deaths_cases,
                               Model = Model,
                               With_controls = With_controls,
                               Time_varying = F,
                               Year = NA,
                               Variable_name = join_output$Variable_Name,
                               Estimate = join_output$Estimate,
                               SE_estimate = join_output$SE_estimate,
                               Pval_estimate = join_output$Pval_estimate,
                               Signif_estimate = stars.pval(join_output$Pval_estimate),
                               Marginal_effect = join_output$AME, 
                               SE_marginal_effect = join_output$SE,
                               LCI_marginal_effect = join_output$lower,
                               UCI_marginal_effect = join_output$upper,
                               SD_change_effect = join_output$SD_change_effect)
  
  # ---- Compile output for the time-varying variables ----
  
  # OLD EXPLANATION BELOW
  # For explanation of SD_change_effect, XXX  
  # sd change effects are: % change in the dependent variable associated with a 1sd change in indep var
  
  # static variables: sd change effect of e.g. Average_rooms_1901: 
  #     (marginal effect of Average_rooms_1901 * sd of Average_rooms_1901) / mean COV over all years
  # time-varying: sd change effect of e.g. Smallpox_cases
  #     (marginal effect of Smallpox_cases for that year * sd of Smallpox_cases) / mean COV in that year 
  
  
  margins_output_years_df <- as.data.frame(summary(margins_output_years))
  margins_output_years_df$Year <- 1906 + margins_output_years_df$Year_Numeric
  # Join margins and COV descriptive stats (i.e. by year)
  join_margins_COV <- left_join(margins_output_years_df[c("factor","Year","AME","SE","lower","upper")], 
                                COV_descriptive_stats[c("Year","Mean_COV","sd_COV")], 
                                by = c("Year" = "Year"))
  # Join with descriptive stats for the main variable of interest here (e.g. Smallpox_cases)
  join_margins_COV_desc_stats <- left_join(join_margins_COV, descriptive_stats,
                                           by = c("factor" = "var"))
  # Calculation of SD_change_effect for each year
  join_margins_COV_desc_stats$SD_change_effect <- 
    (join_margins_COV_desc_stats$AME * join_margins_COV_desc_stats$sd) / 
    join_margins_COV_desc_stats$Mean_COV
  # Create tidy version: compile all time-varying information above, with meaningful variable names and
  #    information on whether they are counts versus rates, the model type, etc. for plotting later
  years_df <- data.frame (Experience_variable = Experience_var_name, 
                          Count_rate = Count_rate,
                          Deaths_cases = Deaths_cases,
                          Model = Model,
                          With_controls = With_controls,
                          Time_varying = T,
                          Year = yrs_text,
                          Variable_name = "Year",
                          Estimate = NA,
                          SE_estimate = NA,
                          Pval_estimate = NA,
                          Signif_estimate = NA,
                          Marginal_effect = join_margins_COV_desc_stats$AME,
                          SE_marginal_effect = join_margins_COV_desc_stats$SE,
                          LCI_marginal_effect = join_margins_COV_desc_stats$lower,
                          UCI_marginal_effect = join_margins_COV_desc_stats$upper,
                          SD_change_effect = join_margins_COV_desc_stats$SD_change_effect)
  
  
  dim(years_df)
  
  # ---- Combine output ----
  combined_df <- rbind(time_invar_df, years_df)
  
  return(combined_df)
}
