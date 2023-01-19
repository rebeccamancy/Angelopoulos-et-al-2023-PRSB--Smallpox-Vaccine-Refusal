#######################################################################################
#         Function to compile OLS outputs into a format for easy plotting
#
# In two steps: First, we compile output for time-invariant variables, and then compile
# output for the time-varying variables
#######################################################################################

# y-axis is % change in the dependent variable (conscientious objection to vaccination rate, COV) 
#   associated with a one sd change in the smallpox experience variable

# Steps
# -----
# Step 0. Calculate the sd of the smallpox experience variable (e.g. case rate, death rate)
# Step 1. Calculate the mean value of the dependent variable at each year (from the data)
# Step 2. Calculate the predicted change in the dependent variable when you apply a 
#     one sd change of the smallpox experience variable. This is given by margin * 1sd.
# Step 3. We want the % change in COV predicted if there were to be 
#     a 1sd change in the smallpox experience variable. To obtain this as a %, 
#     we need to divide the answer from Step 2 by answer to Step 1, i.e.
#     (margin * sd) / mean value of the dependent variable in that year
# All this gives us the change in the dependent variable as a share of the 
#     dependent variable (i.e. the % change of the dependent variable)
# Note that the margin and the mean value change each year; the sd does not.

require(gtools) # for stars from pvals

compile_OLS_outputs <- function(Experience_var_name, Count_rate, Deaths_cases, 
                                Model, With_controls, 
                                model_output, margins_output, margins_output_years,
                                descriptive_stats, COV_descriptive_stats, mean_COV) {
  
  # ---- Compile output for the time-invariant variables ----
  # Model output coefficients etc.
  model_output_df <- data.frame(Variable_Name = names(model_output$coefficients),
                                Estimate = model_output$coefficients,
                                SE_Estimate = model_output$std.error,
                                Pval_Estimate = model_output$p.value)
  rownames(model_output_df) <- NULL
  # Join with margins information
  join_model_info <- left_join(model_output_df, as.data.frame(summary(margins_output))[c("factor","AME","SE","lower","upper")], by = c("Variable_Name" = "factor")) 
  # Add descriptive statistics (mean and sd) for each of the variables in the model structure
  join_output <- left_join(join_model_info, descriptive_stats[c("var","mean","sd")], by = c("Variable_Name"="var"))
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
                               SE_estimate = join_output$SE_Estimate,
                               Pval_estimate = join_output$Pval_Estimate,
                               Signif_estimate = stars.pval(join_output$Pval_Estimate),
                               Marginal_effect = join_output$AME, 
                               SE_marginal_effect = join_output$SE,
                               LCI_marginal_effect = join_output$lower,
                               UCI_marginal_effect = join_output$upper,
                               SD_change_effect = join_output$SD_change_effect)
  rownames(time_invar_df) <- NULL
  dim(time_invar_df)
  
  # ---- Compile output for the time-varying variables ----
  
  # For explanation of SD_change_effect, XXX
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
