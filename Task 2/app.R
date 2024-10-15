# TS task
# Author: Markus Hagenbäck

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(DT)
library(scales)  
library(rsconnect)

rm(list=ls())

# Read in the data from Excel
file_url <- "https://raw.githubusercontent.com/MarkusHagenback/VIG-tasks/refs/heads/main/002%20-%20Time%20Series/S0201_ts_dummy.csv"
data <- read.csv(file_url)

# Assuming you've already loaded the dataset as 'data'
data_cleaned <- data %>%
  # Recode the company dummy (X.BIC.S2QRT2) as firm ID
  mutate(Firm_ID = as.numeric(as.factor(X.BIC.S2QRT2))) %>%
  # Rename columns for clarity
  rename(VG = X.BIC.S2ROW,
         Balance_Sheet_Position = FISCVARNT,
         Value = Column1,
         Period = X.BIC.S2COMPANY,
         Solvency_II_template = X.BIC.S2COLUMN) %>%
  # Remove unnecessary columns
  select(Firm_ID, Period, Solvency_II_template, VG, Balance_Sheet_Position, Value) 

# Create a new column for Year (first 4 characters of the Period)
data_cleaned <- data_cleaned %>%
  mutate(Year = substr(Period, 1, 4))

# Create a new column for Quarter or Adjustment (last 3 characters of the Period)
data_cleaned <- data_cleaned %>%
  mutate(NumericPeriod = as.numeric(substr(Period, 5, 7)))

# Filter the data to keep only rows where the second digit of the Year is "0"
data_cleaned <- data_cleaned %>%
  filter(substr(Year, 2, 2) == "0")

# Create a new Time column using Year and NumericPeriod for better time handling
data_cleaned <- data_cleaned %>%
  mutate(
    Year = as.integer(Year),  # Ensure Year is an integer
    NumericPeriod = as.integer(NumericPeriod),  # Ensure NumericPeriod is an integer
    Time = paste(Year, NumericPeriod)  # Create a string to represent the time
  )

# Ensure Time is recognized as a factor
data_cleaned$Time <- factor(data_cleaned$Time, levels = unique(data_cleaned$Time))

# Clean the VG and Balance Sheet Position columns in data_cleaned
data_cleaned$VG <- trimws(data_cleaned$VG)
data_cleaned$Balance_Sheet_Position <- trimws(data_cleaned$Balance_Sheet_Position)

## Making some graphs to understand the data.

# firm_1_data <- data_cleaned %>%
#   filter(Firm_ID == 1)
# time_series_graphs <- unique(firm_1_data$Balance_Sheet_Position)
# 
# if (!dir.exists("test_graphs")) {
#   dir.create("test_graphs")
# }
# 
# for (i in seq_along(time_series_graphs)) {
#   
#   
#   plot_in_making_name <- time_series_graphs[i]
#   print(plot_in_making_name)
#   
#   graph_data <- firm_1_data %>%
#     filter(Balance_Sheet_Position == plot_in_making_name)
#   
#   # Create the plot
#   plot <- ggplot(graph_data, aes(x = Time, y = Value, color = VG, group = VG)) +
#     geom_line() +   # Line plot
#     geom_point() +  # Points for each observation
#     labs(title = paste("Firm:", unique(graph_data$Firm_ID), 
#                        "- Balance Sheet Position:", plot_in_making_name),
#          x = "Time", y = "Value", color = "VG") +  # Labels for axes and legend
#     theme_minimal()  # A clean theme
#   
#   # Construct a unique filename using Firm_ID and Balance_Sheet_Position
#   file_name <- paste0("test_graphs/Firm_", unique(graph_data$Firm_ID), 
#                       "_Position_", plot_in_making_name, ".png")
#   
#   # Save the plot with a unique name
#   ggsave(filename = file_name, plot = plot, width = 10, height = 6)
# }


##

####

# Define the names and codes as vectors
names <- c(
  "Goodwill",
  "Deferred acquisition costs",
  "Intangible assets",
  "Deferred tax assets",
  "Pension benefit surplus",
  "Property, plant & equipment held for own use",
  "Investments (other than assets held for index-linked and unit-linked contracts)",
  "Property (other than for own use)",
  "Holdings in related undertakings, including participations",
  "Equities",
  "Equities - listed",
  "Equities - unlisted",
  "Bonds",
  "Government Bonds",
  "Corporate Bonds",
  "Structured notes",
  "Collateralised securities",
  "Collective Investments Undertakings",
  "Derivatives",
  "Deposits other than cash equivalents",
  "Other investments",
  "Assets held for index-linked and unit-linked contracts",
  "Loans and mortgages",
  "Loans on policies",
  "Loans and mortgages to individuals",
  "Other loans and mortgages",
  "Reinsurance recoverables from:",
  "Non-life and health similar to non-life",
  "Non-life excluding health",
  "Health similar to non-life",
  "Life and health similar to life, excluding health and index-linked and unit-linked",
  "Health similar to life",
  "Life excluding health and index-linked and unit-linked",
  "Life index-linked and unit-linked",
  "Deposits to cedants",
  "Insurance and intermediaries receivables",
  "Reinsurance receivables",
  "Receivables (trade, not insurance)",
  "Own shares (held directly)",
  "Amounts due in respect of own fund items or initial fund called up but not yet paid in",
  "Cash and cash equivalents",
  "Any other assets, not elsewhere shown",
  "Total assets"
)

codes <- c(
  "R0010",
  "R0020",
  "R0030",
  "R0040",
  "R0050",
  "R0060",
  "R0070",
  "R0080",
  "R0090",
  "R0100",
  "R0110",
  "R0120",
  "R0130",
  "R0140",
  "R0150",
  "R0160",
  "R0170",
  "R0180",
  "R0190",
  "R0200",
  "R0210",
  "R0220",
  "R0230",
  "R0240",
  "R0250",
  "R0260",
  "R0270",
  "R0280",
  "R0290",
  "R0300",
  "R0310",
  "R0320",
  "R0330",
  "R0340",
  "R0350",
  "R0360",
  "R0370",
  "R0380",
  "R0390",
  "R0400",
  "R0410",
  "R0420",
  "R0500"
)

# Create a data frame
balance_sheet_df <- data.frame(
  Name = names,
  Code = codes,
  stringsAsFactors = FALSE  # Avoid converting to factors
)

# Create binary columns for each item based on their codes
balance_sheet_df$Total_assets <- ifelse(balance_sheet_df$Code == "R0500", 1, 0)
balance_sheet_df$Goodwill <- ifelse(balance_sheet_df$Code == "R0010", 1, 0)
balance_sheet_df$Deferred_acquisition_costs <- ifelse(balance_sheet_df$Code == "R0020", 1, 0)
balance_sheet_df$Intangible_assets <- ifelse(balance_sheet_df$Code == "R0030", 1, 0)
balance_sheet_df$Deferred_tax_assets <- ifelse(balance_sheet_df$Code == "R0040", 1, 0)
balance_sheet_df$Pension_benefit_surplus <- ifelse(balance_sheet_df$Code == "R0050", 1, 0)
balance_sheet_df$Property_plant_equipment_held_for_own_use <- ifelse(balance_sheet_df$Code == "R0060", 1, 0)
balance_sheet_df$Investments_other_than_assets_held_for_index_linked_and_unit_linked_contracts <- ifelse(balance_sheet_df$Code == "R0070", 1, 0)
balance_sheet_df$Property_other_than_for_own_use <- ifelse(balance_sheet_df$Code == "R0080", 1, 0)
balance_sheet_df$Holdings_in_related_undertakings_including_participations <- ifelse(balance_sheet_df$Code == "R0090", 1, 0)
balance_sheet_df$Equities <- ifelse(balance_sheet_df$Code == "R0100", 1, 0)
balance_sheet_df$Equities_listed <- ifelse(balance_sheet_df$Code == "R0110", 1, 0)
balance_sheet_df$Equities_unlisted <- ifelse(balance_sheet_df$Code == "R0120", 1, 0)
balance_sheet_df$Bonds <- ifelse(balance_sheet_df$Code == "R0130", 1, 0)
balance_sheet_df$Government_Bonds <- ifelse(balance_sheet_df$Code == "R0140", 1, 0)
balance_sheet_df$Corporate_Bonds <- ifelse(balance_sheet_df$Code == "R0150", 1, 0)
balance_sheet_df$Structured_notes <- ifelse(balance_sheet_df$Code == "R0160", 1, 0)
balance_sheet_df$Collateralised_securities <- ifelse(balance_sheet_df$Code == "R0170", 1, 0)
balance_sheet_df$Collective_Investments_Undertakings <- ifelse(balance_sheet_df$Code == "R0180", 1, 0)
balance_sheet_df$Derivatives <- ifelse(balance_sheet_df$Code == "R0190", 1, 0)
balance_sheet_df$Deposits_other_than_cash_equivalents <- ifelse(balance_sheet_df$Code == "R0200", 1, 0)
balance_sheet_df$Other_investments <- ifelse(balance_sheet_df$Code == "R0210", 1, 0)
balance_sheet_df$Assets_held_for_index_linked_and_unit_linked_contracts <- ifelse(balance_sheet_df$Code == "R0220", 1, 0)
balance_sheet_df$Loans_and_mortgages <- ifelse(balance_sheet_df$Code == "R0230", 1, 0)
balance_sheet_df$Loans_on_policies <- ifelse(balance_sheet_df$Code == "R0240", 1, 0)
balance_sheet_df$Loans_and_mortgages_to_individuals <- ifelse(balance_sheet_df$Code == "R0250", 1, 0)
balance_sheet_df$Other_loans_and_mortgages <- ifelse(balance_sheet_df$Code == "R0260", 1, 0)
balance_sheet_df$Reinsurance_recoverables_from <- ifelse(balance_sheet_df$Code == "R0270", 1, 0)
balance_sheet_df$Non_life_and_health_similar_to_non_life <- ifelse(balance_sheet_df$Code == "R0280", 1, 0)
balance_sheet_df$Non_life_excluding_health <- ifelse(balance_sheet_df$Code == "R0290", 1, 0)
balance_sheet_df$Health_similar_to_non_life <- ifelse(balance_sheet_df$Code == "R0300", 1, 0)
balance_sheet_df$Life_and_health_similar_to_life_excluding_health_and_index_linked_and_unit_linked <- ifelse(balance_sheet_df$Code == "R0310", 1, 0)
balance_sheet_df$Health_similar_to_life <- ifelse(balance_sheet_df$Code == "R0320", 1, 0)
balance_sheet_df$Life_excluding_health_and_index_linked_and_unit_linked <- ifelse(balance_sheet_df$Code == "R0330", 1, 0)
balance_sheet_df$Life_index_linked_and_unit_linked <- ifelse(balance_sheet_df$Code == "R0340", 1, 0)
balance_sheet_df$Deposits_to_cedants <- ifelse(balance_sheet_df$Code == "R0350", 1, 0)
balance_sheet_df$Insurance_and_intermediaries_receivables <- ifelse(balance_sheet_df$Code == "R0360", 1, 0)
balance_sheet_df$Reinsurance_receivables <- ifelse(balance_sheet_df$Code == "R0370", 1, 0)
balance_sheet_df$Receivables_trade_not_insurance <- ifelse(balance_sheet_df$Code == "R0380", 1, 0)
balance_sheet_df$Own_shares_held_directly <- ifelse(balance_sheet_df$Code == "R0390", 1, 0)
balance_sheet_df$Amounts_due_in_respect_of_own_fund_items_or_initial_fund_called_up_but_not_yet_paid_in <- ifelse(balance_sheet_df$Code == "R0400", 1, 0)
balance_sheet_df$Cash_and_cash_equivalents <- ifelse(balance_sheet_df$Code == "R0410", 1, 0)
balance_sheet_df$other_assets <- ifelse(balance_sheet_df$Code == "R0420", 1, 0)

# Define the main components
main_components <- c(
  "Goodwill",
  "Deferred acquisition costs",
  "Intangible assets",
  "Deferred tax assets",
  "Pension benefit surplus",
  "Property, plant & equipment held for own use",
  "Investments (other than assets held for index-linked and unit-linked contracts)",
  "Assets held for index-linked and unit-linked contracts",
  "Loans and mortgages",
  "Reinsurance recoverables from:",
  "Deposits to cedants",
  "Insurance and intermediaries receivables",
  "Reinsurance receivables",
  "Receivables (trade, not insurance)",
  "Own shares (held directly)",
  "Amounts due in respect of own fund items or initial fund called up but not yet paid in",
  "Cash and cash equivalents",
  "Any other assets, not elsewhere shown"
)

# Add the Main_or_Sub_Component column
balance_sheet_df$Main_or_Sub_Component <- ifelse(balance_sheet_df$Name %in% main_components, 1, 0)

# Define the investments components
investments_components <- c(
  "Property (other than for own use)",
  "Holdings in related undertakings, including participations",
  "Equities",
  "Bonds",
  "Collective Investments Undertakings",
  "Derivatives",
  "Deposits other than cash equivalents",
  "Other investments"
)

# Add the Investments_Component column
balance_sheet_df$Investments_Component <- ifelse(balance_sheet_df$Name %in% investments_components, 1, 0)

# Define the equities components
equities_components <- c(
  "Equities - listed",
  "Equities - unlisted"
)

# Define the bond components
bond_components <- c(
  "Government Bonds",
  "Corporate Bonds",
  "Structured notes",
  "Collateralised securities"
)

# Define the loan components
loan_components <- c(
  "Loans on policies",
  "Loans and mortgages to individuals",
  "Other loans and mortgages"
)

# Add the Equities_Component column
balance_sheet_df$Equities_Component <- ifelse(balance_sheet_df$Name %in% equities_components, 1, 0)

# Add the Bond_Component column
balance_sheet_df$Bond_Component <- ifelse(balance_sheet_df$Name %in% bond_components, 1, 0)

# Add the Loan_Component column
balance_sheet_df$Loan_Component <- ifelse(balance_sheet_df$Name %in% loan_components, 1, 0)

# Define the reinsurance recoverables components
reinsurance_recoverables <- c(
  "Non-life and health similar to non-life",
  "Life and health similar to life, excluding health and index-linked and unit-linked",
  "Life index-linked and unit-linked"
)

# Define the nonlife components
nonlife_components <- c(
  "Non-life excluding health",
  "Health similar to non-life"
)

# Define the life components
life_components <- c(
  "Health similar to life",
  "Life excluding health and index-linked and unit-linked"
)

# Add the Reinsurance_Recoverables column
balance_sheet_df$Reinsurance_Recoverables <- ifelse(balance_sheet_df$Name %in% reinsurance_recoverables, 1, 0)

# Add the Nonlife column
balance_sheet_df$Nonlife <- ifelse(balance_sheet_df$Name %in% nonlife_components, 1, 0)

# Add the Life column
balance_sheet_df$Life <- ifelse(balance_sheet_df$Name %in% life_components, 1, 0)

##### Adding this classificaiton to the dataframe
colnames(balance_sheet_df)[colnames(balance_sheet_df) == "Code"] <- "Balance_Sheet_Position"

# match balance_sheet_df with data_cleaned based on code
data_cleaned <- left_join(data_cleaned, balance_sheet_df, by = "Balance_Sheet_Position")

###### Now the same for liabilities: ##########

# Define the new names and codes
names <- c(
  "Total liabilities",
  "Technical provisions – non-life",
  "Technical provisions – non-life (excluding health)",
  "Technical provisions calculated as a whole",
  "Best Estimate",
  "Risk margin",
  "Technical provisions - health (similar to non-life)",
  "Technical provisions calculated as a whole",
  "Best Estimate",
  "Risk margin",
  "Technical provisions - life (excluding index-linked and unit-linked)",
  "Technical provisions - health (similar to life)",
  "Technical provisions calculated as a whole",
  "Best Estimate",
  "Risk margin",
  "Technical provisions – life (excluding health and index-linked and unit-linked)",
  "Technical provisions calculated as a whole",
  "Best Estimate",
  "Risk margin",
  "Technical provisions – index-linked and unit-linked",
  "Technical provisions calculated as a whole",
  "Best Estimate",
  "Risk margin",
  "Other technical provisions",
  "Contingent liabilities",
  "Provisions other than technical provisions",
  "Pension benefit obligations",
  "Deposits from reinsurers",
  "Deferred tax liabilities",
  "Derivatives_l",
  "Debts owed to credit institutions",
  "Debts owed to credit institutions resident domestically",
  "Debts owed to credit institutions resident in the euro area other than domestic",
  "Debts owed to credit institutions resident in rest of the world",
  "Financial liabilities other than debts owed to credit institutions",
  "Debts owed to non-credit institutions",
  "Debts owed to non-credit institutions resident domestically",
  "Debts owed to non-credit institutions resident in the euro area other than domestic",
  "Debts owed to non-credit institutions resident in rest of the world",
  "Other financial liabilities (debt securities issued)",
  "Insurance & intermediaries payables",
  "Reinsurance payables",
  "Payables (trade, not insurance)",
  "Subordinated liabilities",
  "Subordinated liabilities not in Basic Own Funds",
  "Subordinated liabilities in Basic Own Funds",
  "Any other liabilities, not elsewhere shown",
  "Excess of assets over liabilities"
)

codes <- c(
  "R0900",
  "R0510",
  "R0520",
  "R0530",
  "R0540",
  "R0550",
  "R0560",
  "R0570",
  "R0580",
  "R0590",
  "R0600",
  "R0610",
  "R0620",
  "R0630",
  "R0640",
  "R0650",
  "R0660",
  "R0670",
  "R0680",
  "R0690",
  "R0700",
  "R0710",
  "R0720",
  "R0730",
  "R0740",
  "R0750",
  "R0760",
  "R0770",
  "R0780",
  "R0790",
  "R0800",
  "ER0801",
  "ER0802",
  "ER0803",
  "R0810",
  "ER0811",
  "ER0812",
  "ER0813",
  "ER0814",
  "ER0815",
  "R0820",
  "R0830",
  "R0840",
  "R0850",
  "R0860",
  "R0870",
  "R0880",
  "R1000"
)

# Create a data frame
balance_sheet_df_liabilities <- data.frame(
  Name = names,
  Code = codes,
  stringsAsFactors = FALSE  # Avoid converting to factors
)

# Create binary columns for each item based on their codes
balance_sheet_df_liabilities$Total_liabilities <- ifelse(balance_sheet_df_liabilities$Code == "R0900", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_non_life <- ifelse(balance_sheet_df_liabilities$Code == "R0510", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_non_life_excluding_health <- ifelse(balance_sheet_df_liabilities$Code == "R0520", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_calculated_as_a_whole_1 <- ifelse(balance_sheet_df_liabilities$Code == "R0530", 1, 0)
balance_sheet_df_liabilities$Best_Estimate_1 <- ifelse(balance_sheet_df_liabilities$Code == "R0540", 1, 0)
balance_sheet_df_liabilities$Risk_margin_1 <- ifelse(balance_sheet_df_liabilities$Code == "R0550", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_health_similar_to_non_life <- ifelse(balance_sheet_df_liabilities$Code == "R0560", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_calculated_as_a_whole_2 <- ifelse(balance_sheet_df_liabilities$Code == "R0570", 1, 0)
balance_sheet_df_liabilities$Best_Estimate_2 <- ifelse(balance_sheet_df_liabilities$Code == "R0580", 1, 0)
balance_sheet_df_liabilities$Risk_margin_2 <- ifelse(balance_sheet_df_liabilities$Code == "R0590", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_life_excluding_index_linked_and_unit_linked <- ifelse(balance_sheet_df_liabilities$Code == "R0600", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_health_similar_to_life <- ifelse(balance_sheet_df_liabilities$Code == "R0610", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_calculated_as_a_whole_3 <- ifelse(balance_sheet_df_liabilities$Code == "R0620", 1, 0)
balance_sheet_df_liabilities$Best_Estimate_3 <- ifelse(balance_sheet_df_liabilities$Code == "R0630", 1, 0)
balance_sheet_df_liabilities$Risk_margin_3 <- ifelse(balance_sheet_df_liabilities$Code == "R0640", 1, 0)

balance_sheet_df_liabilities$Technical_provisions_life_excluding_health_and_index_linked_and_unit_linked <- ifelse(balance_sheet_df_liabilities$Code == "R0650", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_calculated_as_a_whole_4 <- ifelse(balance_sheet_df_liabilities$Code == "R0660", 1, 0)
balance_sheet_df_liabilities$Best_Estimate_4 <- ifelse(balance_sheet_df_liabilities$Code == "R0670", 1, 0)
balance_sheet_df_liabilities$Risk_margin_4 <- ifelse(balance_sheet_df_liabilities$Code == "R0680", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_index_linked_and_unit_linked <- ifelse(balance_sheet_df_liabilities$Code == "R0690", 1, 0)
balance_sheet_df_liabilities$Technical_provisions_calculated_as_a_whole_5 <- ifelse(balance_sheet_df_liabilities$Code == "R0700", 1, 0)
balance_sheet_df_liabilities$Best_Estimate_5 <- ifelse(balance_sheet_df_liabilities$Code == "R0710", 1, 0)
balance_sheet_df_liabilities$Risk_margin_5 <- ifelse(balance_sheet_df_liabilities$Code == "R0720", 1, 0)
balance_sheet_df_liabilities$Other_technical_provisions <- ifelse(balance_sheet_df_liabilities$Code == "R0730", 1, 0)
balance_sheet_df_liabilities$Contingent_liabilities <- ifelse(balance_sheet_df_liabilities$Code == "R0740", 1, 0)
balance_sheet_df_liabilities$Provisions_other_than_technical_provisions <- ifelse(balance_sheet_df_liabilities$Code == "R0750", 1, 0)
balance_sheet_df_liabilities$Pension_benefit_obligations <- ifelse(balance_sheet_df_liabilities$Code == "R0760", 1, 0)
balance_sheet_df_liabilities$Deposits_from_reinsurers <- ifelse(balance_sheet_df_liabilities$Code == "R0770", 1, 0)
balance_sheet_df_liabilities$Deferred_tax_liabilities <- ifelse(balance_sheet_df_liabilities$Code == "R0780", 1, 0)
balance_sheet_df_liabilities$Derivatives_l <- ifelse(balance_sheet_df_liabilities$Code == "R0790", 1, 0)

balance_sheet_df_liabilities$Debts_owed_to_credit_institutions <- ifelse(balance_sheet_df_liabilities$Code == "R0800", 1, 0)
balance_sheet_df_liabilities$Debts_owed_to_credit_institutions_resident_domestically <- ifelse(balance_sheet_df_liabilities$Code == "ER0801", 1, 0)
balance_sheet_df_liabilities$Debts_owed_to_credit_institutions_resident_in_the_euro_area_other_than_domestic <- ifelse(balance_sheet_df_liabilities$Code == "ER0802", 1, 0)
balance_sheet_df_liabilities$Debts_owed_to_credit_institutions_resident_in_rest_of_the_world <- ifelse(balance_sheet_df_liabilities$Code == "ER0803", 1, 0)
balance_sheet_df_liabilities$Financial_liabilities_other_than_debts_owed_to_credit_institutions <- ifelse(balance_sheet_df_liabilities$Code == "R0810", 1, 0)
balance_sheet_df_liabilities$Debts_owed_to_non_credit_institutions <- ifelse(balance_sheet_df_liabilities$Code == "ER0811", 1, 0)
balance_sheet_df_liabilities$Debts_owed_to_non_credit_institutions_resident_domestically <- ifelse(balance_sheet_df_liabilities$Code == "ER0812", 1, 0)
balance_sheet_df_liabilities$Debts_owed_to_non_credit_institutions_resident_in_the_euro_area_other_than_domestic <- ifelse(balance_sheet_df_liabilities$Code == "ER0813", 1, 0)
balance_sheet_df_liabilities$Debts_owed_to_non_credit_institutions_resident_in_rest_of_the_world <- ifelse(balance_sheet_df_liabilities$Code == "ER0814", 1, 0)
balance_sheet_df_liabilities$Other_financial_liabilities_debt_securities_issued <- ifelse(balance_sheet_df_liabilities$Code == "ER0815", 1, 0)
balance_sheet_df_liabilities$Insurance_intermediaries_payables <- ifelse(balance_sheet_df_liabilities$Code == "R0820", 1, 0)
balance_sheet_df_liabilities$Reinsurance_payables <- ifelse(balance_sheet_df_liabilities$Code == "R0830", 1, 0)
balance_sheet_df_liabilities$Payables_trade_not_insurance <- ifelse(balance_sheet_df_liabilities$Code == "R0840", 1, 0)
balance_sheet_df_liabilities$Subordinated_liabilities <- ifelse(balance_sheet_df_liabilities$Code == "R0850", 1, 0)
balance_sheet_df_liabilities$Subordinated_liabilities_not_in_Basic_Own_Funds <- ifelse(balance_sheet_df_liabilities$Code == "R0860", 1, 0)
balance_sheet_df_liabilities$Subordinated_liabilities_in_Basic_Own_Funds <- ifelse(balance_sheet_df_liabilities$Code == "R0870", 1, 0)
balance_sheet_df_liabilities$Any_other_liabilities_not_elsewhere_shown <- ifelse(balance_sheet_df_liabilities$Code == "R0880", 1, 0)
balance_sheet_df_liabilities$Excess_of_assets_over_liabilities <- ifelse(balance_sheet_df_liabilities$Code == "R1000", 1, 0)

##### Adding this classificaiton to the dataframe
colnames(balance_sheet_df_liabilities)[colnames(balance_sheet_df_liabilities) == "Code"] <- "Balance_Sheet_Position"

# match balance_sheet_df with data_cleaned based on code
data_cleaned <- left_join(data_cleaned, balance_sheet_df_liabilities, by = "Balance_Sheet_Position")

###################################################################
### Still, there is some balance sheet items not covered
unique_balance_sheet_all_items <- unique(data_cleaned$Balance_Sheet_Position)

# Removing those we have covered
unique_liabilities_codes <- unique(balance_sheet_df_liabilities$Balance_Sheet_Position)
unique_assets_codes <- unique(balance_sheet_df$Balance_Sheet_Position)

# Combine liabilities and assets codes into one vector
codes_to_filter_out <- unique(c(unique_liabilities_codes, unique_assets_codes))

# Remove the codes that exist in unique_liabilities_codes and unique_assets_codes
filtered_balance_sheet_items <- setdiff(unique_balance_sheet_all_items, codes_to_filter_out)

## Codes we dont know about 
unknown_balance_sheet_df <- data.frame(Balance_Sheet_Position = filtered_balance_sheet_items)

# Creating binary columns for each balance sheet position
unknown_balance_sheet_df$GAR0010 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0010", 1, 0)
unknown_balance_sheet_df$GAR0020 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0020", 1, 0)
unknown_balance_sheet_df$GAR0030 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0030", 1, 0)
unknown_balance_sheet_df$GAR0040 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0040", 1, 0)
unknown_balance_sheet_df$GAR0050 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0050", 1, 0)
unknown_balance_sheet_df$GAR0080 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0080", 1, 0)
unknown_balance_sheet_df$GAR0100 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0100", 1, 0)
unknown_balance_sheet_df$GAR0120 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0120", 1, 0)
unknown_balance_sheet_df$GAR0140 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0140", 1, 0)
unknown_balance_sheet_df$GAR0160 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0160", 1, 0)
unknown_balance_sheet_df$GAR0110 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0110", 1, 0)
unknown_balance_sheet_df$GAR0130 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0130", 1, 0)
unknown_balance_sheet_df$GAR0150 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0150", 1, 0)
unknown_balance_sheet_df$GAR0170 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0170", 1, 0)
unknown_balance_sheet_df$GAR0090 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0090", 1, 0)
unknown_balance_sheet_df$GAR0060 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0060", 1, 0)
unknown_balance_sheet_df$GAR0070 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0070", 1, 0)
unknown_balance_sheet_df$GAR0042 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0042", 1, 0)
unknown_balance_sheet_df$GAR0041 <- ifelse(unknown_balance_sheet_df$Balance_Sheet_Position == "GAR0041", 1, 0)

data_cleaned <- left_join(data_cleaned, unknown_balance_sheet_df, by = "Balance_Sheet_Position")

# Simple renaming:
data_cleaned <- data_cleaned %>%
  rename(
    `Asset Name` = Name.x,
    `Liability Name` = Name.y
  )

# Assuming your dataframe is called data_cleaned
data_cleaned <- data_cleaned %>%
  mutate(VG = case_when(
    VG == "C0010" ~ "C0010 (Solvency II value)",
    VG == "C0020" ~ "C0020 (Statutory accounts value)",
    VG == "EC0021" ~ "EC0021 (Reclassification adjustments)",
    TRUE ~ VG  # Keep original value if no change is needed
  ))

################################################################################

# UI
ui <- fluidPage(
  titlePanel("Task 2 Dashboard"),
  
  # Wrap tabsetPanel in a div with specified width
  tags$div(style = "width: 2500px; margin: auto;",  # Set the desired width here
           tabsetPanel(
             # First Tab: Filters
             tabPanel("Filters", 
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("firm_filter", "Filter firm:", 
                                       choices = unique(data_cleaned$Firm_ID),
                                       selected = 1),
                          
                          # Use the numeric range of Period for filtering without changing its format
                          sliderInput("date_filter", "Filter time period:", 
                                      min = min(data_cleaned$Period), 
                                      max = max(data_cleaned$Period),
                                      value = c(min(data_cleaned$Period), max(data_cleaned$Period)),
                                      step = 100),  # Adjust the step to suit the granularity of your Periods
                          
                          checkboxGroupInput("template_filter", "Filter Solvency II template:", 
                                             choices = unique(data_cleaned$Solvency_II_template),
                                             selected = unique(data_cleaned$Solvency_II_template)),
                          
                          checkboxGroupInput("vg_filter", "Filter VG:", 
                                             choices = unique(data_cleaned$VG),
                                             selected = unique(data_cleaned$VG)),
                          
                          # Checkbox inputs for each option
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "total_assets"), 
                                   tags$label(`for` = "total_assets", "Total assets (R0500)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "goodwill"), 
                                   tags$label(`for` = "goodwill", "Goodwill (R0010)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "deferred_acquisition"), 
                                   tags$label(`for` = "deferred_acquisition", "Deferred acquisition costs (R0020)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "intangible_assets"), 
                                   tags$label(`for` = "intangible_assets", "Intangible assets (R0030)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "deferred_tax"), 
                                   tags$label(`for` = "deferred_tax", "Deferred tax assets (R0040)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "pension_surplus"), 
                                   tags$label(`for` = "pension_surplus", "Pension benefit surplus (R0050)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "property_owned"), 
                                   tags$label(`for` = "property_owned", "Property, plant & equipment held for own use (R0060)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "investments"), 
                                   tags$label(`for` = "investments", "Investments (other than assets held for index-linked and unit-linked contracts) (R0070)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "property_other"), 
                                   tags$label(`for` = "property_other", "Property (other than for own use) (R0080)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "holdings"), 
                                   tags$label(`for` = "holdings", "Holdings in related undertakings, including participations (R0090)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "equities"), 
                                   tags$label(`for` = "equities", "Equities (R0100)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "equities_listed"), 
                                   tags$label(`for` = "equities_listed", "Equities - listed (R0110)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "equities_unlisted"), 
                                   tags$label(`for` = "equities_unlisted", "Equities - unlisted (R0120)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "bonds"), 
                                   tags$label(`for` = "bonds", "Bonds (R0130)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "government_bonds"), 
                                   tags$label(`for` = "government_bonds", "Government Bonds (R0140)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "corporate_bonds"), 
                                   tags$label(`for` = "corporate_bonds", "Corporate Bonds (R0150)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "structured_notes"), 
                                   tags$label(`for` = "structured_notes", "Structured notes (R0160)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "collateralised"), 
                                   tags$label(`for` = "collateralised", "Collateralised securities (R0170)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "collective_investments"), 
                                   tags$label(`for` = "collective_investments", "Collective Investments Undertakings (R0180)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "derivatives"), 
                                   tags$label(`for` = "derivatives", "Derivatives (R0190)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "deposits"), 
                                   tags$label(`for` = "deposits", "Deposits other than cash equivalents (R0200)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "other_investments"), 
                                   tags$label(`for` = "other_investments", "Other investments (R0210)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "assets_index_linked"), 
                                   tags$label(`for` = "assets_index_linked", "Assets held for index-linked and unit-linked contracts (R0220)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "loans_mortgages"), 
                                   tags$label(`for` = "loans_mortgages", "Loans and mortgages (R0230)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "loans_policies"), 
                                   tags$label(`for` = "loans_policies", "Loans on policies (R0240)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "loans_individuals"), 
                                   tags$label(`for` = "loans_individuals", "Loans and mortgages to individuals (R0250)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "other_loans"), 
                                   tags$label(`for` = "other_loans", "Other loans and mortgages (R0260)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "reinsurance_recoverables"), 
                                   tags$label(`for` = "reinsurance_recoverables", "Reinsurance recoverables from: (R0270)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "non_life_health"), 
                                   tags$label(`for` = "non_life_health", "Non-life and health similar to non-life (R0280)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "non_life_excluding_health"), 
                                   tags$label(`for` = "non_life_excluding_health", "Non-life excluding health (R0290)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "health_similar_non_life"), 
                                   tags$label(`for` = "health_similar_non_life", "Health similar to non-life (R0300)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "life_health"), 
                                   tags$label(`for` = "life_health", "Life and health similar to life, excluding health and index-linked and unit-linked (R0310)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "health_similar_life"), 
                                   tags$label(`for` = "health_similar_life", "Health similar to life (R0320)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "life_excluding_health"), 
                                   tags$label(`for` = "life_excluding_health", "Life excluding health and index-linked and unit-linked (R0330)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "life_index_linked"), 
                                   tags$label(`for` = "life_index_linked", "Life index-linked and unit-linked (R0340)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "deposits_cedants"), 
                                   tags$label(`for` = "deposits_cedants", "Deposits to cedants (R0350)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "receivables"), 
                                   tags$label(`for` = "receivables", "Insurance and intermediaries receivables (R0360)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "reinsurance_receivables"), 
                                   tags$label(`for` = "reinsurance_receivables", "Reinsurance receivables (R0370)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "receivables_trade"), 
                                   tags$label(`for` = "receivables_trade", "Receivables (trade, not insurance) (R0380)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "own_shares"), 
                                   tags$label(`for` = "own_shares", "Own shares (held directly) (R0390)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "amounts_due"), 
                                   tags$label(`for` = "amounts_due", "Amounts due in respect of own fund items or initial fund called up but not yet paid in (R0400)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "cash_equivalents"), 
                                   tags$label(`for` = "cash_equivalents", "Cash and cash equivalents (R0410)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "other_assets"), 
                                   tags$label(`for` = "other_assets", "Any other assets, not elsewhere shown (R0420)")),
                          

                          ################### Liabilities below
                          # Generate a checkbox for each liability column
                          
                                     tags$div(class = "checkbox-label", 
                                              tags$input(type = "checkbox", id = "total_liabilities"), 
                                              tags$label(`for` = "total_liabilities", "Total liabilities (R0900)")),
                                     tags$div(class = "checkbox-label indent-1", 
                                              tags$input(type = "checkbox", id = "technical_provisions_non_life"), 
                                              tags$label(`for` = "technical_provisions_non_life", "Technical provisions – non-life (R0510)")),
                                     tags$div(class = "checkbox-label indent-2", 
                                              tags$input(type = "checkbox", id = "technical_provisions_non_life_excluding_health"), 
                                              tags$label(`for` = "technical_provisions_non_life_excluding_health", "Technical provisions – non-life (excluding health) (R0520)")),
                                     tags$div(class = "checkbox-label indent-3", 
                                              tags$input(type = "checkbox", id = "technical_provisions_calculated_as_a_whole_1"), 
                                              tags$label(`for` = "technical_provisions_calculated_as_a_whole_1", "Technical provisions calculated as a whole (R0530)")),
                                     tags$div(class = "checkbox-label indent-3", 
                                              tags$input(type = "checkbox", id = "best_estimate_1"), 
                                              tags$label(`for` = "best_estimate_1", "Best Estimate (R0540)")),
                                     tags$div(class = "checkbox-label indent-3", 
                                              tags$input(type = "checkbox", id = "risk_margin_1"), 
                                              tags$label(`for` = "risk_margin_1", "Risk margin (R0550)")),
                                     tags$div(class = "checkbox-label indent-2", 
                                              tags$input(type = "checkbox", id = "technical_provisions_health_similar_to_non_life"), 
                                              tags$label(`for` = "technical_provisions_health_similar_to_non_life", "Technical provisions - health (similar to non-life) (R0560)")),
                                     tags$div(class = "checkbox-label indent-3", 
                                              tags$input(type = "checkbox", id = "technical_provisions_calculated_as_a_whole_2"), 
                                              tags$label(`for` = "technical_provisions_calculated_as_a_whole_2", "Technical provisions calculated as a whole (R0570)")),
                                     tags$div(class = "checkbox-label indent-3", 
                                              tags$input(type = "checkbox", id = "best_estimate_2"), 
                                              tags$label(`for` = "best_estimate_2", "Best Estimate (R0580)")),
                                     tags$div(class = "checkbox-label indent-3", 
                                              tags$input(type = "checkbox", id = "risk_margin_2"), 
                                              tags$label(`for` = "risk_margin_2", "Risk margin (R0590)")),
                                     tags$div(class = "checkbox-label indent-1", 
                                              tags$input(type = "checkbox", id = "technical_provisions_life_excluding_index_linked_and_unit_linked"), 
                                              tags$label(`for` = "technical_provisions_life_excluding_index_linked_and_unit_linked", "Technical provisions – life (excluding index-linked and unit-linked) (R0600)")),
                                     tags$div(class = "checkbox-label indent-2", 
                                              tags$input(type = "checkbox", id = "technical_provisions_health_similar_to_life"), 
                                              tags$label(`for` = "technical_provisions_health_similar_to_life", "Technical provisions - health (similar to life) (R0610)")),
                                     tags$div(class = "checkbox-label indent-3", 
                                              tags$input(type = "checkbox", id = "technical_provisions_calculated_as_a_whole_3"), 
                                              tags$label(`for` = "technical_provisions_calculated_as_a_whole_3", "Technical provisions calculated as a whole (R0620)")),
                                     tags$div(class = "checkbox-label indent-3", 
                                              tags$input(type = "checkbox", id = "best_estimate_3"), 
                                              tags$label(`for` = "best_estimate_3", "Best Estimate (R0630)")),
                                     tags$div(class = "checkbox-label indent-3", 
                                              tags$input(type = "checkbox", id = "risk_margin_3"), 
                                              tags$label(`for` = "risk_margin_3", "Risk margin (R0640)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "technical_provisions_life_excluding_health_and_index_linked_and_unit_linked"), 
                                   tags$label(`for` = "technical_provisions_life_excluding_health_and_index_linked_and_unit_linked", "Technical provisions – life (excluding health and index-linked and unit-linked) (R0650)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "technical_provisions_calculated_as_a_whole_4"), 
                                   tags$label(`for` = "technical_provisions_calculated_as_a_whole_4", "Technical provisions calculated as a whole (R0660)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "best_estimate_4"), 
                                   tags$label(`for` = "best_estimate_4", "Best Estimate (R0670)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "risk_margin_4"), 
                                   tags$label(`for` = "risk_margin_4", "Risk margin (R0680)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "technical_provisions_index_linked_and_unit_linked"), 
                                   tags$label(`for` = "technical_provisions_index_linked_and_unit_linked", "Technical provisions – index-linked and unit-linked (R0690)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "technical_provisions_calculated_as_a_whole_5"), 
                                   tags$label(`for` = "technical_provisions_calculated_as_a_whole_5", "Technical provisions calculated as a whole (R0700)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "best_estimate_5"), 
                                   tags$label(`for` = "best_estimate_5", "Best Estimate (R0710)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "risk_margin_5"), 
                                   tags$label(`for` = "risk_margin_5", "Risk margin (R0720)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "other_technical_provisions"), 
                                   tags$label(`for` = "other_technical_provisions", "Other technical provisions (R0730)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "contingent_liabilities"), 
                                   tags$label(`for` = "contingent_liabilities", "Contingent liabilities (R0740)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "provisions_other_than_technical_provisions"), 
                                   tags$label(`for` = "provisions_other_than_technical_provisions", "Provisions other than technical provisions (R0750)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "pension_benefit_obligations"), 
                                   tags$label(`for` = "pension_benefit_obligations", "Pension benefit obligations (R0760)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "deposits_from_reinsurers"), 
                                   tags$label(`for` = "deposits_from_reinsurers", "Deposits from reinsurers (R0770)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "deferred_tax_liabilities"), 
                                   tags$label(`for` = "deferred_tax_liabilities", "Deferred tax liabilities (R0780)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "derivatives_l"), 
                                   tags$label(`for` = "derivatives_l", "Derivatives (R0790)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "debts_owed_to_credit_institutions"), 
                                   tags$label(`for` = "debts_owed_to_credit_institutions", "Debts owed to credit institutions (R0800)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "debts_owed_to_credit_institutions_resident_domestically"), 
                                   tags$label(`for` = "debts_owed_to_credit_institutions_resident_domestically", "Debts owed to credit institutions – resident domestically (ER0801)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "debts_owed_to_credit_institutions_resident_in_the_euro_area_other_than_domestic"), 
                                   tags$label(`for` = "debts_owed_to_credit_institutions_resident_in_the_euro_area_other_than_domestic", "Debts owed to credit institutions – resident in the euro area (ER0802)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "debts_owed_to_credit_institutions_resident_in_rest_of_the_world"), 
                                   tags$label(`for` = "debts_owed_to_credit_institutions_resident_in_rest_of_the_world", "Debts owed to credit institutions – resident in rest of the world (ER0803)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "financial_liabilities_other_than_debts_owed_to_credit_institutions"), 
                                   tags$label(`for` = "financial_liabilities_other_than_debts_owed_to_credit_institutions", "Financial liabilities other than debts owed to credit institutions (R0810)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "debts_owed_to_non_credit_institutions"), 
                                   tags$label(`for` = "debts_owed_to_non_credit_institutions", "Debts owed to non-credit institutions (ER0811)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "debts_owed_to_non_credit_institutions_resident_domestically"), 
                                   tags$label(`for` = "debts_owed_to_non_credit_institutions_resident_domestically", "Debts owed to non-credit institutions – resident domestically (ER0812)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "debts_owed_to_non_credit_institutions_resident_in_the_euro_area_other_than_domestic"), 
                                   tags$label(`for` = "debts_owed_to_non_credit_institutions_resident_in_the_euro_area_other_than_domestic", "Debts owed to non-credit institutions – resident in the euro area (ER0813)")),
                          tags$div(class = "checkbox-label indent-3", 
                                   tags$input(type = "checkbox", id = "debts_owed_to_non_credit_institutions_resident_in_rest_of_the_world"), 
                                   tags$label(`for` = "debts_owed_to_non_credit_institutions_resident_in_rest_of_the_world", "Debts owed to non-credit institutions – resident in rest of the world (ER0814)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "other_financial_liabilities_debt_securities_issued"), 
                                   tags$label(`for` = "other_financial_liabilities_debt_securities_issued", "Other financial liabilities – debt securities issued (ER0815)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "insurance_intermediaries_payables"), 
                                   tags$label(`for` = "insurance_intermediaries_payables", "Insurance intermediaries payables (R0820)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "reinsurance_payables"), 
                                   tags$label(`for` = "reinsurance_payables", "Reinsurance payables (R0830)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "payables_trade_not_insurance"), 
                                   tags$label(`for` = "payables_trade_not_insurance", "Payables – trade not insurance (R0840)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "subordinated_liabilities"), 
                                   tags$label(`for` = "subordinated_liabilities", "Subordinated liabilities (R0850)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "subordinated_liabilities_not_in_Basic_Own_Funds"), 
                                   tags$label(`for` = "subordinated_liabilities_not_in_Basic_Own_Funds", "Subordinated liabilities not in Basic Own Funds (R0860)")),
                          tags$div(class = "checkbox-label indent-2", 
                                   tags$input(type = "checkbox", id = "subordinated_liabilities_in_Basic_Own_Funds"), 
                                   tags$label(`for` = "subordinated_liabilities_in_Basic_Own_Funds", "Subordinated liabilities in Basic Own Funds (R0870)")),
                          tags$div(class = "checkbox-label indent-1", 
                                   tags$input(type = "checkbox", id = "any_other_liabilities_not_elsewhere_shown"), 
                                   tags$label(`for` = "any_other_liabilities_not_elsewhere_shown", "Any other liabilities not elsewhere shown (R0880)")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "excess_of_assets_over_liabilities"), 
                                   tags$label(`for` = "excess_of_assets_over_liabilities", "Excess of assets over liabilities (R1000)")),
           
                          ### Buttons for the ones we dont know what they are:
                          
                          # Define the list of codes
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0010"), 
                                   tags$label(`for` = "GAR0010", "GAR0010")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0020"), 
                                   tags$label(`for` = "GAR0020", "GAR0020")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0030"), 
                                   tags$label(`for` = "GAR0030", "GAR0030")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0040"), 
                                   tags$label(`for` = "GAR0040", "GAR0040")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0050"), 
                                   tags$label(`for` = "GAR0050", "GAR0050")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0080"), 
                                   tags$label(`for` = "GAR0080", "GAR0080")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0100"), 
                                   tags$label(`for` = "GAR0100", "GAR0100")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0120"), 
                                   tags$label(`for` = "GAR0120", "GAR0120")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0140"), 
                                   tags$label(`for` = "GAR0140", "GAR0140")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0160"), 
                                   tags$label(`for` = "GAR0160", "GAR0160")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0110"), 
                                   tags$label(`for` = "GAR0110", "GAR0110")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0130"), 
                                   tags$label(`for` = "GAR0130", "GAR0130")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0150"), 
                                   tags$label(`for` = "GAR0150", "GAR0150")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0170"), 
                                   tags$label(`for` = "GAR0170", "GAR0170")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0090"), 
                                   tags$label(`for` = "GAR0090", "GAR0090")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0060"), 
                                   tags$label(`for` = "GAR0060", "GAR0060")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0070"), 
                                   tags$label(`for` = "GAR0070", "GAR0070")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0042"), 
                                   tags$label(`for` = "GAR0042", "GAR0042")),
                          tags$div(class = "checkbox-label", 
                                   tags$input(type = "checkbox", id = "GAR0041"), 
                                   tags$label(`for` = "GAR0041", "GAR0041")),
                          
                          ###################
                          
                          # Custom CSS for indentation
                          tags$style(HTML("
                            .indent-1 {
                              margin-left: 20px; /* Indentation for certain items */
                            }
                            .indent-2 {
                              margin-left: 40px; /* Indentation for other items */
                            }
                            .indent-3 {
                              margin-left: 60px; /* Indentation for other items */
                            }
                            .checkbox-label {
                              margin-bottom: 5px; /* Optional: Adds a bit of vertical spacing */
                            }
                          ")),
                          
                          # Balance Sheet Position will be dynamically updated based on selected VG
                          # checkboxGroupInput("balance_sheet_filter", "Filter Balance Sheet Position:", 
                          #                    choices = unique(data_cleaned$Balance_Sheet_Position),
                          #                    selected = unique(data_cleaned$Balance_Sheet_Position))
                        ),
                        

                        mainPanel(
                          uiOutput("filtered_summary")  # Use uiOutput for the summary
                        )
                        
                        
                      )),
          
             ###################################################################
                
             # Second Tab: Data Table
             tabPanel("Data", 
                      DTOutput("filtered_data_table")),
             
             # Third Tab: Graphs (autogenerate depending on the dynamic input)
             tabPanel("Graphs",
                      uiOutput("plot_ui")
                      )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to filter the data
  filtered_data <- reactive({
    req(input$firm_filter, input$date_filter, input$template_filter, input$vg_filter)
    
    # Start with the original data
    df <- data_cleaned %>%
      filter(Firm_ID %in% input$firm_filter,
             Period >= input$date_filter[1],  
             Period <= input$date_filter[2],
             Solvency_II_template %in% input$template_filter,
             VG %in% input$vg_filter)
             # ,Balance_Sheet_Position %in% input$balance_sheet_filter)
    
    # Check if base dataframe has data
    if (nrow(df) == 0) {
      return(df)  # Early exit if no data after base filters
    }
    
    # Initialize a vector to store filter conditions
    filter_conditions <- list()
    
    # Check each checkbox and construct the filter conditions if columns exist
    if (input$total_assets && "Total_assets" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Total_assets == 1")
    }
    if (input$goodwill && "Goodwill" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Goodwill == 1")
    }
    if (input$deferred_acquisition && "Deferred_acquisition" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Deferred_acquisition == 1")
    }
    if (input$intangible_assets && "Intangible_assets" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Intangible_assets == 1")
    }
    if (input$deferred_tax && "Deferred_tax" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Deferred_tax == 1")
    }
    if (input$pension_surplus && "Pension_surplus" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Pension_surplus == 1")
    }
    if (input$property_owned && "Property_owned" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Property_owned == 1")
    }
    if (input$investments && "Investments" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Investments == 1")
    }
    if (input$property_other && "Property_other" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Property_other == 1")
    }
    if (input$holdings && "Holdings" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Holdings == 1")
    }
    if (input$equities && "Equities" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Equities == 1")
    }
    if (input$equities_listed && "Equities_listed" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Equities_listed == 1")
    }
    if (input$equities_unlisted && "Equities_unlisted" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Equities_unlisted == 1")
    }
    if (input$bonds && "Bonds" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Bonds == 1")
    }
    if (input$government_bonds && "Government_bonds" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Government_bonds == 1")
    }
    if (input$corporate_bonds && "Corporate_bonds" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Corporate_bonds == 1")
    }
    if (input$structured_notes && "Structured_notes" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Structured_notes == 1")
    }
    if (input$collateralised && "Collateralised" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Collateralised == 1")
    }
    if (input$collective_investments && "Collective_investments" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Collective_investments == 1")
    }
    if (input$derivatives && "Derivatives" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Derivatives == 1")
    }
    if (input$deposits && "Deposits" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Deposits == 1")
    }
    if (input$other_investments && "Other_investments" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Other_investments == 1")
    }
    if (input$assets_index_linked && "Assets_index_linked" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Assets_index_linked == 1")
    }
    if (input$loans_mortgages && "Loans_mortgages" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Loans_mortgages == 1")
    }
    if (input$loans_policies && "Loans_policies" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Loans_policies == 1")
    }
    if (input$loans_individuals && "Loans_individuals" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Loans_individuals == 1")
    }
    if (input$other_loans && "Other_loans" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Other_loans == 1")
    }
    if (input$reinsurance_recoverables && "Reinsurance_recoverables" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Reinsurance_recoverables == 1")
    }
    if (input$non_life_health && "Non_life_health" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Non_life_health == 1")
    }
    if (input$non_life_excluding_health && "Non_life_excluding_health" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Non_life_excluding_health == 1")
    }
    if (input$health_similar_non_life && "Health_similar_non_life" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Health_similar_non_life == 1")
    }
    if (input$life_health && "Life_health" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Life_health == 1")
    }
    if (input$health_similar_life && "Health_similar_life" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Health_similar_life == 1")
    }
    if (input$life_excluding_health && "Life_excluding_health" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Life_excluding_health == 1")
    }
    if (input$life_index_linked && "Life_index_linked" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Life_index_linked == 1")
    }
    if (input$deposits_cedants && "Deposits_cedants" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Deposits_cedants == 1")
    }
    if (input$receivables && "Receivables" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Receivables == 1")
    }
    if (input$reinsurance_receivables && "Reinsurance_receivables" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Reinsurance_receivables == 1")
    }
    if (input$receivables_trade && "Receivables_trade" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Receivables_trade == 1")
    }
    if (input$own_shares && "Own_shares" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Own_shares == 1")
    }
    if (input$amounts_due && "Amounts_due" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Amounts_due == 1")
    }
    if (input$cash_equivalents && "Cash_equivalents" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Cash_equivalents == 1")
    }
    if (input$other_assets && "Other_assets" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Other_assets == 1")
    }
    
    
    ####################
    
    ## liabilities
    if (input$total_liabilities && "Total_liabilities" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Total_liabilities == 1")
    }
    if (input$technical_provisions_non_life && "Technical_provisions_non_life" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_non_life == 1")
    }
    if (input$technical_provisions_non_life_excluding_health && "Technical_provisions_non_life_excluding_health" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_non_life_excluding_health == 1")
    }
    if (input$technical_provisions_calculated_as_a_whole_1 && "Technical_provisions_calculated_as_a_whole_1" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_calculated_as_a_whole_1 == 1")
    }
    if (input$best_estimate_1 && "Best_estimate_1" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Best_estimate_1 == 1")
    }
    if (input$risk_margin_1 && "Risk_margin_1" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Risk_margin_1 == 1")
    }
    if (input$technical_provisions_health_similar_to_non_life && "Technical_provisions_health_similar_to_non_life" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_health_similar_to_non_life == 1")
    }
    if (input$technical_provisions_calculated_as_a_whole_2 && "Technical_provisions_calculated_as_a_whole_2" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_calculated_as_a_whole_2 == 1")
    }
    if (input$best_estimate_2 && "Best_estimate_2" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Best_estimate_2 == 1")
    }
    if (input$risk_margin_2 && "Risk_margin_2" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Risk_margin_2 == 1")
    }
    if (input$technical_provisions_life_excluding_index_linked_and_unit_linked && "Technical_provisions_life_excluding_index_linked_and_unit_linked" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_life_excluding_index_linked_and_unit_linked == 1")
    }
    if (input$technical_provisions_health_similar_to_life && "Technical_provisions_health_similar_to_life" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_health_similar_to_life == 1")
    }
    if (input$technical_provisions_calculated_as_a_whole_3 && "Technical_provisions_calculated_as_a_whole_3" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_calculated_as_a_whole_3 == 1")
    }
    if (input$best_estimate_3 && "Best_estimate_3" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Best_estimate_3 == 1")
    }
    if (input$risk_margin_3 && "Risk_margin_3" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Risk_margin_3 == 1")
    }
    if (input$technical_provisions_life_excluding_health_and_index_linked_and_unit_linked && "Technical_provisions_life_excluding_health_and_index_linked_and_unit_linked" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_life_excluding_health_and_index_linked_and_unit_linked == 1")
    }
    if (input$technical_provisions_calculated_as_a_whole_4 && "Technical_provisions_calculated_as_a_whole_4" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_calculated_as_a_whole_4 == 1")
    }
    if (input$best_estimate_4 && "Best_estimate_4" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Best_estimate_4 == 1")
    }
    if (input$risk_margin_4 && "Risk_margin_4" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Risk_margin_4 == 1")
    }
    if (input$technical_provisions_index_linked_and_unit_linked && "Technical_provisions_index_linked_and_unit_linked" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_index_linked_and_unit_linked == 1")
    }
    if (input$technical_provisions_calculated_as_a_whole_5 && "Technical_provisions_calculated_as_a_whole_5" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Technical_provisions_calculated_as_a_whole_5 == 1")
    }
    if (input$best_estimate_5 && "Best_estimate_5" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Best_estimate_5 == 1")
    }
    if (input$risk_margin_5 && "Risk_margin_5" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Risk_margin_5 == 1")
    }
    if (input$other_technical_provisions && "Other_technical_provisions" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Other_technical_provisions == 1")
    }
    if (input$contingent_liabilities && "Contingent_liabilities" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Contingent_liabilities == 1")
    }
    if (input$provisions_other_than_technical_provisions && "Provisions_other_than_technical_provisions" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Provisions_other_than_technical_provisions == 1")
    }
    if (input$pension_benefit_obligations && "Pension_benefit_obligations" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Pension_benefit_obligations == 1")
    }
    if (input$deposits_from_reinsurers && "Deposits_from_reinsurers" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Deposits_from_reinsurers == 1")
    }
    if (input$deferred_tax_liabilities && "Deferred_tax_liabilities" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Deferred_tax_liabilities == 1")
    }
    if (input$derivatives_l && "Derivatives_l" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Derivatives_l == 1")
    }
    if (input$debts_owed_to_credit_institutions && "Debts_owed_to_credit_institutions" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Debts_owed_to_credit_institutions == 1")
    }
    if (input$debts_owed_to_credit_institutions_resident_domestically && "Debts_owed_to_credit_institutions_resident_domestically" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Debts_owed_to_credit_institutions_resident_domestically == 1")
    }
    if (input$debts_owed_to_credit_institutions_resident_in_the_euro_area_other_than_domestic && "Debts_owed_to_credit_institutions_resident_in_the_euro_area_other_than_domestic" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Debts_owed_to_credit_institutions_resident_in_the_euro_area_other_than_domestic == 1")
    }
    if (input$debts_owed_to_credit_institutions_resident_in_rest_of_the_world && "Debts_owed_to_credit_institutions_resident_in_rest_of_the_world" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Debts_owed_to_credit_institutions_resident_in_rest_of_the_world == 1")
    }
    if (input$financial_liabilities_other_than_debts_owed_to_credit_institutions && "Financial_liabilities_other_than_debts_owed_to_credit_institutions" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Financial_liabilities_other_than_debts_owed_to_credit_institutions == 1")
    }
    if (input$debts_owed_to_non_credit_institutions && "Debts_owed_to_non_credit_institutions" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Debts_owed_to_non_credit_institutions == 1")
    }
    if (input$debts_owed_to_non_credit_institutions_resident_domestically && "Debts_owed_to_non_credit_institutions_resident_domestically" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Debts_owed_to_non_credit_institutions_resident_domestically == 1")
    }
    if (input$debts_owed_to_non_credit_institutions_resident_in_the_euro_area_other_than_domestic && "Debts_owed_to_non_credit_institutions_resident_in_the_euro_area_other_than_domestic" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Debts_owed_to_non_credit_institutions_resident_in_the_euro_area_other_than_domestic == 1")
    }
    if (input$debts_owed_to_non_credit_institutions_resident_in_rest_of_the_world && "Debts_owed_to_non_credit_institutions_resident_in_rest_of_the_world" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Debts_owed_to_non_credit_institutions_resident_in_rest_of_the_world == 1")
    }
    if (input$other_financial_liabilities_debt_securities_issued && "Other_financial_liabilities_debt_securities_issued" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Other_financial_liabilities_debt_securities_issued == 1")
    }
    if (input$insurance_intermediaries_payables && "Insurance_intermediaries_payables" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Insurance_intermediaries_payables == 1")
    }
    if (input$reinsurance_payables && "Reinsurance_payables" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Reinsurance_payables == 1")
    }
    if (input$payables_trade_not_insurance && "Payables_trade_not_insurance" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Payables_trade_not_insurance == 1")
    }
    if (input$subordinated_liabilities && "Subordinated_liabilities" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Subordinated_liabilities == 1")
    }
    if (input$subordinated_liabilities_not_in_Basic_Own_Funds && "Subordinated_liabilities_not_in_Basic_Own_Funds" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Subordinated_liabilities_not_in_Basic_Own_Funds == 1")
    }
    if (input$subordinated_liabilities_in_Basic_Own_Funds && "Subordinated_liabilities_in_Basic_Own_Funds" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Subordinated_liabilities_in_Basic_Own_Funds == 1")
    }
    if (input$any_other_liabilities_not_elsewhere_shown && "Any_other_liabilities_not_elsewhere_shown" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Any_other_liabilities_not_elsewhere_shown == 1")
    }
    if (input$excess_of_assets_over_liabilities && "Excess_of_assets_over_liabilities" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "Excess_of_assets_over_liabilities == 1")
    }
    
    ##################################################
    
    ## codes we dont know anything about
    
    # Check each checkbox and construct the filter conditions if columns exist
    if (input$GAR0010 && "GAR0010" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0010 == 1")
    }
    
    if (input$GAR0020 && "GAR0020" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0020 == 1")
    }
    
    if (input$GAR0030 && "GAR0030" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0030 == 1")
    }
    
    if (input$GAR0040 && "GAR0040" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0040 == 1")
    }
    
    if (input$GAR0050 && "GAR0050" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0050 == 1")
    }
    
    if (input$GAR0080 && "GAR0080" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0080 == 1")
    }
    
    if (input$GAR0100 && "GAR0100" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0100 == 1")
    }
    
    if (input$GAR0120 && "GAR0120" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0120 == 1")
    }
    
    if (input$GAR0140 && "GAR0140" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0140 == 1")
    }
    
    if (input$GAR0160 && "GAR0160" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0160 == 1")
    }
    
    if (input$GAR0110 && "GAR0110" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0110 == 1")
    }
    
    if (input$GAR0130 && "GAR0130" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0130 == 1")
    }
    
    if (input$GAR0150 && "GAR0150" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0150 == 1")
    }
    
    if (input$GAR0170 && "GAR0170" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0170 == 1")
    }
    
    if (input$GAR0090 && "GAR0090" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0090 == 1")
    }
    
    if (input$GAR0060 && "GAR0060" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0060 == 1")
    }
    
    if (input$GAR0070 && "GAR0070" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0070 == 1")
    }
    
    if (input$GAR0042 && "GAR0042" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0042 == 1")
    }
    
    if (input$GAR0041 && "GAR0041" %in% colnames(df)) {
      filter_conditions <- c(filter_conditions, "GAR0041 == 1")
    }
    
    # Apply the OR conditions if any checkboxes are selected
    if (length(filter_conditions) > 0) {
      filter_query <- paste(filter_conditions, collapse = " | ")
      df <- df %>% filter(eval(parse(text = filter_query)))
    }
    
    # Check final dataframe state before returning
    if (nrow(df) == 0) {
      return(data.frame())  # Return empty dataframe if no results
    }
    
    return(df)  # Return the filtered data frame
  })
  
  
  # Observe changes in VG selection and update Balance Sheet Position filter dynamically
  observeEvent(input$vg_filter, {
    # Filter the data based on selected VG and get unique Balance Sheet Positions
    filtered_positions <- unique(data_cleaned %>% 
                                   filter(VG %in% input$vg_filter) %>% 
                                   pull(Balance_Sheet_Position))
    
    # Update the Balance Sheet Position choices dynamically
    updateCheckboxGroupInput(session, "balance_sheet_filter", 
                             choices = filtered_positions,
                             selected = filtered_positions)  # Default to selecting all positions
  })
  
  # Display filtered data summary
  output$filtered_summary <- renderUI({
    
    filtered_data <- filtered_data()
    unique_balance_sheets_used <- unique(filtered_data$Balance_Sheet_Position)
    
    tagList(
      h4("Selected firms:"),
      p(paste(input$firm_filter, collapse = ", ")),
      hr(),  # Horizontal line
      h4("Period range:"),
      p(paste(input$date_filter[1], "to", input$date_filter[2])),
      hr(),  # Horizontal line
      h4("Solvency II template:"),
      p(paste(input$template_filter, collapse = ", ")),
      hr(),  # Horizontal line
      h4("VG:"),
      p(paste(input$vg_filter, collapse = ", ")),
      hr(),  # Horizontal line
      h4("Balance Sheet Position:"),
      p(paste(unique_balance_sheets_used, collapse = ", "))
    )
  })
  
  # Display filtered data in a table
  output$filtered_data_table <- renderDT({
    # Select only the desired columns from the filtered data
    filtered_data() %>%
      select(Firm_ID, Period, VG, Balance_Sheet_Position, Value, `Asset Name`, `Liability Name`, Solvency_II_template, Year) %>%
      datatable(
        options = list(
          autoWidth = TRUE  # Allow automatic width adjustment
        ),
        width = '75%', # Adjust to your preference (can also be fixed)
        style = 'bootstrap' # Use Bootstrap styling
      ) %>%
      formatStyle(
        'Value', # Assuming you want to highlight the Value column
        backgroundColor = styleInterval(c(0), c('lightgrey', 'white'))
      )
  })

  ########## Graphs ##############
  
  # Server logic
  output$plot_ui <- renderUI({
    # Fetch the filtered data
    filtered_data <- filtered_data()
    
    # Get the unique Balance_Sheet_Position from the filtered data
    positions <- unique(filtered_data$Balance_Sheet_Position)
    
    # Create one plotOutput for each unique Balance_Sheet_Position
    plot_outputs <- lapply(positions, function(pos) {
      plotname <- paste0("plot_", pos)
      plotOutput(outputId = plotname, height = "400px", width = "50%")
    })
    
    # Return the list of plotOutput objects wrapped in a fluidRow for spacing
    fluidRow(do.call(tagList, plot_outputs))
  })
  
  # Render each plot based on the Balance_Sheet_Position
  observe({
    filtered_data <- filtered_data()
  
    # Loop through each unique Balance_Sheet_Position and create a corresponding plot
    unique_positions <- unique(filtered_data$Balance_Sheet_Position)
    
    lapply(unique_positions, function(pos) {
      plotname <- paste0("plot_", pos)
      
      output[[plotname]] <- renderPlot({
        # Filter data for the specific Balance_Sheet_Position
        pos_data <- filtered_data %>%
          filter(Balance_Sheet_Position == pos)
        
        unique_name_l <- unique(pos_data$`Liability Name`)
        unique_name_a <- unique(pos_data$`Asset Name`)
        
          if (!is.na(unique_name_l)) {
            # We have an liability
            name <- paste("Liability:", unique_name_l, "(", pos, ")")  # Create the name string
          } else if (!is.na(unique_name_a)) {
            # We have an asset
            name <- paste("Asset:", unique_name_a, "(", pos, ")")  # Create the name string
          } else {
            # We dont know
            name <- paste("Unknown:", pos)  # Create the name string
          }
        
        
        # Create the plot using ggplot2
        ggplot(pos_data, aes(x = Time, y = Value, color = VG, group = VG)) +
          geom_line() +   # Line plot
          geom_point() +  # Points for each observation
          labs(title = paste("Firm:", unique(pos_data$Firm_ID), 
                             ":", name),
               x = "", y = "Value", color = "VG") +  # Labels for axes and legend
    scale_y_continuous(labels = comma) +  # Ensure y-axis is numeric and formatted with commas
          theme_minimal()  # A clean theme
        
      })
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)