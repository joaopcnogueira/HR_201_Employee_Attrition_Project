# BUSINESS UNDERSTANDING ----

# Libraries
library(tidyverse)
library(tidyquant)
library(readxl)

# Load Data 
path_train <- "00_Data/telco_train.xlsx"
train_raw_tbl <- read_excel(path_train, sheet = 1)

# Data Subset
dept_job_role_tbl <- train_raw_tbl %>% 
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

dept_job_role_tbl

# 1. Business Science Problem Framework ----

# 1A. View Business as Machine ----

# Business Unities: Department and Job Role
# Define Objectives: Retain High Performers
# Assess Outcomes: TBD

dept_job_role_tbl %>% 
  count(Attrition) %>% 
  mutate(pct = n / sum(n))

# 1B. Understand the Drivers ----

# Investigate Objectives: 16% Attrition
# Synthesize Outcomes: High Counts and High percentages
# Hypothesize Drivers: Job Role and Departments

# Department ----
dept_job_role_tbl %>% 
  count(Department, Attrition) %>% 
  group_by(Department) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup()

# Job Role ----
dept_job_role_tbl %>% 
  count(Department, JobRole, Attrition) %>% 
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition == "Yes") %>% 
  arrange(desc(pct))


# 1C. Measure The Drivers ----

# Collect Information on Employee Attrition: On going task

# Develops KPI's: Industry KPIs: 8.8%

dept_job_role_tbl %>% 
  count(Department, JobRole, Attrition) %>% 
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition == "Yes") %>% 
  arrange(desc(pct)) %>% 
  mutate(above_industry_avg = case_when(
    pct > 0.088 ~ "Yes",
    TRUE       ~ "No"
  ))


# 1D. Uncover Problems & Opportunities ----

calculate_attrition_cost <- function(
  
  # Employee
  n                    = 1,
  salary               = 80000,
  
  # Direct Costs
  separation_cost      = 500,
  vacancy_cost         = 10000,
  acquisition_cost     = 4900,
  placement_cost       = 3500,
  
  # Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60,
  onboarding_efficiency    = 0.50
  
) {
  
  # Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year * 
    (workdays_position_open + workdays_onboarding * (1-onboarding_efficiency)) 
  
  # Savings of Salary & Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  # Estimated Turnover Per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
  
}

calculate_attrition_cost(n = 200)

# Calculate Cost By Job Role ----
dept_job_role_tbl %>% 
  count(Department, JobRole, Attrition) %>% 
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition == "Yes") %>% 
  arrange(desc(pct)) %>% 
  mutate(above_industry_avg = case_when(
    pct > 0.088 ~ "Yes",
    TRUE        ~ "No"
  )) %>% 
  
  mutate(cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)) %>% 
  arrange(cost_of_attrition %>% desc())

