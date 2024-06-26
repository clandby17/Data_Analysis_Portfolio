library(dplyr)
library(ggplot2) 

# Import csv file
ltv_analysis <- read.csv("C:/Users/calvi/Downloads/ltv_analysis.csv")

# Add aov, purchase freq, customer value, retention, churn, and cLTV
ltv <- ltv_analysis %>%
  group_by(customer_id) %>%
  summarize(total_revenue = sum(average_order_value),
            num_orders = n(),
            AOV = total_revenue / num_orders,
            num_customers = 1,
            avg_purchase_freq_rate = num_orders / num_customers,
            customer_value = (total_revenue / num_orders) * (num_orders / num_customers),
            retention = 0.1325, churn = 1 - retention, avg_customer_lifespan = 1 / churn,
            cLTV = customer_value * avg_customer_lifespan)

# Establish cLTV tiers
ltv_tiers <- ltv %>%
  group_by(customer_id) %>%
  summarize(cLTV = sum(cLTV)) %>%
  arrange(desc(cLTV)) %>%
  mutate(ltv_tier = case_when(
    cLTV > 2 * sd(cLTV) ~ "Tier 1",
    cLTV > mean(cLTV) ~ "Tier 2",
    TRUE ~ "Tier 3"
  ))
 



         

