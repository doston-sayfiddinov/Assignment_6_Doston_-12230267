


# Step 1

set.seed(123)

customers <- data.frame(
  customer_id = 1:60,
  name = sample(c("John", "Doston", "Murtazo", "Azizbek", "DAvid", "Alex",
                  "Michael", "Susan", "James", "Isabella"),
                60, replace = TRUE),
  age = round(rnorm(60, mean = 35, sd = 12)),
  city = sample(c("Incheon", "Tashkent", "Paris", "Berlin", "Moscow"),
                60, replace = TRUE)
)
#### fix unrealistic ages #####
customers$age[customers$age < 18] <- 18
customers$age[customers$age > 65] <- 65

transactions <- data.frame(
  transaction_id = 1:200,
  customer_id = sample(1:60, 200, replace = TRUE),
  product = sample(c("Laptop", "Phone", "Headphones", "Tablet", "Mouse"), 200, replace = TRUE),
  amount = rnorm(200, mean = 150, sd = 50),
  purchase_date = sample(seq(as.Date("2025-01-01"), as.Date("2025-05-01"), by = "day"), 200, replace = TRUE)
)

head(transactions)
head(customers)




# Step 2

library(dplyr)


# 1.Select
customers_selected <- select(customers, customer_id, name, age, city)
transactions_selected <- select(transactions, transaction_id, customer_id, product, amount)
# 2.Filter
high_value_transactions <- filter(transactions, amount > 200)
customers$city <- sample(c("Incheon", "Seoul", "Tashkent", "Paris", "Berlin"),
                         60, replace = TRUE)
seoul_customers <- filter(customers, city == "Seoul")

# 3.Arrange
transactions_sorted <- arrange(transactions, desc(amount))
customers_sorted <- arrange(customers, age)

# 4.Mutate
transactions <- mutate(transactions,
                       discounted_amount = amount * 0.9)
customers <- mutate(customers,
                    age_group = case_when(
                      age < 30 ~ "Young",
                      age >= 30 & age < 50 ~ "Middle",
                      TRUE ~ "Senior"
                    ))

# 5.Group By
avg_spending <- transactions %>%
  group_by(customer_id) %>%
  summarise(avg_amount = mean(amount))

# Total spending by city
total_by_city <- transactions %>%
  group_by(customer_id) %>%
  summarise(total_spent = sum(amount)) %>%
  left_join(customers, by = "customer_id") %>%
  group_by(city) %>%
  summarise(city_total = sum(total_spent))

# 6.Merge

mergedData <- merge(transactions, customers,
                    by.x = "customer_id",
                    by.y = "customer_id",
                    all = TRUE)
# 7.Match
no_transaction_customers <- customers[
  !(customers$customer_id %in% transactions$customer_id),
]







