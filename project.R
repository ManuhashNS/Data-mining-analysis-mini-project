library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

file <- "C:/Users/manuh/Desktop/Manuhash/Data Mining & Analytics/Project/Merged_Expenses.xlsx"
sheets <- excel_sheets(file)

merged_data <- lapply(seq_along(sheets), function(i) {
  read_excel(file, sheet = sheets[i]) %>%
    mutate(Student_ID = i) %>%
    group_by(Student_ID) %>%
    fill(Date, .direction = "down") %>%
    ungroup()
}) %>%
  bind_rows()

merged_data <- merged_data %>%
  mutate(Date = as.Date(Date)) %>%
  filter(
    Date >= as.Date("2026-10-06"),
    Date <= as.Date("2026-11-05")
  )

spent_data <- merged_data %>%
  filter(`Spent/Received` == "Spent")

category_spent_data <- spent_data %>%
  filter(Type != "Pocket Money")

spent_data <- spent_data %>%
  mutate(Amount_log = log1p(Amount))


# Plot 1: Net balance by student
net_balance <- merged_data %>%
  group_by(Student_ID) %>%
  summarise(
    Received = sum(Amount[`Spent/Received` == "Received"], na.rm = TRUE),
    Spent = sum(Amount[`Spent/Received` == "Spent"], na.rm = TRUE),
    Net_Balance = Received - Spent,
    .groups = "drop"
  )

ggplot(net_balance, aes(x = factor(Student_ID), y = Net_Balance,
                        fill = Net_Balance >= 0)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual(
    values = c("TRUE" = "seagreen3", "FALSE" = "indianred2"),
    labels = c("TRUE" = "Surplus", "FALSE" = "Deficit")
  ) +
  labs(
    title = "Net Balance by Student",
    x = "Student ID",
    y = "Net Balance (Received - Spent)",
    fill = "Balance"
  ) +
  theme_minimal()


# Plot 2: Average transaction amount per student
avg_transaction <- merged_data %>%
  group_by(Student_ID) %>%
  summarise(
    Avg_Amount = mean(Amount, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(avg_transaction,
       aes(x = factor(Student_ID), y = Avg_Amount)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = round(Avg_Amount, 2)),
            vjust = -0.5, size = 3) +
  labs(
    title = "Average Transaction Amount per Student",
    x = "Student ID",
    y = "Average Transaction Amount"
  ) +
  theme_minimal()


# Plot 3: Median transaction amount per student
median_transaction <- merged_data %>%
  group_by(Student_ID) %>%
  summarise(
    Median_Amount = median(Amount, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(median_transaction,
       aes(x = factor(Student_ID), y = Median_Amount)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = round(Median_Amount, 2)),
            vjust = -0.5, size = 3) +
  labs(
    title = "Median Transaction Amount per Student",
    x = "Student ID",
    y = "Median Transaction Amount"
  ) +
  theme_minimal()


# Plot 4: Total spending by category
category_spending <- category_spent_data %>%
  group_by(Type) %>%
  summarise(Total_Spending = sum(Amount, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(Total_Spending))

ggplot(category_spending,
       aes(x = reorder(Type, Total_Spending), y = Total_Spending)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Total Spending by Category",
    x = "Category",
    y = "Total Spending"
  ) +
  theme_minimal()


# Plot 5: Number of transactions by category
category_transactions <- category_spent_data %>%
  group_by(Type) %>%
  summarise(Transaction_Count = n(), .groups = "drop") %>%
  arrange(desc(Transaction_Count))

ggplot(category_transactions,
       aes(x = reorder(Type, Transaction_Count),
           y = Transaction_Count)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Number of Transactions by Category",
    x = "Category",
    y = "Number of Transactions"
  ) +
  theme_minimal()


# Plot 6: Average spending per transaction by category
category_avg <- category_spent_data %>%
  group_by(Type) %>%
  summarise(Avg_Amount = mean(Amount, na.rm = TRUE),
            .groups = "drop")

ggplot(category_avg,
       aes(x = reorder(Type, Avg_Amount), y = Avg_Amount)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Avg_Amount, 2)),
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Average Spending per Transaction by Category",
    x = "Category",
    y = "Average Transaction Amount"
  ) +
  theme_minimal()


# Plot 7: Average daily spending per student
all_dates <- seq(
  min(merged_data$Date, na.rm = TRUE),
  max(merged_data$Date, na.rm = TRUE),
  by = "day"
)

all_students <- sort(unique(merged_data$Student_ID))

student_daily <- spent_data %>%
  group_by(Student_ID, Date) %>%
  summarise(Daily_Spending = sum(Amount, na.rm = TRUE),
            .groups = "drop")

student_daily_complete <- expand.grid(
  Student_ID = all_students,
  Date = all_dates
) %>%
  left_join(student_daily, by = c("Student_ID", "Date")) %>%
  mutate(Daily_Spending = ifelse(is.na(Daily_Spending), 0,
                                 Daily_Spending))

student_avg_daily <- student_daily_complete %>%
  group_by(Student_ID) %>%
  summarise(Avg_Daily_Spending = mean(Daily_Spending),
            .groups = "drop")

ggplot(student_avg_daily,
       aes(x = factor(Student_ID), y = Avg_Daily_Spending)) +
  geom_col(fill = "seagreen3") +
  labs(
    title = "Average Daily Spending per Student",
    x = "Student ID",
    y = "Average Daily Spending"
  ) +
  theme_minimal()


# Plot 8: Healthy vs unhealthy food spending by student
food_summary <- category_spent_data %>%
  filter(Type %in% c("Food (H)", "Food (UH)")) %>%
  group_by(Student_ID, Type) %>%
  summarise(Total = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
  group_by(Student_ID) %>%
  mutate(Percentage = Total / sum(Total) * 100) %>%
  ungroup()

ggplot(food_summary,
       aes(x = factor(Student_ID), y = Percentage, fill = Type)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(
    values = c("Food (H)" = "seagreen3",
               "Food (UH)" = "indianred2"),
    labels = c("Food (H)" = "Healthy Food",
               "Food (UH)" = "Unhealthy Food")
  ) +
  labs(
    title = "Healthy vs Unhealthy Food Spending by Student",
    x = "Student ID",
    y = "Percentage of Food Spending",
    fill = "Food Category"
  ) +
  theme_minimal()


# Plot 9: Category-wise spending distribution by student
category_student <- category_spent_data %>%
  group_by(Student_ID, Type) %>%
  summarise(Total_Spending = sum(Amount, na.rm = TRUE),
            .groups = "drop")

category_percentage <- category_student %>%
  group_by(Student_ID) %>%
  mutate(Percentage = Total_Spending / sum(Total_Spending) * 100) %>%
  ungroup()

ggplot(category_percentage,
       aes(x = factor(Student_ID), y = Type, fill = Percentage)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +
  labs(
    title = "Category-wise Spending Distribution (%)",
    x = "Student ID",
    y = "Category",
    fill = "Percentage"
  ) +
  theme_minimal()


# Plot 10: Total spending per week
weekly_spending <- spent_data %>%
  mutate(Week_Start = Date - (as.POSIXlt(Date)$wday + 6) %% 7) %>%
  group_by(Week_Start) %>%
  summarise(Total_Spending = sum(Amount, na.rm = TRUE),
            .groups = "drop")

ggplot(weekly_spending,
       aes(x = Week_Start, y = Total_Spending)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Total Spending per Week",
    x = "Week Starting",
    y = "Total Spending"
  ) +
  theme_minimal()


# Plot 11: Total spending per day
daily_spending <- spent_data %>%
  group_by(Date) %>%
  summarise(Total_Spending = sum(Amount, na.rm = TRUE),
            .groups = "drop")

ggplot(daily_spending,
       aes(x = Date, y = Total_Spending,
           fill = Total_Spending == max(Total_Spending))) +
  geom_col() +
  scale_fill_manual(
    values = c("FALSE" = "steelblue", "TRUE" = "red"),
    guide = "none"
  ) +
  labs(
    title = "Total Spending per Day",
    subtitle = "Highest spending day highlighted in red",
    x = "Date",
    y = "Total Spending"
  ) +
  theme_minimal()


# Plot 12: Number of transactions by payment app
payment_data <- spent_data %>%
  filter(!is.na(`Payment Type`), `Payment Type` != "")

app_transactions <- payment_data %>%
  group_by(`Payment Type`) %>%
  summarise(Transaction_Count = n(), .groups = "drop")

ggplot(app_transactions,
       aes(x = reorder(`Payment Type`, Transaction_Count),
           y = Transaction_Count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Number of Transactions by Payment App",
    x = "Payment App",
    y = "Number of Transactions"
  ) +
  theme_minimal()


# Plot 13: Distribution of spending amounts
ggplot(spent_data, aes(x = Amount)) +
  geom_histogram(bins = 30, fill = "steelblue",
                 color = "black") +
  labs(
    title = "Distribution of Spending Amounts",
    x = "Amount",
    y = "Frequency"
  ) +
  theme_minimal()


# Plot 14: Log-transformed spending amounts
ggplot(spent_data, aes(x = Amount_log)) +
  geom_histogram(bins = 30, fill = "darkorange",
                 color = "black") +
  labs(
    title = "Log Transformed Spending Amounts",
    x = "log(Amount + 1)",
    y = "Frequency"
  ) +
  theme_minimal()


# Plot 15: Spending distribution without outliers
Q1 <- quantile(spent_data$Amount, 0.25, na.rm = TRUE)
Q3 <- quantile(spent_data$Amount, 0.75, na.rm = TRUE)
IQR_value <- IQR(spent_data$Amount, na.rm = TRUE)
lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value

no_outliers <- spent_data %>%
  filter(Amount >= lower, Amount <= upper)

ggplot(no_outliers, aes(x = Amount)) +
  geom_histogram(bins = 30, fill = "seagreen",
                 color = "black") +
  labs(
    title = "Spending Distribution Without Outliers",
    x = "Amount",
    y = "Frequency"
  ) +
  theme_minimal()


# Plot 16: Box plot of spending amounts
ggplot(spent_data, aes(x = "", y = Amount)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Box Plot of Spending Amounts",
    x = NULL,
    y = "Amount"
  ) +
  theme_minimal()


# Plot 17: Box plot of log-transformed spending amounts
ggplot(spent_data, aes(x = "", y = Amount_log)) +
  geom_boxplot(fill = "darkorange") +
  labs(
    title = "Box Plot of Log Transformed Amounts",
    x = NULL,
    y = "log(Amount + 1)"
  ) +
  theme_minimal()
