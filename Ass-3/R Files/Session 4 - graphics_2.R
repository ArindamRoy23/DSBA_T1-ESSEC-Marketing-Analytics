# Load the packages
library(RODBC)
library(ggplot2)

# Connect to MySQL (use your own credentials)
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity")

# Extract the donation amounts per day of week
query = "SELECT DAYNAME(act_date) AS 'day_name', amount FROM acts WHERE act_type_id = 'DO' AND YEAR(act_date) = 2021"
weekdays = sqlQuery(db, query)

# ____________________________________________________________________________________________________


# Plot the results as boxplots
# Boxplot : 25 percentile - median - 75 percentile + "outliers"
# Notice that day_name here is a factor, not a continuous value
gr = ggplot(data = weekdays, aes(x = day_name, y = amount)) +
   geom_boxplot()
print(gr)

# Only show a part of the y-axis
# Do NOT run something like "WHERE Amount <= 200", that woud be wrong
gr = ggplot(data = weekdays, aes(x = day_name, y = amount)) +
   geom_boxplot() +
   scale_y_continuous(limits = c(0, 200))
print(gr)

# Use day_name as a factor to fill each boxplot with a different color
gr = ggplot(data = weekdays, aes(x = day_name, y = amount, fill = day_name)) +
   geom_boxplot() +
   scale_y_continuous(limits = c(0, 200))
print(gr)

# Factors are always shown in alphabetical order, which might not always make sense
# This shows how to reorder it...
# Re-order the factor "day_name"
weekdays$day_name = factor(weekdays$day_name, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
gr = ggplot(data = weekdays, aes(x = day_name, y = amount, fill = day_name)) +
   geom_boxplot() +
   scale_y_continuous(limits = c(0, 200))
print(gr)

# ggplot also allows statistics (here, a mean)
# Add a mean to the boxplot
gr = ggplot(data = weekdays, aes(x = day_name, y = amount, fill = day_name)) +
   geom_boxplot() +
   stat_summary(fun.y = mean, colour = "black", geom = "point", shape = 4, size = 5, show_guide = FALSE) +
   scale_y_continuous(limits = c(0, 200))
print(gr)

# ____________________________________________________________________________________________________


# Close the connection
odbcClose(db)
