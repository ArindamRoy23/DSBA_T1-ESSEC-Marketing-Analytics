# Load the packages
library(RODBC)
library(ggplot2)
library(scales)

# Connect to MySQL (use your own credentials)
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity")

# Fundraising is highly cyclical, especially regular donations
# We'd like to show how the fundraising has evolved during the years 2019-2022
query = "SELECT
           act_date,
           YEAR(act_date) AS 'year',
           SUM(amount) AS sum_amount
         FROM acts
         WHERE YEAR(act_date) >= 2019
           AND act_type_id = 'DO'
         GROUP BY act_date
         ORDER BY act_date"
fundraising = sqlQuery(db, query)

# ____________________________________________________________________________________________________


# Plot fundraising for each day (not very useful)
gr = ggplot(data = fundraising, aes(x = act_date, y = sum_amount)) + geom_line()
print(gr)

# Plot fundraising for each day, color year
# YEAR is a continuous variable, not a factor, so it is shown on a continuous scale
# This is not what we want
gr = ggplot(data = fundraising, aes(x = act_date, y = sum_amount, color = year)) + geom_line()
print(gr)

# Transform year as a factor and plot again
fundraising$year = factor(x = fundraising$year)
gr = ggplot(data = fundraising, aes(x = act_date, y = sum_amount, color = year)) + geom_line()
print(gr)

# We can cumulate fundraising over time, but it does not help us very much
# to compare one year to the next
fundraising$sum_amount = cumsum(fundraising$sum_amount)
gr = ggplot(data = fundraising, aes(x = act_date, y = sum_amount, color = year)) + geom_line()
print(gr)

# ____________________________________________________________________________________________________


# We need the "x" to be comparable across years, for instance by extracting
# the number of days elapsed since January 1st from database instead of the date per se
query = "SELECT
           act_date,
           YEAR(act_date) AS 'year',
           DATEDIFF(act_date, CONCAT(YEAR(act_date),'-01-01')) + 1 AS num_days,
           SUM(amount) AS sum_amount
         FROM acts
         WHERE YEAR(act_date) >= 2019
           AND act_type_id = 'DO'
         GROUP BY act_date
         ORDER BY act_date"
fundraising = sqlQuery(db, query)
print(head(fundraising))

# Show fundraising on same x axis
# Still not very useful though (messy)
fundraising$year = factor(x = fundraising$year)
gr = ggplot(data = fundraising, aes(x = num_days, y = sum_amount, color = year)) + geom_line()
print(gr)

# We cannot simply apply cumsum() to the sum_amount column
# Can you guess why? Uncomment the last line to figure it out
fundraising$cum_amount = 0
for (y in 2019:2022) {
   z = which(fundraising$year == y)
   fundraising$cum_amount[z] = cumsum(fundraising$sum_amount[z])
}
# fundraising$cum_amount = cumsum(fundraising$sum_amount)

# This is finally what we need...
gr = ggplot(data = fundraising, aes(x = num_days, y = cum_amount, color = year)) +
   geom_line(size = 1) +
   scale_y_continuous(name = "Cumulative fundraising", labels = comma)
print(gr)

# ____________________________________________________________________________________________________


# Close the connection
odbcClose(db)
