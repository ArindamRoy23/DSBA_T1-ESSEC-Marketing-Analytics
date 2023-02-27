# Load the packages
library(RODBC)
library(ggplot2)

# Connect to MySQL (use your own credentials)
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity")

# ____________________________________________________________________________________________________


# Extract all data from the 'acts' table with MessageId = '11MT1SQC', and print first few lines
query = "SELECT * FROM acts WHERE campaign_id = 'SV21' AND act_type_id = 'DO' ORDER BY act_date"
data = sqlQuery(db, query)
print(head(data))

# Note the three core elements: data, aesthetics, and geom
# Histogram of donation amount
# In the interactive console, you just need to "ggplot(...)" to see the chart
# "ggplot(...)" will also display the plot if you run line by line, but not if you "Source on save"
gr = ggplot(data = data, aes(x = amount)) + geom_histogram()
print(gr)

# Histogram with a specified binwidth
# Binwidth is not an aesthetic, it's just a parameter of the function
gr = ggplot(data = data, aes(x = data$amount)) + geom_histogram(binwidth = 10, ) + xlim(0, 200)
print(gr)

# ____________________________________________________________________________________________________


# Get the data re: campaign C158-3, cumulated per day
query = "SELECT act_date, COUNT(amount) AS 'counter', SUM(amount) AS 'sum_amount'
         FROM acts
         WHERE campaign_id = 'SV21'
           AND act_type_id = 'DO'
         GROUP BY act_date
         ORDER BY act_date"
campaign = sqlQuery(db, query)
print(head(campaign))

# Simple plot (points)
# Two columns of the data are used for the aesthetics (x, y)
gr = ggplot(data = campaign, aes(x = act_date, y = counter)) + geom_point()
print(gr)

# Simple plot (points + lines)
gr = ggplot(data = campaign, aes(x = act_date, y = counter)) +
   geom_point() +
   geom_line()
print(gr)

# Simple plot (points + 2 lines)
# The aesthetics in ggplot(...) are the defaults for all the geoms
# But you can set set specific aesthetics within each geom
# (this chart does not make a lot of sense, by the way, just an illustration)
gr = ggplot(data = campaign, aes(x = act_date, y = counter)) +
   geom_point() +
   geom_line(aes(y = counter + 10)) +
   geom_line(aes(y = counter - 10))
print(gr)

# Simple plot (points + smoother)
# geom_smooth is a very specific (and useful) kind of geom
gr = ggplot(data = campaign, aes(x = act_date, y = counter)) +
   geom_point() +
   geom_smooth()
print(gr)

# Simple plot (points + aes(size))
# Each point is defined by 3 aesthetics : x-y coordinates + size
gr = ggplot(data = campaign, aes(x = act_date, y = counter, size = sum_amount)) +
   geom_point()
print(gr)

# Simple plot (points + aes(size, color))
# Each point is defined by 4 aesthetics : x-y coordinates + size + color
gr = ggplot(data = campaign, aes(x = act_date, y = counter, color = sum_amount, size = sum_amount / counter)) + geom_point()
print(gr)

# Simple plot (lines + aes(size, color))
# Warning: this does not make any sense. It's not because you can than you should...
gr = ggplot(data = campaign, aes(x = act_date, y = counter, color = sum_amount, size = sum_amount)) + geom_line()
print(gr)

# And now compare the next 3 charts...

# We want to use big purple dots instead of small black ones. How to ?
# This is incorrect. Color and size are used as if they were data (constants)
# Constants are NOT aesthethics, because they do not convey any information about the data
gr = ggplot(data = campaign, aes(x = act_date, y = counter, color = 'purple', size = 4)) + geom_point()
print(gr)

# This chart is grammatically correct but has no effect
# When you set aesthetics within ggplot(...), they become the defaults of all the geoms
# But the constants (function parameters) only apply to the current function
gr = ggplot(data = campaign, aes(x = act_date, y = counter), color = 'purple', size = 4) + geom_point()
print(gr)

# This one is the correct and effective way of doing it
gr = ggplot(data = campaign, aes(x = act_date, y = counter)) + geom_point(color = 'purple', size = 4)
print(gr)

# ____________________________________________________________________________________________________


# Compute cumulative fundraising, and scale both to a max of 100%
campaign$cumul_counter     = cumsum(campaign$counter)
campaign$cumul_amount      = cumsum(campaign$sum_amount)
campaign$cumul_counter_100 = cumsum(campaign$counter)    / sum(campaign$counter)
campaign$cumul_amount_100  = cumsum(campaign$sum_amount) / sum(campaign$sum_amount)
print(head(campaign))

# Cumulative fundraising over time
# Note that size and color are NOT aesthetics, they are constants
gr = ggplot(data = campaign, aes(x = act_date, y = cumul_counter)) +
   geom_line(size = 2, color = 'darkred')
print(gr)

# Make it pretty...
# There are a lot of functions to format the legend, axes, title, etc.
gr = ggplot(data = campaign, aes(x = act_date, y = cumul_counter)) +
   geom_line(size = 2, color = 'darkred') +
   ggtitle("Campaign SV21") +
   theme(plot.title = element_text(lineheight=1.2, face="bold")) +
   scale_y_continuous(name = "Number of donations")
print(gr)

# ____________________________________________________________________________________________________


# Close the connection
odbcClose(db)
