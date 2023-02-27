# Load the packages we'll use in this
library(RODBC)
library(ggplot2)
library(scales)
library(ggrepel)

# Connect to MySQL (use your own credentials)
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity")

# Report the financial contribution in "period 0" (last 12 months) of donors
# grouped by which segment they used to belong to in period 1 (a year before)
# This assumes that you have ran the SQL segmentation queries from session 3
query = "SELECT s.segment,
                COUNT(DISTINCT(s.contact_id)) AS 'numdonors',
                COUNT(a.amount)               AS 'numdonations',
                CEILING(AVG(a.amount))        AS 'avgamount',
                CEILING(SUM(a.amount))        AS 'totalgenerosity'
         FROM
           segments s
         LEFT JOIN
           (SELECT *
            FROM acts a,
                 periods p
            WHERE p.period_id = 0
              AND a.act_date >= p.first_day
			     AND a.act_date <  p.last_day) AS a
         ON (a.contact_id = s.contact_id)
         WHERE (s.period_id = 1) AND
               (s.segment IS NOT NULL)
         GROUP BY 1"
segments = sqlQuery(db, query)
print(segments)

# ____________________________________________________________________________________________________


# Show the number of donors per segment (a year ago)
# This is the first step to a pie chart
pie = ggplot(segments, aes(x = factor(1), y = numdonors, fill = segment)) +
   geom_bar(width = 1, stat = "identity")
print(pie)

# A pie chart is "simply" a geom_bar chart where the y value is plotted on polar coordinates
# Note that the "grammar of graphics" does NOT like pie charts (hard to read, misleading,
# too much noise for too little information, 3D makes things even worse)
pie = ggplot(segments, aes(x = factor(1), y = numdonors, fill = segment)) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar(theta = "y")
print(pie)

# Re-order the factor "segment" and modify the palette
segments$segment = factor(segments$segment, levels = c("AUTO", "TOP", "BOTTOM", "NEW", "WARM", "COLD", "LOST"), ordered = TRUE)
pie = ggplot(segments, aes(x = factor(1), y = numdonors, fill = segment, order = factor(segment))) +
   geom_bar(width = 1, stat = "identity") +
   coord_polar(theta = "y") +
   scale_fill_brewer(palette = "RdYlBu")
print(pie)

# The grammar of graphic would recommend a simple geom_bar instead
# Note that labels (geom_text) is just another geom
chart = ggplot(segments, aes(x = segment, y = numdonors, fill = segment)) +
   geom_bar(stat = "identity") +
   coord_flip() +
   scale_fill_brewer(palette = "RdYlBu") +
   geom_text(aes(label = numdonors, hjust = -0.2))
print(chart)

# ____________________________________________________________________________________________________


# We'd like to compare % of donors to % of fundraising
# Express number of donors and fundraising as a % of...
segments$numdonors_pc       = segments$numdonors       / sum(segments$numdonors)
segments$totalgenerosity_pc = segments$totalgenerosity / sum(segments$totalgenerosity)
print(segments)

# This is NOT how you are supposed to do it
chart = ggplot(segments, aes(x = segment)) +
   geom_bar(stat = "identity", aes(y = numdonors_pc),       fill = "darkred") +
   geom_bar(stat = "identity", aes(y = totalgenerosity_pc), fill = "darkorange") +
   coord_flip()
print(chart)

# Since you are comparing two categories of data (% donors and % fundraising), you need that
# category to appear as a factor in your data frame
# Here, we prepare the data in the right format ("long" form), but not effectively at all
# There are better ways to do it (look at tidyr, for instance), but this way is easier to understand
df = rbind.data.frame(cbind.data.frame(segment = segments$segment, percent = segments$numdonors_pc,       category = "Donors"),
                      cbind.data.frame(segment = segments$segment, percent = segments$totalgenerosity_pc, category = "Fundraising"))
print(df)

# Use "category" as a fill aesthetic, the way it should be
# Note the "dodge" position, which is a geom_bar parameter (the default is "stack")
chart = ggplot(data = df, aes(x = segment, y = percent, fill = category)) +
   geom_bar(stat = "identity", position = "dodge") +
   coord_flip() +
   scale_y_continuous(labels = percent)
print(chart)

# Draw the same chart as before, but using facets
# Facets use a factor from the data, and draw as many charts as there are different values
# taken by that factor
# Note also how we have formatted the geom_text
chart = ggplot(data = df, aes(x = segment, y = percent, fill = category)) +
   geom_bar(stat = "identity") +
   facet_grid(. ~ category) +
   geom_text(aes(label = paste(round(100 * percent, 1), "%", sep = "")), size = 4, fontface = "bold") +
   coord_flip() +
   scale_y_continuous(labels = percent) +
   theme(legend.position = "none")
print(chart)

# ____________________________________________________________________________________________________

# Create a segmentation "gain chart"
# First, compute the total observed generosity per donor, active or not
# Second, re-order data frame (not the factor!) in decreasing order of such ratio
# Show how the data frame "segments" is modified at each operation
print(segments)
segments$ratio = segments$totalgenerosity / segments$numdonors
print(segments)
segments = segments[order(segments$ratio, decreasing = TRUE), ]
print(segments)

# Compute cumulative donors and fundraising
segments$cum_donors      = cumsum(segments$numdonors)       / sum(segments$numdonors)
segments$cum_fundraising = cumsum(segments$totalgenerosity) / sum(segments$totalgenerosity)

# Add a "zero"
# This is how much is collected from no donor at all
segments = rbind.data.frame(0, segments)
print(segments)

# Create a gain chart using the smart geom_text_repel() function from the ggrepel library
gain = ggplot(data = segments, aes(x = cum_donors, y = cum_fundraising)) +
   geom_line(aes(y = cum_donors), color = "grey", size = 1, linetype = 2) +
   geom_line(color = "darkgreen", size = 2) +
   geom_point(size = 4) +
   geom_text_repel(aes(label = segment)) +
   scale_x_continuous(labels = percent) +
   scale_y_continuous(labels = percent)
print(gain)


# ____________________________________________________________________________________________________


# Close the connection
odbcClose(db)
