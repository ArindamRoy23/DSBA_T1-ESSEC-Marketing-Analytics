# Load the package
library(RODBC)

# Connect to MySQL (use your credentials)
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity")

# Get key data (for a random sample of 1,000 contacts)
query = "SELECT contact_id,
                DATEDIFF(20220530, MAX(act_date)) / 365 AS 'recency',
                COUNT(amount) AS 'frequency',
                AVG(amount) AS 'avgamount',
                DATEDIFF(20220530, MIN(act_date)) / 365 AS 'firstdonation'
         FROM acts
         WHERE act_type_id = 'DO'
         GROUP BY 1
         ORDER BY RAND()
         LIMIT 1000"
data = sqlQuery(db, query)

# Close the connection
odbcClose(db)

# Assign contact id as row names, remove id from data
rownames(data) = data$contact_id
data = data[, -1]

# Compute distance metrics on standardized data
d = dist(scale(data))

# Perform hierarchical clustering on distance metrics
c = hclust(d, method="ward.D2")

# Plot de dendogram
plot(c)

# Cut at 5 segments
members = cutree(c, k=5)

# Show 50 first donors, frequency table
print(members[1:50])
print(table(members))

# Show profile of each segment
for (i in 1:5) {
   print(colMeans(data[members == i, ]))
}


