# Load the package
library(RODBC)

# Connect to MySQL (use your credentials)
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity_small")

# Extract data from database
query = "SELECT contact_id,
                DATEDIFF(20220530, MAX(act_date)) / 365 AS 'recency',
                COUNT(amount) AS 'frequency',
                AVG(amount) AS 'avgamount',
                DATEDIFF(20220530, MIN(act_date)) / 365 AS 'firstdonation'
         FROM acts
         WHERE act_type_id = 'DO'
         GROUP BY 1"
data = sqlQuery(db, query)

# Close the connection
odbcClose(db)

# Assign contact id as row names, remove id from data
rownames(data) = data$contact_id
data = data[, -1]

# Perform segmentation on standardized data
k = kmeans(x = scale(data), centers = 5, nstart = 50)

# Print cluster size, standardized centers, and
# un-standardized centers
print(k$size)
print(k$centers)
for (i in 1:5) {
   print(colMeans(data[k$cluster == i, ]))
}


