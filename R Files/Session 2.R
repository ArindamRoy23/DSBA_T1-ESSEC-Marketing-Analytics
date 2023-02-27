# Load the package
library(RODBC)

# Connect to you new data source
# Mine is “mysql_server_64”, with “root”/”” for credentials
db = odbcConnect("mysql_server_64", uid="root", pwd="")

# Load the first 1000 rows of a table into a data frame
query = "SELECT * FROM ma_charity_small.acts"
df = sqlQuery(db, query)

print(sum(df$amount))

# If embedded in a function, use print(head(df)) instead
print(head(df))

# Close the connection
odbcClose(db)





# Size of test
N = 10000000

# Without pre-allocating memory
start_at = proc.time()
x = vector()
for (i in 1:N) {
   x[i] = i
}
print(proc.time() - start_at)

# Pre-allocating memory makes a world of difference
start_at = proc.time()
x = vector(length = N)
for (i in 1:N) {
   x[i] = i
}
print(proc.time() - start_at)