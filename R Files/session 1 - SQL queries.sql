Do not execute this line...;


# Find where data files are stored locally
SHOW VARIABLES LIKE 'datadir';


# Create an empty database
CREATE DATABASE ma_charity_small;


# Show the existing databases
SHOW DATABASES;


# Use a specific database
USE ma_charity_small;


# Create contact table (as an example)
CREATE TABLE `contacts` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `prefix_id` varchar(5) DEFAULT NULL,
  `first_name_clean` varchar(32) DEFAULT NULL,
  `zip_code` varchar(5) DEFAULT NULL,
  `town_clean` varchar(60) DEFAULT NULL,
  `code_geo` varchar(5) DEFAULT NULL,
  `active` tinyint(3) unsigned DEFAULT '1',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM;


# Load a text file into a table
# (Please note this is an hypothetical example, just so you know how to load a text file
# into an existing table. It will NOT work if you run it.)
LOAD DATA LOCAL INFILE 'C:/Documents/Databases/charity/contacts.txt' INTO TABLE contacts;


# Insert a row in a table
# Note that AUTO_INCREMENT fields... auto increment
# Field names are optional if you set a value for all
# Watch out for the NOT NULL fields
INSERT INTO contacts
   (id, prefix_id, first_name_clean, zip_code)
 VALUES (249280, "MR", "ARNAUD", 95000);


# Update a row
UPDATE contacts
SET prefix_id = "DR"
WHERE id = 249280;


# Delete a row
DELETE FROM contacts
WHERE id = 249280;


# Select (list) all data from a table
SELECT * FROM contacts;


# Select all data from a table, and save into file
SELECT * INTO OUTFILE 'contacts.txt' FROM contacts;


# Select specific fields from a table
SELECT first_name_clean, zip_code FROM contacts;


# Select aggregate functions
SELECT MIN(act_date),
       MAX(act_date),
       COUNT(*),
       SUM(amount),
       AVG(amount)
FROM acts;


# Rename output (alias)
SELECT MIN(act_date) AS firstgift,
       MAX(act_date) AS lastgift,
       COUNT(*)      AS numgifts,
       SUM(amount)   AS sumgifts,
       AVG(amount)   AS averagegift
FROM acts;


# Sum of donations, per year
SELECT YEAR(act_date), SUM(amount)
FROM acts
GROUP BY 1
ORDER BY 1;


# This is equivalent, with an alias, and
# listed in decreasing order
# Notice the character ` so MySQL is not confused (YEAR is a reserved keyword)
SELECT YEAR(act_date) AS `year`, SUM(amount)
FROM acts
GROUP BY `year`
ORDER BY `year` DESC;


# List first names in decreasing order of occurrence
SELECT first_name_clean, COUNT(*)
FROM contacts
GROUP BY 1
ORDER BY 2 DESC;


# List the ten most common first names
SELECT first_name_clean, COUNT(*)
FROM contacts
GROUP BY 1
ORDER BY 2 DESC
LIMIT 10;


# List donors and key marketing indicators
# by decreasing order of average donation
SELECT contact_id,
       AVG(amount) AS averageamount,
       COUNT(*)    AS numdonations,
       SUM(amount) AS totalgenerosity
FROM acts
GROUP BY contact_id
ORDER BY averageamount DESC;


# List gifts of 1000 EUR or more
SELECT *
FROM acts
WHERE amount >= 1000;


# List donors who made gifts of 1000 EUR or more
SELECT contact_id
FROM acts
WHERE Amount >= 1000
ORDER BY contact_id;


# Same, but exclude duplicates
SELECT DISTINCT(contact_id)
FROM acts
WHERE amount >= 1000
ORDER BY contact_id;


# List the ten most common first names,
# but exclude NULL values
SELECT first_name_clean, COUNT(*)
FROM contacts
WHERE first_name_clean IS NOT NULL
GROUP BY first_name_clean
ORDER BY 2 DESC
LIMIT 10;


# This is equivalent...
# COUNT(*)     = number of rows
# COUNT(field) = number of non-null values
SELECT first_name_clean, COUNT(first_name_clean)
FROM contacts
GROUP BY first_name_clean
ORDER BY 2 DESC
LIMIT 10;


# List contact information of donors who made
# single donations of 1000 EUR or more
# But you will have duplicates !!!
SELECT contacts.id,
       contacts.first_name_clean,
       contacts.prefix_id,
       contacts.zip_code
FROM contacts
JOIN acts
ON contacts.id = acts.contact_id
WHERE acts.amount >= 1000;


# List contact information of donors who made
# single donations of 1000 EUR or more
# GROUP BY will remove duplicates,
# BUT IT MAY NOT WORK, depending on your MySQL version
SELECT contacts.id,
       contacts.first_name_clean,
       contacts.prefix_id,
       contacts.zip_code
FROM contacts
JOIN acts
ON contacts.id = acts.contact_id
WHERE acts.amount >= 1000
GROUP BY contacts.id;


# This will always work,
# regardless of your MySQL version
SELECT contacts.id,
       ANY_VALUE(contacts.first_name_clean),
       ANY_VALUE(contacts.prefix_id),
       ANY_VALUE(contacts.zip_code)
FROM contacts
JOIN acts
ON contacts.id = acts.contact_id
WHERE acts.amount >= 1000
GROUP BY contacts.id;


# This is equivalent, less verbose, no ambiguity
SELECT c.id,
       ANY_VALUE(c.first_name_clean),
       ANY_VALUE(c.prefix_id),
       ANY_VALUE(c.zip_code)
FROM contacts AS c
JOIN acts AS a
ON c.id = a.contact_id
WHERE a.amount >= 1000
GROUP BY c.id;


# This is also equivalent, with a WHERE clause
SELECT c.id,
       ANY_VALUE(c.first_name_clean),
       ANY_VALUE(c.prefix_id),
       ANY_VALUE(c.zip_code)
FROM contacts AS c,
     acts AS a
WHERE (c.id = a.contact_id)
  AND (a.amount >= 1000)
GROUP BY c.id;


# --- SESSION 1 STOPS ROUGHLY AROUND HERE


# List the most generous first names
SELECT c.first_name_clean,
       FLOOR(AVG(a.amount)) AS averagegift
FROM acts AS a
JOIN contacts AS c
ON a.contact_id = c.id
GROUP BY 1
ORDER BY 2 DESC;


# List the most generous first names, but only
# if there are enough observations
# HAVING is “like” WHERE, but used after grouping
SELECT c.first_name_clean,
       FLOOR(AVG(a.amount)) AS averagegift
FROM acts AS a
JOIN contacts AS c
ON a.contact_id = c.id
GROUP BY 1
HAVING COUNT(c.first_name_clean) >= 10
ORDER BY 2 DESC;


# Compute key marketing indicators for each donor
# Since we are computing aggregate functions,
# do not forget GROUP BY
# ('dpt' stands for 'department')
# Warning: we are missing contacts. Why?
SELECT c.id,
       LEFT(ANY_VALUE(c.zip_code), 2) AS dpt,
       MIN(a.act_date)                AS firstgift,
       MAX(a.act_date)                AS recency,
       CEILING(AVG(a.amount))         AS avgamount,
       COUNT(a.amount)                AS frequency
FROM contacts AS c
JOIN acts AS a
ON c.id = a.contact_id
GROUP BY c.id;


# Same query with a LEFT JOIN
SELECT c.id,
       LEFT(ANY_VALUE(c.zip_code), 2) AS dpt,
       MIN(a.act_date)                AS firstgift,
       MAX(a.act_date)                AS recency,
       CEILING(AVG(a.amount))         AS avgamount,
       COUNT(a.amount)                AS frequency
FROM contacts AS c
LEFT JOIN acts AS a
ON c.id = a.contact_id
GROUP BY c.id;


# Count the number of donors by frequency
# of regular donations (“DO”), excluding
# automatic deductions (“PA”)

# Step 1, compute frequencies
SELECT contact_id, COUNT(*) AS frequency
FROM acts
WHERE act_type_id = "DO"
GROUP BY contact_id;

# Step 2, group donors by frequencies
# Note that every derived table needs its own alias
SELECT COUNT(frequency) AS counter, frequency
FROM (SELECT contact_id, COUNT(*) AS frequency
      FROM acts
      WHERE act_type_id = "DO"
      GROUP BY contact_id) AS q
GROUP BY frequency
ORDER BY frequency;


# Report average frequency and donation amount
# by prefix

# Step 1
SELECT contact_id,
       COUNT(*) AS frequency,
       AVG(amount) AS avgamount
FROM acts
WHERE act_type_id = "DO"
GROUP BY contact_id;

# Step 2
# Note the difference between computing
# AVG(amount) and AVG(avgamount)
SELECT prefix_id,
       AVG(frequency),
       AVG(avgamount),
       COUNT(*)
FROM contacts c
JOIN (SELECT contact_id,
             COUNT(*) AS frequency,
             AVG(amount) AS avgamount
      FROM acts
      WHERE act_type_id = "DO"
      GROUP BY contact_id) AS q
ON c.id = q.contact_id
GROUP BY 1
ORDER BY 2 DESC;


# Compute number of regular donations and number
# of automatic deductions for all donors
# Note: query may be a bit slow
SELECT c.id, d.frequency, p.frequency
FROM contacts AS c
LEFT JOIN (SELECT contact_id, COUNT(*) AS frequency
           FROM acts WHERE act_type_id = "DO"
           GROUP BY 1) AS d
ON c.id = d.contact_id
LEFT JOIN (SELECT contact_id, COUNT(*) AS frequency
           FROM acts WHERE act_type_id = "PA"
           GROUP BY 1) AS p
ON c.id = p.contact_id
ORDER BY c.id;




