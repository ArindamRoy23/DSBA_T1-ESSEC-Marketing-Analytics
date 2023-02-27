Do not execute this line...;


# Use the charity database
USE ma_charity;


# Clean tables for the exercise
DROP TABLE IF EXISTS periods;
DROP TABLE IF EXISTS segments;


# We're going to divide the past in periods
# Create a table to store period information
CREATE TABLE periods (
  period_id INTEGER NOT NULL,
  first_day DATE NOT NULL,
  last_day DATE NOT NULL,
  PRIMARY KEY (period_id)
);

# Define 15 periods (or more)
# Period 0 = the most recent ("today")
INSERT INTO periods
VALUES ( 0, 20210601, 20220530),
       ( 1, 20200601, 20210530),
       ( 2, 20190601, 20200530),
       ( 3, 20180601, 20190530),
       ( 4, 20170601, 20180530),
       ( 5, 20160601, 20170530),
       ( 6, 20150601, 20160530),
       ( 7, 20140601, 20150530),
       ( 8, 20130601, 20140530),
       ( 9, 20120601, 20130530),
       (10, 20110601, 20120530),
       (11, 20100601, 20110530),
       (12, 20090601, 20100530),
       (13, 20080601, 20090530),
       (14, 20070601, 20080530);


# Create a segment table
# It will store to which segment each donor belonged
# in each period
CREATE TABLE segments (
  sq INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,
  contact_id INTEGER UNSIGNED NOT NULL,
  period_id INTEGER NOT NULL,
  segment VARCHAR(6),
  PRIMARY KEY (sq),
  INDEX idx_contact_id(contact_id),
  INDEX idx_period_id(period_id)
);


# This will create a placeholder for all
# contact-by-period possible combinations
INSERT INTO segments (contact_id, period_id)
SELECT a.contact_id, p.period_id
FROM acts a,
     periods p
GROUP BY 1, 2;


# Create the AUTO segment
# You may require to remove the "safe mode" in MySQL Workbench
# Edit > Preferences > SQL Editor > Uncheck "Safe Updates"
UPDATE
  segments s,
  (SELECT contact_id, period_id
   FROM   acts a, periods p
   WHERE  (a.act_date <= p.last_day) AND
          (a.act_date >= p.first_day) AND
          (a.act_type_id LIKE 'PA')
   GROUP BY 1, 2) AS d
SET
  s.segment = "AUTO"
WHERE
  (s.contact_id = d.contact_id) AND
  (s.period_id = d.period_id);


# Create the NEW segment
UPDATE
  segments s,
  (SELECT contact_id, period_id
   FROM periods p,
        (SELECT contact_id, MIN(act_date) AS first_act
         FROM acts
         GROUP BY 1) AS f
   WHERE (f.first_act <= p.last_day) AND
         (f.first_act >= p.first_day)) AS d
SET
  s.segment = "NEW"
WHERE
  (s.contact_id = d.contact_id) AND
  (s.period_id = d.period_id) AND
  (s.segment IS NULL);


# Create the BOTTOM/UP segment
UPDATE
  segments s,
  (SELECT contact_id, period_id, SUM(amount) AS generosity
   FROM   acts a, periods p
   WHERE  (a.act_date <= p.last_day) AND
          (a.act_date >= p.first_day) AND
          (a.act_type_id LIKE 'DO')
   GROUP BY 1, 2) AS d
SET
  s.segment = IF(generosity < 100, "BOTTOM", "TOP")
WHERE
  (s.contact_id = d.contact_id) AND
  (s.period_id = d.period_id) AND
  (s.segment IS NULL);


# Create the WARM segment
UPDATE
  segments s,
  (SELECT contact_id, period_id
   FROM   segments
   WHERE  (segment LIKE "NEW")    OR
          (segment LIKE "AUTO")   OR
          (segment LIKE "BOTTOM") OR
          (segment LIKE "TOP")) AS a
SET
  s.segment = "WARM"
WHERE
  (s.contact_id = a.contact_id) AND
  (s.period_id = a.period_id - 1) AND
  (s.segment IS NULL);


# Create the COLD segment
UPDATE
  segments s,
  (SELECT contact_id, period_id
   FROM   segments
   WHERE  Segment LIKE "WARM") AS a
SET
  s.segment = "COLD"
WHERE
  (s.contact_id = a.contact_id) AND
  (s.period_id = a.period_id - 1) AND
  (s.segment IS NULL);


# Create the LOST segment
UPDATE
  segments s,
  (SELECT contact_id, period_id
   FROM   segments
   WHERE  segment LIKE "COLD") AS a
SET
  s.segment = "LOST"
WHERE
  (s.contact_id = a.contact_id) AND
  (s.period_id  < a.period_id)  AND
  (s.segment IS NULL);


# Count segment members per period
SELECT period_id, Segment, COUNT(*)
FROM segments
GROUP BY 1, 2
ORDER BY 2, 1 DESC;


# In which segments were donors last period,
# and where are they now?
SELECT old.segment, new.segment, COUNT(new.segment)
FROM segments old,
     segments new
WHERE (old.contact_id = new.contact_id) AND
      (old.period_id = 1) AND
      (new.period_id = 0)
GROUP BY 1, 2
ORDER BY 1, 2;


# Report the financial contribution of each segment
SELECT
  s.segment,
  COUNT(DISTINCT(s.contact_id)) AS 'numdonors',
  COUNT(a.amount)               AS 'numdonations',
  CEILING(AVG(a.amount))        AS 'avgamount',
  CEILING(SUM(a.amount))        AS 'totalgenerosity'
FROM
  segments s,
  periods p,
  acts a
WHERE
  (s.contact_id = a.contact_id) AND
  (s.period_id = 0) AND
  (p.period_id = 0) AND
  (a.act_date >= p.first_day) AND
  (a.act_date <= p.last_day)
GROUP BY 1
ORDER BY totalgenerosity DESC;


# Report the financial contribution in "period 0"
# (last 12 months) of each segment in period 1 (a year before)
SELECT
  s.segment,
  COUNT(DISTINCT(s.contact_id)) AS 'numdonors',
  COUNT(a.amount)               AS 'numdonations',
  CEILING(AVG(a.amount))        AS 'avgamount',
  CEILING(SUM(a.amount))        AS 'totalgenerosity'
FROM
  segments s,
  periods p,
  acts a
WHERE
  (s.contact_id = a.contact_id) AND
  (s.period_id = 1) AND
  (p.period_id = 0) AND
  (a.act_date >= p.first_day) AND
  (a.act_date <= p.last_day)
GROUP BY 1
ORDER BY totalgenerosity DESC;