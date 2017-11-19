library(DBI)
library(RPostgreSQL)
db <- dbConnect(drv=dbDriver("PostgreSQL"), 
                dbname = "SC",
                host = "localhost",port = 5432, 
                user = "postgres",
                password = "sintiempo")
query <- "SELECT * FROM companias_15092016 LIMIT 5"
#query <- "SELECT * FROM companias_15092016 WHERE RUC IN ('1790008959001')"
dbGetQuery(db, query)

dbDisconnect(db)

