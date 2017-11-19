library(RPostgreSQL)
#library(DBI)
query <- "SELECT * FROM companias_15092016 where EXPEDIENTE '%1790500748001%')"

db <- dbConnect(drv=dbDriver("PostgreSQL"), 
                dbname = "SC",
                host = "localhost",port = 5432, 
                user = "postgres",
                password = "sintiempo")
#query <- sqlInterpolate(db, sql, id1=busq)
dbGetQuery(db, query)
dbDisconnect(db)
datos <- do.call(rbind, datos)
datos

