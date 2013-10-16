library(RPostgreSQL)

driver <- dbDriver("PostgreSQL")
channel <- dbConnect(driver, dbname="rhodium", user="Ben", password="950dogwood")

# create grid
fraction <- 6 # change this if desired

min.x <- -180
max.x <- 180.0029
min.y <- -58
max.y <- 85.00114
ncols <- (max.x-min.x) %/% (1/fraction)
nrows <- (max.y-min.y) %/% (1/fraction)
dbSendQuery(channel, "DROP TABLE IF EXISTS grid")
dbSendQuery(channel, paste("CREATE TABLE grid AS SELECT * FROM generate_series(0,", ncols, ") AS col, generate_series(0,", nrows, ") AS row", sep= " "))

# create unique id
dbSendQuery(channel, "ALTER TABLE grid ADD COLUMN id integer")
dbSendQuery(channel, "UPDATE grid SET id=1000000*col+row")

# create geometry
dbSendQuery(channel, "ALTER TABLE grid ADD COLUMN long double precision")
dbSendQuery(channel, sprintf("UPDATE grid SET long=%f+1/(2*%f) + col*(1/%f)", min.x, fraction, fraction))
dbSendQuery(channel, "ALTER TABLE grid ADD COLUMN lat double precision")
dbSendQuery(channel, sprintf("UPDATE grid SET lat=%f+1/(2*%f) + row*(1/%f)", min.y, fraction, fraction))
dbGetQuery(channel, "SELECT addGeometryColumn('grid', 'centroid', 4326, 'POINT', 2)")
dbSendQuery(channel, "UPDATE grid SET centroid=ST_SetSRID(ST_MakePoint(long, lat), 4326)")
dbGetQuery(channel, "SELECT addGeometryColumn('grid', 'cell', 4326, 'POLYGON', 2)")
dbSendQuery(channel, sprintf("UPDATE grid SET cell=ST_SetSRID(ST_MakePolygon(ST_MakeLine(ARRAY[ST_MakePoint(long-1/(2*%1$f), lat-1/(2*%1$f)), ST_MakePoint(long-1/(2*%1$f), lat+1/(2*%1$f)), ST_MakePoint(long+1/(2*%1$f), lat+1/(2*%1$f)), ST_MakePoint(long+1/(2*%1$f), lat-1/(2*%1$f)), ST_MakePoint(long-1/(2*%1$f), lat-1/(2*%1$f))])), 4326)", fraction))
dbSendQuery(channel, "CREATE INDEX grid_centroid_index ON grid USING GIST (centroid)")
dbSendQuery(channel, "CREATE INDEX grid_cell_index ON grid USING GIST (cell)")
dbSendQuery(channel, "CREATE INDEX grid_id_index ON grid (id)")
dbSendQuery(channel, "CREATE INDEX grid_row_index ON grid (row)")
dbSendQuery(channel, "CREATE INDEX grid_col_index ON grid (col)")
dbSendQuery(channel, sprintf("DELETE FROM grid WHERE NOT id IN (SELECT id FROM grid, WORLD_adm0 WHERE ST_Within(grid.centroid, WORLD_adm0.the_geom))"))