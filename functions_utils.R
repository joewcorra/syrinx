# Query the DuckDB File

con <- dbConnect(duckdb(), dbdir = "path/to/user_data.duckdb", read_only = FALSE)

df <- dbReadTable(con, "energy")  # OR:
tbl(con, "energy") %>% head()

dbDisconnect(con, shutdown = TRUE)

# 
# Use collect() only when you really need the data in memory
# 
# Avoid writing back to the same .duckdb file from multiple processes (i.e., avoid concurrent writes)
# 
# Each targets pipeline could optionally generate its own intermediate .duckdb file if modularity is important
# 
# 🚨 Caution: Path Consistency
# Because the .duckdb file is being generated in data-fetch/output/user_data.duckdb, you'll want your other repos to know where to find it. You can:
# 
# Standardize the location (e.g., always reference ../data-fetch/output/user_data.duckdb).
# 
# Use a config file (config.yml) shared across projects.

Use environment variables or a renv profile to track it dynamically.