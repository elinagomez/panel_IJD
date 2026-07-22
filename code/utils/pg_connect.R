pg_connect <- function(drv = RPostgres::Postgres(),
                       name_db = Sys.getenv("NAME_DB"),
                       host_db = Sys.getenv("HOST_DB"),
                       port_db = '5432',
                       user_db = Sys.getenv("USER_DB"),
                       pass_db = Sys.getenv("PASS_DB")){
  
  return(
    DBI::dbConnect(drv = drv, 
                   dbname = name_db, 
                   host = host_db,
                   port = port_db,
                   user = user_db, 
                   password = pass_db)  
  )  
}

