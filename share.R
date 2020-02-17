# shared functions

# operation id
UPDATEID = 3
INSERTID = 2
DELETEID = 4

killDbConnections <- function () {
  
  all_cons <- dbListConnections(RMySQL::MySQL())
  
  if (length(all_cons) > 0) {
    # print(all_cons)
    for(con in all_cons) {
      dbDisconnect(con)
    }
    
    # print(paste(length(all_cons), " connections killed."))
  }
  
}

get_select_list <- function(con, table, key_col, value_col) {
  sel_tbl <- as_tibble(DBI::dbReadTable(con, table))
  sel_lst <- as.list(sel_tbl %>% pull(value_col))
  names(sel_lst) <- sel_tbl %>% pull(key_col)
  return(sel_lst)
}

get_table_id <- function(con, table) {
  # get table id using table name
  table_sql <- paste("SELECT `id` FROM `Table` WHERE `name` = '", table, "'", sep = "")
  table_rv <- dbGetQuery(con, table_sql)
  if (nrow(table_rv) == 0) {
    stop("Table not found!")
  } else {
    return(table_rv$id[1])
  }
}

