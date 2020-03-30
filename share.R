# shared functions
library(DBI)
library(tibble)

# operation id
UPDATEID = 3
INSERTID = 2
DELETEID = 4
UPLOADID = 5
DOWNLOADID = 6

dbargs <- list(
  drv = RMySQL::MySQL(),
  dbname = "BEAGLE_LIMS",
  host = "192.168.253.178",
  username = "lims",
  password = "lims"
)

limsdbExecute <- function(querysql) {
  conn <- do.call(DBI::dbConnect, dbargs)
  dbSendQuery(conn,"SET NAMES utf8")
  on.exit(DBI::dbDisconnect(conn))
  
  dbExecute(conn, querysql)
}

limsdbGetQuery <- function(querysql) {
  conn <- do.call(DBI::dbConnect, dbargs)
  dbSendQuery(conn,"SET NAMES utf8")
  on.exit(DBI::dbDisconnect(conn))
  
  dbGetQuery(conn, querysql)
}

limsdbReadTable <- function(name) {
  conn <- do.call(DBI::dbConnect, dbargs)
  dbSendQuery(conn,"SET NAMES utf8")
  on.exit(DBI::dbDisconnect(conn))
  
  as.tibble(dbReadTable(conn, name))
}

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

get_select_list <- function(table, key_col, value_col) {
  sel_tbl <- limsdbReadTable(table)
  sel_lst <- as.list(sel_tbl %>% pull(value_col))
  names(sel_lst) <- sel_tbl %>% pull(key_col)
  return(sel_lst)
}

get_user_list <- function(table = "`User`", key_col = "name", value_col = "userKey") {
  sel_tbl <- limsdbReadTable(table)
  sel_lst <- as.list(sel_tbl %>% filter(isValid == 1) %>% pull(value_col))
  names(sel_lst) <- sel_tbl %>% filter(isValid == 1) %>% pull(key_col)
  return(sel_lst)
}

get_table_id <- function(table) {
  # get table id using table name
  table_sql <- paste("SELECT `id` FROM `Table` WHERE `name` = '", table, "'", sep = "")
  table_rv <- limsdbGetQuery(table_sql)
  if (nrow(table_rv) == 0) {
    stop("Table not found!")
  } else {
    return(table_rv$id[1])
  }
}

label_query <- function(specNum, returnKey = "source") {
  # get source info from label table using specimen number
  lbl_tbl <- limsdbReadTable("Label")
  lbl_tbl %>% filter(`startNumber` <= as.integer(specNum), `startNumber` + `serialLength` > as.integer(specNum)) %>% pull(returnKey)
}

observeSample <- function(testNumber, date, experimentNumber) {
  o_sql <- paste("SELECT *
                 FROM `Run`
                 WHERE testNumber = '", testNumber, "'
                 AND   date = '", date, "'
                 AND   experimentNumber = '", experimentNumber, "'", sep = "")
  limsdbGetQuery(o_sql)
}

get_log <- function(table) {
  log_sql <- paste("SELECT u.name name, o.chinese operation, t.chinese `table`, recordId, `timestamp` `time`
  FROM Log l
  LEFT JOIN `User` u on u.userKey = l.userKey
  LEFT JOIN `Operation` o on l.operationId = o.id
  LEFT JOIN `Table` t on l.tableId = t.id
  WHERE t.name = '", table, "'
  ORDER BY `timestamp` DESC", sep = "")
  
  log_tbl <- limsdbGetQuery(log_sql)
  
  if (nrow(log_tbl) > 0) {
    paste(log_tbl$name, "于", log_tbl$time, "在", log_tbl$table, "中", log_tbl$operation, "了第", log_tbl$recordId,"号记录", sep = "")
  } else {
    return("无操作记录")
  }
}

