# **databaser**  <a href='https://github.com/skgrange/databaser'><img src='man/figures/logo.png' align="right" height="131.5" /></a>

[![Lifecycle Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/)

![](inst/extdata/images/icon_small.png)

**databaser** is an R package which extends [**DBI**](https://github.com/rstats-db/DBI), [**RSQLite**](https://github.com/r-dbi/RSQLite), [**RMariaDB**](https://github.com/r-dbi/RMariaDB), and [**RPostgreSQL**](https://code.google.com/archive/p/rpostgresql/). **databaser**  keeps things consistent with functions starting with `db_`. **databaser** contains many simple wrappers which are sometimes undesirable, but they are included for consistency. 

  - Functions include: 
  
    - `db_connect`
    - `db_execute` (vectorised)
    - `db_get`
    - `db_insert`
    - `db_contents`
    - `db_count_rows`
    - `db_head`
    - `db_list_variables`
    - `db_vacuum`
    - `db_read_table`
    - `db_insert`
    - `db_use_sql`
    - `db_kill_process`
    - `db_list_constraints`
    - `db_list_indices` 
    - `db_size`
    - `db_version`
    - `read_sql`
    - and many more...

**databaser** started off life within the [**threadr**](https://github.com/skgrange/threadr) package. 
