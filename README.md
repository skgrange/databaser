# **databaser**

[![Build Status](https://travis-ci.org/skgrange/databaser.svg?branch=master)](https://travis-ci.org/skgrange/databaser)

**databaser** is an R package which extends **DBI**, **RSQLite**, **RMySQL**, and **RPostgreSQL** packages and keeps things consistent with functions starting with `db_`. **databaser** contains many simple wrappers which are sometimes undesirable, but they are included for consistency. 

  - Functions include: 
    - `db_connect`
    - `db_send` (vectorised)
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

**databaser** started off life within the [**threadr**](https://github.com/skgrange/threadr) package. 
