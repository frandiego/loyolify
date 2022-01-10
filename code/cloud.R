## cloud.R
## author: Francisco Jose Diego Acosta


set_auth <- function(cnf){
  googleCloudStorageR::gcs_auth(json_file = cnf$deploy$credentials)
}

ls_cloud <- function(cnf){
  cnf$deploy$bucket %>% gcs_list_objects(bucket = .) %>% .[['name']] %>% unique()
}

ls_local <- function(cnf){
  c(cnf$tidy$output_path, cnf$preprocess$output_path) %>% 
    map(.,~list.files(., full.names = T)) %>% 
    unlist() %>%
    unique()
}

ls_clean <- function(v, prefix='data', folders=c('prep', 'tidy'), extension='RDS'){
  v %>% 
    .[grepl(paste0('^', prefix), .)] %>% 
    .[grepl(paste0(folders, collapse = '|'), .)] %>% 
    .[grepl(paste0( extension, '$'), .)] %>% 
    unique()
}

list_files <- function(cnf){
  list(
    local = ls_local(cnf) %>% ls_clean(), 
    cloud = ls_cloud(cnf) %>% ls_clean()
  )
}


sync <- function(cnf){
  set_auth(cnf)
  files = list_files(cnf)
  
  
  setdiff(files$local, files$cloud) %>% 
    walk(~gcs_upload(file=., bucket=cnf$deploy$bucket, name=.))
  
  setdiff(files$cloud, files$local) %>% 
    walk(~gcs_get_object(object_name=., bucket=cnf$deploy$bucket, saveToDisk=.))
  
  
}




upload_file <- function(cnf, file){
  gcs_upload(file = file,
             bucket = cnf$deploy$bucket,
             name = gsub('^./', '', file))
}

download_file <- function(cnf, file){
  gcs_get_object(object_name = gsub('^./', '', file),
                 bucket = cnf$deploy$bucket,
                 saveToDisk = file, 
                 overwrite = T)
}



upload_credentials <- function(cnf){
  library(RSQLite)
  csv_file = gsub('.sqlite', '.csv', cnf$deploy$database)
  cnf$deploy$database %>% dbConnect(SQLite(), dbname = .) -> conn
  shinymanager::read_db_decrypt(conn, name='credentials', 
                                passphrase = cnf$deploy$passphrase) %>% 
    as.data.table() %>% 
    fwrite(csv_file)
  upload_file(cnf, csv_file)
  file.remove(csv_file)
}

download_credentials <- function(cnf){
  library(RSQLite)
  csv_file =gsub('.sqlite', '.csv', cnf$deploy$database)
  download_file(cnf, csv_file)
  df = fread(csv_file)
  cnf$deploy$database %>% dbConnect(SQLite(), dbname = .) -> conn
  shinymanager::write_db_encrypt(conn, value = df, 
                                 name='credentials', 
                                 passphrase = cnf$deploy$passphrase)
  
}

sync_credentials <- function(cnf){
  upload_credentials(cnf)
  download_credentials(cnf)
}
