## preprocess.R
## author: Francisco Jose Diego Acosta


get_filenames <- function(path) {
  ls <- list.files(gsub("/$", "", path))
  ls_split <- strsplit(ls, "_")
  time <- map_chr(ls_split, ~ .[1])
  base <- map_chr(ls_split, ~ paste0(.[-1], collapse = "_"))
  data.table::data.table(filename = ls, time = time, base = base) -> dfile
  dfile[, max_time := max(time), by = base]
  unique(dfile[max_time == time][["filename"]])
}


preprocess_read_tidy <- function(path) {
  path %>%
    readRDS() %>%
    data.table::as.data.table() %>%
    setnames(names(.), map_chr(names(.), clean_colname))
}


preprocess_filter <- function(cnf) {
  columns <- cnf$preprocess$columns
  path <- cnf$tidy$output_path
  path %>% list.files(full.names = T, pattern = ".RDS") -> ls
  ls %>% map(preprocess_read_tidy) -> dtl
  ls %>% map_chr(~ basename(.) %>%
    strsplit("_") %>%
    unlist() %>%
    head(1)) -> times
  map2(.x = dtl, .y = times, .f = function(x, y) x[, timestamp := y]) -> dtl
  dtl %>%
    data.table::rbindlist(fill = T) %>%
    .[, c(columns, "timestamp"), with = F] -> df
  df[, timestamp := as.double(timestamp)]
  df[, max_timestamp := max(timestamp), by = .(alumno_id, estudio, curso, grupo)]
  df[timestamp == max_timestamp, c(columns), with = F] %>% unique()
  return(list(data = df, files = ls))
}

preprocess_variables <- function(df) {
  l <- list()
  l[["actitudes"]] <- create_section_actitudes(df)
  l[["cognitivas"]] <- create_section_cognitivas(df)
  l[["social"]] <- create_section_social(df)
  l[["salud"]] <- create_section_salud(df)
  l %>%
    rbindlist() %>%
    unique() %>%
    setnames("alumno_id", "uid") %>%
    .[]
}


preprocess_group <- function(df) {
  vars <- c("alumno_id", "estudio", "curso", "grupo", "genero", "zurdo")
  dt <- df[, vars, with = F]
  dt[, uid := as.integer(alumno_id)]
  dt[, school := as.integer(estudio)]
  dt[, course := as.integer(gsub("[^0-9]", "", curso))]
  dt[, group := as.integer(factor(grupo, levels = letters))]
  dt[, gender := ifelse(genero == "mujer", 1, 0)]
  dt[, is_left_handed := ifelse(zurdo %in% c("si", "izquierda"), 1,
    ifelse(zurdo %in% c("no", "derecha"), 0, NA)
  )]
  dt[, -c(vars), with = F] %>% unique()
}


stats_mode <- function(x) {
  names(head(sort(table(x), T), 1))
}


find_repetidor <- function(df) {
  dft <- df[, c("alumno_id", "curso", "grupo", "estudio", "ano_nacimiento"), with = F]
  dft[, curso := as.integer(gsub("[^0-9]", "", curso))]
  dft[, grupo := tolower(trimws(grupo))]
  dft[, estudio := as.integer(estudio)]
  dft <- unique(dft)
  dft[, .(ano_nacimiento_mode = as.numeric(stats_mode(ano_nacimiento))),
    by = .(grupo, curso, estudio)
  ] -> df_ano_nacimiento_mode

  dft <- merge(dft, df_ano_nacimiento_mode, by = c("grupo", "curso", "estudio"), all.x = T)
  dft[, repetidor := as.integer(ano_nacimiento > 2000 & ano_nacimiento < ano_nacimiento_mode)]
  dft[is.na(repetidor), repetidor := 0]
  dft[, c("alumno_id", "repetidor"), with = F] %>%
    unique() %>%
    setnames(names(.), c("uid", "is_repeater")) %>%
    .[]
}


preprocess_read_last <- function(cnf) {
  cnf$preprocess$output_path %>%
    list.files() %>%
    map_dbl(~ strsplit(., "_") %>%
      unlist() %>%
      head(1) %>%
      as.double()) %>%
    .[!is.na(.)] %>%
    which.max() %>%
    list.files(cnf$preprocess$output_path, full.names = 1)[.] %>%
    readRDS() %>%
    unique() %>%
    as.data.table()
}

preprocess_new_files <- function(cnf) {
  cnf$preprocess$output_path %>%
    file.path("files.RDS") %>%
    readRDS() %>%
    .[["file"]] %>%
    unique() %>%
    setdiff(list.files(cnf$tidy$output_path, full.names = T), .)
}



preprocess_data <- function(df) {
  dfvars <- preprocess_variables(df)
  dfgroups <- preprocess_group(df)
  dfpopu <- dfvars[variable == "relacional", .(is_popular = as.integer(y != 0)), by = .(uid)]
  dfgroups <- dfgroups %>% merge(dfpopu, by = "uid", all.x = T)
  df_rep <- find_repetidor(df)
  dfgroups <- dfgroups %>% merge(df_rep, by = "uid", all.x = T)
  chars <- setdiff(names(dfgroups), c("uid", "school", "course", "group"))
  dfvars <- unique(dfvars[!is.na(y)])
  dfgroups <- unique(dfgroups[uid %in% unique(dfvars[, uid])])
  dfgroups[, n_value := dfgroups[, c(chars), with = F] %>% reduce(`+`)]
  cols <- unique(c(colnames(dfgroups), colnames(dfvars)))
  dfgroups[, n_na := dfgroups %>% map(is.na) %>% reduce(`+`)]
  dfgroups[, max_value := max(n_value, na.rm = T), by = .(uid)]
  dfgroups[, min_n_na := min(n_na, na.rm = T), by = .(uid)]
  dfgroups <- dfgroups[n_na == min_n_na]
  dfgroups <- dfgroups[n_value == max_value]
  dfgroups[, n_uid := .N, by = .(uid)]
  dfgroups[, nuid := 1:.N, by = .(uid)]
  dfgroups <- dfgroups[nuid == 1]

  dfvars %>%
    merge(dfgroups, by = c("uid"), all.x = T) %>%
    .[, c(cols), with = F] %>%
    unique() -> dfprep
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  dfprep[, timestamp := timestamp]
  return(dfprep)
}

save_files <- function(ls_new) {
  cnf$preprocess$output_path %>%
    file.path("files.RDS") %>%
    readRDS() %>%
    list(., data.table(file = ls_new)) %>%
    rbindlist() %>%
    unique() %>%
    saveRDS(file.path(cnf$preprocess$output_path, "files.RDS"))
}





preprocess <- function(cnf, get_data = F) {
  df <- preprocess_read_last(cnf)
  ls_new <- preprocess_new_files(cnf)
  df_new <- ls_new %>%
    map(readRDS) %>%
    rbindlist(fill = T)
  if (nrow(df_new) > 1) {
    df_new_prep <- preprocess_data(df_new)
    df <- rbindlist(list(df, df_new_prep), use.names = T, fill = T)
    df[is.na(timestamp), timestamp := 0]
    df[, c("max_ts", "nuid") := list(max(timestamp, na.rm = T), .N), by = .(uid, school, course, group)]
    df <- df[timestamp == max_ts]
    save_files(ls_new)
    df <- unique(df[, -c("max_ts", "nuid"), with = F])
    filename <- paste0(df[, max(timestamp)], "_prep", ".RDS")
    saveRDS(df, file.path(cnf$preprocess$output_path, filename))
  }
  if (get_data) {
    return(df)
  }
}

last_filepath <- function(cnf) {
  cnf$preprocess$output_path %>%
    lslatr() %>%
    head(1) %>%
    .[["rn"]]
}


filepath <- function(cnf, filename = NULL) {
  if (!is.null(filename)) {
    if (filename != "") {
      filename %>%
        file.path(cnf$preprocess, paste0(., ".RDS"))
    } else {
      last_filepath(cnf)
    }
  } else {
    last_filepath(cnf)
  }
}



tidy_and_process <- function(input_files, cnf) {
  tidy(input_files = input_files, cnf = cnf)
  preprocess(cnf)
}

tidy_and_process_safe <- purrr::safely(tidy_and_process)


preprocess_paths <- function(paths, dataset_name, cnf) {
  paths %>%
    map(readRDS) %>%
    rbindlist(fill = T) %>%
    preprocess_data() %>%
    saveRDS(file.path(cnf$preprocess$output_path, paste0(dataset_name, ".RDS")))
}

preprocess_paths_safe <- purrr::safely(preprocess_paths)



read_prep_file <- function(file, cnf) {
  cnf$preprocess$output_path %>%
    file.path(., file) %>%
    readRDS() %>%
    .[!is.na(school)] %>%
    .[]
}
