## tidy.R
## author: Francisco Jose Diego Acosta


clean_string <- function(x) {
    gsub("[^a-z0-9| ]", "", tolower(x))
}


clean_column <- function(x, time_fraction = 0.5) {
    if (sum(grepl("->", x))/length(x) >= time_fraction) {
        unlist(lapply(strsplit(as.character(x), "->"), function(x) ifelse(length(x) < 2, "", clean_string(rev(x)[1]))))
    } else {
        clean_string(x)
    }

}


as_factor <- function(x, cardinality_ratio = 0.75) {
    if (data.table::uniqueN(x)/length(x) <= cardinality_ratio) {
        lv = sort(unique(tolower(x)))
        factor(tolower(x), lv[lv != ""], ordered = T)
    } else {
        NA
    }
}


guess_column <- function(x, funs = c(as.integer, as.numeric, as_factor, as.character), out = c("", NA)) {
    x_ = clean_column(x)
    for (f in funs) {
        if (!any(is.na(f(x_[!x_ %in% out])))) {
            return(f(x_))
        }
    }
}


tidy <- function(input_file, output_path) {
    df = readxl::read_excel(input_file, skip = 1)
    df = data.table::as.data.table(df)
    columns = names(df)[which(gsub("[^a-z]", "", tolower(names(df))) != "")]
    df = df[, `:=`(c(columns), lapply(.SD, guess_column)), .SDcols = c(columns)]
    df = df[, c(columns), with = F]
    saveRDS(df, file.path(output_path, paste0(format(Sys.time(), "%Y%m%d%H%M%S"), ".RDS")))
    df[]
}


