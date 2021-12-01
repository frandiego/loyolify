rm(list = ls())
cnf = yaml::read_yaml('config.yml')
sapply(list.files('code', recursive = T, full.names = T), source)
# tidy(list.files('data/raw', full.names = T), cnf)
# preprocess(cnf)
df = read_data(cnf)

filter = list(variable='autocontrol', group=c(1,2))

plot(df, filter)
