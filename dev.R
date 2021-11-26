rm(list = ls())
cnf = yaml::read_yaml('config.yml')
sapply(list.files('code', recursive = T, full.names = T), source)
tidy(list.files('data/raw', full.names = T), cnf)
df = preprocess(cnf)

df[, curso := take_integer(curso)]
df[, genero := take_words(genero, c('mujer','hombre'), 'otro')]
df[, zurdo := take_words(genero, c('derecha','izquierda'), NA)]

