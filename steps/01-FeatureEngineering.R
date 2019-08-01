# Aplicando Engenharia de Atributos em Variaveis Numericas

# --------------------------------------------------------------------
# Transformar o objeto de data
df$date <- strptime(as.character(df$date),format="%Y-%m-%d %H:%M")
df$date <- as.POSIXct(df$date , tz="UTC")

# Extraindo ano, mes, dia, hora e minuto do campo data
df$day   <- as.integer(format(df$date, "%d"))
df$month <- as.factor(format(df$date, "%m"))
df$hour <- as.integer(format(df$date, "%H"))

# Transformando variáveis numéricas em variáveis categóricas
df$lights <- as.factor(df$lights)

