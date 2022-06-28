
# Funci?n 'myReadData_byDate'
#
#	Lee datos de un fichero csv de nombre 'file'.
#	Devuelve un vector que contiene los datos de la columna especificada por la etiqueta 'column'
#	desde la fecha inicial 'date_ini' hasta la fecha final 'date_fin'.
# 	
# Argumentos:
#	- file: Cadena de caracteres (string) especificando el nombre del fichero.
#	- date_ini: Cadena de caracteres (string) especificando la fecha inicial. Formato dd/mm/aaa.
#	- date_fin: Cadena de caracteres (string) especificando la fecha final. Formato dd/mm/aaa.
#	- column: Cadena de caracteres (string) especificando la columna (etiqueta o cabecera) que se desea leer.
#
# A?adir la siguiente linea para hacer llamadas a esta funci?n desde otros ficheros:
#
#	source('Lectura_datos_por_fecha.R')
#

myReadData_byDate = function(file, date_ini, date_fin, column){
	
	df = read.csv(file, sep = ',', row.names = 1)
	idx_Dates = as.character( seq(as.Date(date_ini, format = '%d/%m/%Y'), as.Date(date_fin, format = '%d/%m/%Y'), 'days') )
	return( na.omit(df[idx_Dates, column]) )
}


############## Ejemplo de uso ##############

# Leyendo datos de casos nuevos desde el 1 de marzo de 2021 al 31 de marzo de 2021 (1 mes).
data = myReadData_byDate('WHO-COVID-19-global-data-SPAIN.csv', '01/03/2021', '31/03/2021', 'New_cases')

# Imprimir los primeros y últimos datos
print(head(data))
print(tail(data))
