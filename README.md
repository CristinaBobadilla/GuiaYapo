# GuiaYapo
## En el presente documento se mostrarán los gráficos creados en R con los datos de 3 días recopilados de la página yapo.cl, así como también sus respectivos códigos y una breve descripción. 

# GRÁFICOS
*Para la realización de los gráficos en R se han utilizado los paquetes tidyverse y ggplot2 y se ha ocupado el archivo .csv que contiene la recopilación de los datos de yapo*

•	Gráfico Cantidad de avisos

![grafico1](https://github.com/CristinaBobadilla/GuiaYapo/blob/main/graficotipoaviso1.png)

*En el primer gráfico se refleja la cantidad de avisos según el tipo de aviso, y se aprecia que la gran mayoría de estos son relacionado a las ventas y una mínima parte de las publicaciones de yapo no especifican su tipo de aviso.*
*El gráfico se ha hecho con los códigos:*

productosYapo <- productosYapo %>% mutate( TipoAviso = ifelse( tipo == "Vendo", "Venta", "Servicios") )

tipoaviso <- group_by(productosYapo, TipoAviso) %>% summarise(cantidad = n())

ggplot(tipoaviso, aes(x="", y=cantidad, fill=TipoAviso)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)

*Se genera una nueva columna basada en el campo tipoaviso donde se clasifican los avisos en venta de producto y oferta de servicios. Luego se genera un dataset con la cantidad de avisos por cada una de las categorías y posteriormente se grafica con ggplot.*
