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



•	Grafico cantidad de tipos de moneda

![grafico2](https://github.com/CristinaBobadilla/GuiaYapo/blob/main/graficotipomoneda.png)

*En el segundo gráfico se ve la cantidad de ventas clasificadas según el tipo de moneda que se va usar, la mayoría corresponde a pesos chilenos, también se puede ver que hay pocos avisos con tipo de moneda UF y una gran cantidad de productos que no especifican el precio.*
*Este gráfico se ha hecho con los códigos:*

tipomoneda <- group_by(productosYapo, moneda) %>% summarise(cantidad = n())

ggplot(tipomoneda, aes(x="", y=cantidad, fill=moneda)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)

*En el cual se crea un dataset llamado “tipomoneda” donde se nos hace un recuento de las ventas según su tipo de moneda ya sea $ (peso chileno), UF o NA (sin escpecificar)
Y con el comando de ggplot usamos la variable tipomoneda.*



•	Gráfico cantidad de ventas por comuna

![grafico3](https://github.com/CristinaBobadilla/GuiaYapo/blob/main/graficocomunas.png)

*En el tercer gráfico se ve la cantidad de ventas según las comunas de la región metropolitana. Se aprecia que la región donde hay más avisos de ventas es en la comuna de Santiago, y las comunas que tienen menos ventas son Tiltil, San Pedro, San José de Maipo, María Pinto y Alhué.*
*El gráfico se ha hecho con los siguientes códigos:*

productosYapo <- productosYapo %>% mutate( Comuna = gsub("Región Metropolitana, ", "", region) )
comuna <- group_by(productosYapo, Comuna) %>% summarise(cantidad = n())
ggplot(data=comuna, aes(x=Comuna, y=cantidad, fill=Comuna)) + geom_bar(stat="identity") + coord_flip()

*Se genera una nueva columna de datos con el nombre de la comuna, posteriormente se crea un dataset con el recuento de avisos por comuna.*



•	Gráfico de cantidad de ventas según rango de precios

![grafico4](https://github.com/CristinaBobadilla/GuiaYapo/blob/main/rangopreciosgrafico.png)

*El último gráfico está basado en la cantidad de ventas según el rango de precios, se aprecia que la gran mayoría de productos que se venden en la página de yapo.cl tienen un valor entre 100.000 y 1.000.000 pesos chilenos, también se puede ver que hay poca cantidad de productos que tienen un valor que no supera los 5.000 de pesos chilenos y una cantidad considerable que no especifican el precio.* 
*El gráfico se ha hecho con los siguientes códigos:*

productosYapo <- productosYapo %>% mutate( RangoPrecio = case_when( ValorPeso  < 5000 ~ "< 5.000", ValorPeso < 10000 ~ "< 10.000" , ValorPeso < 50000 ~ "< 50.000" , ValorPeso < 100000 ~ "< 100.000", ValorPeso < 1000000 ~ "< 1.000.000", ValorPeso >= 1000000 ~ ">= 1.000.000")  )

rangoprecio <- group_by(productosYapo, RangoPrecio) %>% summarise(cantidad = n())
ggplot(data=rangoprecio, aes(x=RangoPrecio, y=cantidad, fill=RangoPrecio)) + geom_bar(stat="identity") + coord_flip()

*Primero se genera una nueva columna de datos con el rango de precio donde se encuentran los productos, posteriormente se crea un dataset con el recuento de los rangos, y finalmente se grafica con ggplot.*















