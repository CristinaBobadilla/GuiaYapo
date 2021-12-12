library(ggplot2)
library(tidyverse)

productosYapo <- productosYapo %>% mutate( TipoAviso = ifelse( tipo == "Vendo", "Venta", "Servicios") )

productosYapo <- productosYapo %>% mutate( Comuna = gsub("Región Metropolitana, ", "", region) )


#grafico por tipo de aviso
tipoaviso <- group_by(productosYapo, TipoAviso) %>% summarise(cantidad = n())


ggplot(tipoaviso, aes(x="", y=cantidad, fill=TipoAviso)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)

#grafico por moneda
tipomoneda <- group_by(productosYapo, moneda) %>% summarise(cantidad = n())

ggplot(tipomoneda, aes(x="", y=cantidad, fill=moneda)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)


comuna <- group_by(productosYapo, Comuna) %>% summarise(cantidad = n())
#ggplot(comuna, aes(x="", y=cantidad, fill=Comuna)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)
#ggplot(data=comuna, aes(x=Comuna, y=cantidad, fill=Comuna)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(data=comuna, aes(x=Comuna, y=cantidad, fill=Comuna)) + geom_bar(stat="identity") + coord_flip()


productosYapo <- productosYapo %>% mutate( RangoPrecio = case_when( ValorPeso  < 5000 ~ "< 5.000", ValorPeso < 10000 ~ "< 10.000" , ValorPeso < 50000 ~ "< 50.000" , ValorPeso < 100000 ~ "< 100.000", ValorPeso < 1000000 ~ "< 1.000.000", ValorPeso >= 1000000 ~ ">= 1.000.000")  )

rangoprecio <- group_by(productosYapo, RangoPrecio) %>% summarise(cantidad = n())
ggplot(data=rangoprecio, aes(x=RangoPrecio, y=cantidad, fill=RangoPrecio)) + geom_bar(stat="identity") + coord_flip()
