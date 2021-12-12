library(rvest)
library(xml2)
titulos <- c()
precios <- c()
listaValor <- c()
listaValorUF <- c()
moneda <- c()
imagenes <- c()
urlproducto <- c()
descripciones <- c()
tipoProductos <- c()
regiones <- c()
vendedores <- c()
identificadores <- c()


detalleProducto <- function(indice, urlP)
{
  error404 <<- FALSE
  print(indice)
  result = tryCatch( 
    {paginaProducto <- read_html(urlP)},
    error = function(err){
      print( paste("No se pudo generar pagina", indice))
      regiones <<- c(regiones, NA)
      vendedores <<- c(vendedores, NA)
      descripciones <<- c(descripciones, NA)
      tipoProductos <<- c(tipoProductos, NA)
      error404 <<- TRUE
    },
    finally = {
      
      if( error404 == FALSE)
      {
        print("Procesando detalle")
        sellerinfo <- paginaProducto %>% html_nodes(xpath = "//seller-info") 
        vendedor <- sellerinfo %>% html_attr("username")
        region <- sellerinfo %>% html_attr("region")
        regiones <<- c(regiones, region)
        vendedores <<- c(vendedores, vendedor)
        
        tipoProducto <- ""
        boxinfo <- paginaProducto %>% html_nodes(css = ".info")
        descripcion <- boxinfo %>% html_nodes(css = ".description") %>% html_nodes(css = "p") %>% html_text()
        details <- boxinfo %>% html_nodes(css = ".details") %>% html_nodes(css = "tr")
        for(j in 1 : length(details) )
        {
          if( substr(details[j] %>% html_text(), 1, 4) == "Tipo")
          {
            tipoProducto <- gsub("Tipo", "", details[j] %>% html_text() )
          }
        }
        
        descripciones <<- c(descripciones, descripcion)
        tipoProductos <<- c(tipoProductos, tipoProducto)
      }
    }
  )
  
  
  
}

#define el valor de la UF
valorUF = 30892.18

for(pagina in 1:350)
{
  link <- paste("https://www.yapo.cl/region_metropolitana?ca=15_s&o=", pagina ,sep = "")
  
  print( paste("Pagina Num:", pagina, "\nLink:", link))
  yapoPage <- read_html(link)
  listaProductos <- yapoPage %>% html_nodes( css = ".listing_thumbs tr")
  
  
  for(indice in 1:length(listaProductos))
  {
    noexiste <- TRUE
    nombre <- listaProductos[indice] %>% html_nodes( css = ".title") %>% html_text()
    if( length(nombre) > 0 )
    {
      identificador <- listaProductos[indice] %>% html_attr("id")
      if( identificador %in% identificadores )
      {
        noexiste <- FALSE
        printf( paste("DUPLICADO:", identificador))
      } 
    }
    if( length(nombre) > 0 && noexiste)
    {
      identificadores <- c(identificadores, listaProductos[indice] %>% html_attr("id") )
      
      #obtiene la url del producto
      urlp <- listaProductos[indice] %>% html_nodes( css = ".title") %>% html_attr("href")
      urlproducto <- c(urlproducto, urlp)
      
      #obtiene la imagen
      urlimagen <- listaProductos[indice] %>% html_nodes( css = ".image") %>% html_attr("src")
      if( 	urlimagen == "/img/transparent.gif")
      {
        imagenes <- c(imagenes, NA)
      }
      else if( length(urlimagen) > 0 )
      {
        imagenes <- c(imagenes, urlimagen)
      }
      else
      {
        imagenes <- c(imagenes, NA)
      }
      
      #obtiene el titulo del producto
      titulos <- c(titulos, nombre)
      precio <- listaProductos[indice] %>% html_nodes( css = ".price") %>% html_text()
      if( length(precio) > 0 )
      {
        precio <- gsub("\t", "", precio)
        precio <- gsub("\n", "", precio)
        precio <- gsub("[$]", "", precio)
        precio <- gsub("[.]", "", precio)
        precio <- gsub(",", ".", precio)
        precio <- gsub(" ", "", precio)
        if( substr(precio, 1, 2) == 'UF')
        {
          precioUF = gsub("UF", "", precio)
          pconv <- as.numeric(precioUF)
          precioPeso <-  pconv * valorUF
          listaValorUF <- c(listaValorUF, precioUF)
          listaValor <- c(listaValor, precioPeso)
          moneda <- c(moneda, "UF")
        }
        else
        {
          precioUF <- NA
          precioPeso <- gsub("$", "", precio)
          listaValorUF <- c(listaValorUF, precioUF)
          listaValor <- c(listaValor, precioPeso)
          moneda <- c(moneda, "$")
        }
        precios <- c(precios, precio )  
      }
      else
      {
        precios <- c(precios, NA)
        listaValor <- c(listaValor, NA)
        listaValorUF <- c(listaValorUF, NA)
        moneda <- c(moneda, NA)
      }
    }
  }
  
  
  
}

#obtiene el detalle del producto
for(urlp in 1:length(urlproducto)  )
{
  #productoPage <- read_html(urlp)
  print( paste("Detalle Producto Num:", urlp) )
  detalleProducto(urlp, urlproducto[urlp])
}

productosYapo <- data.frame(nombre = titulos, moneda = moneda, ValorPeso = listaValor, ValorUF = listaValorUF, imagen = imagenes, region = regiones, vendedor = vendedores, tipo = tipoProductos, descripcion = descripciones )




productosYapo$descripcion <- gsub("\n", "", productosYapo$descripcion)
productosYapo$descripcion <- gsub("\r", "", productosYapo$descripcion)
productosYapo$tipo <- gsub("\n", "", productosYapo$tipo)


write.csv(productosYapo,"productosYapo.csv")
