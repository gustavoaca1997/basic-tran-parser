# Lexer de Basic-Translation

Integrantes:
* German Robayo (14-10924)
* Gustavo Castellanos (14-10192)

### ¿Que es?

Un lexer es una herramienta que nos permite analizar una secuencia de caracteres y extraer de ella _tokens_ (análisis lexicográfico) para su posterior parseo.

### Herramienta usada

Se usó como lenguaje de programación *__Haskell__* y como herramienta generadora de analizadores lexicográficos *__Alex__*.
El uso de Alex fue fundamental, pues nos enfocó en solamente determinar expresiones regulares para extraer los tokens del archivo.

### Breve explicación

Alex provee varios wrappers, pero el de nuestro interés fue `posn` pues provee la misma funcionalidad que el `basic` pero añadiendo la columna y fila de cada token detectado.

Para implementar el `show` de cada token, se creo un tipo de dato `TkObject` que puede verse como un par ordenado cuya primera coordenada es un token y segunda es su `AlexPosn`. Así, se evitó tener que escribir un `show` por cada token.

Para el filtrado de errores se creó un token llamado `TkErr`. Cuando invocamos a `alexScanTokens` tenemos un arreglo de `TkObject`'s. Para ver si hay error solamente filtramos los TkObjects y si el arreglo resultante es vacio pues no hay errores y se imprime cada token en el formato descrito. En caso contrario, solamente se imprimen errores.

### Librerias adicionales

Ninguna. Aunque se pudieron importar ciertas funciones se decidió para practicar mas el lenguaje que nosotros mismos implementaramos dichas funciones.

### Uso del programa

Primero debes generar el archivo `.hs`

```bash
$ alex Lex.x
```

Luego compilar el archivo resultante mediante `ghc`

```bash
$ ghc Lex.x
# quizá tengas que usar 'stack ghc'
```

Finalmente, se usa de la siguiente forma:

```bash
$ ./Lex <archivo>
```
