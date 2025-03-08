# Gestor de Consumo de Clientes

Este proyecto consiste en un programa COBOL destinado a gestionar y analizar los datos de consumo de clientes a lo largo de varios meses, permitiendo generar reportes, comparar consumos con años anteriores, calcular promedios, identificar los clientes con mayor o menor consumo, entre otras funcionalidades.

## Descripción del Proyecto

El sistema permite registrar y procesar datos de consumo para un conjunto de clientes. La aplicación está organizada en varias secciones y módulos que realizan las siguientes funciones principales:

- **Gestión de Datos de Consumo:** Carga y manipula los datos de consumo para cada cliente, tanto de manera global como mensual.
- **Comparación con el Año Anterior:** Permite comparar el consumo actual con el de años anteriores y calcular aumentos o disminuciones en términos porcentuales.
- **Cálculos de Costos y Promedios:** Calcula el costo total de consumo y el promedio de consumo entre los clientes.
- **Identificación de Clientes por Rango:** Permite buscar clientes dentro de un rango de consumo específico.
- **Generación de Reportes:** Presenta reportes detallados sobre el consumo, incluyendo los meses con mayor o menor consumo, clientes con mayores consumos, etc.

## Funcionalidades

### Menú Principal
El programa ofrece un menú interactivo donde el usuario puede seleccionar una de las siguientes opciones:

1. Mostrar total global de consumo.
2. Comparar con el año anterior.
3. Mostrar costo total de consumo.
4. Obtener consumo por mes.
5. Mostrar mes con mayor y menor consumo.
6. Obtener consumo promedio entre clientes.
7. Mostrar cliente con mayor y menor consumo.
8. Obtener consumo total de cliente.
9. Identificar clientes dentro de un rango.
10. Mostrar reporte.
11. Mostrar cantidad de clientes con consumo bajo, medio o alto.
12. Mostrar distribución porcentual de consumo por mes.
13. Salir.

### Opciones Detalladas

1. **Comparar con el Año Anterior:** Calcula el porcentaje de aumento o disminución en el consumo comparado con el año anterior. Si el consumo ha aumentado, muestra el porcentaje de aumento, y si ha disminuido, muestra el porcentaje de disminución.
2. **Consumo Mensual:** Muestra el consumo mensual por cliente, con la posibilidad de recorrer todos los meses del año.
3. **Clientes en Rango de Consumo:** Permite buscar clientes cuyo consumo esté dentro de un rango específico de valores.

## Estructura del Programa

### División de Datos

El programa está estructurado con las siguientes secciones de datos:

- **WORKING-STORAGE:** Define las variables necesarias para el procesamiento, como los indicadores de mes, los parámetros de consumo y los datos relacionados con los reportes.
- **WS-Consumo-Parametros:** Almacena los parámetros de consumo, como los valores de consumo, aumento y costo.
- **WS-Reporte-Parametros:** Almacena los parámetros de los reportes generados, como los valores de tabla y promedios.
- **WS-Maximo-Minimo-Parametros:** Define los parámetros para la identificación de los clientes con el mayor y menor consumo.

### Procedimientos

El programa incluye varios procedimientos que permiten realizar las acciones descritas en el menú, como el procesamiento de los datos, la visualización de los resultados, y la comparación de consumos con años anteriores.

## Cómo Ejecutar el Proyecto

Para ejecutar el proyecto, sigue estos pasos:

1. **Compilar el código COBOL:** Asegúrate de tener un compilador COBOL instalado y compatible.
2. **Cargar el programa en el compilador:** Usa el compilador para cargar el archivo COBOL que contiene este código.
3. **Ejecutar el programa:** Una vez cargado el código, ejecuta el programa en tu entorno de desarrollo COBOL.

## Requisitos

- **Compilador COBOL** (por ejemplo, GnuCOBOL o IBM COBOL).
- **Sistema Operativo:** El código es independiente del sistema operativo, pero debes tener un entorno compatible con COBOL.

## Contribuciones

Si deseas contribuir al proyecto, puedes hacer un **fork** del repositorio, realizar tus mejoras o correcciones, y luego enviar un **pull request**.

