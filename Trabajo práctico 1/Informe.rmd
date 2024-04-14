---
title: " "
author: " "
output: pdf_document
---

falta caratula (sacar de bayes)

\pagebreak

# Introducción

En un ensayo clínico aleatorizado se comparan dos drogas para tratar la hemorragia posparto de mujeres embarazadas. El estudio consiste en reclutar mujeres que voluntariamente aceptan formar parte del ensayo, a estas se les asignará aleatoriamente uno de los dos tratamientos y se recolectará información sobre ellas.  

El formulario de parto `DEL` registra las características basales de la mujer e información adicional sobre el nacimiento y administración del tratamiento experimental (ver en anexo). Este ingreso de información se hace en base a las respuestas de un formulario que, aunque no se desea, puede cometer errores.

El objetivo del presente informe es realizar un análisis exhaustivo de validación de datos, empleando técnicas que permiten evaluar la calidad de los datos ingresados y detectar errores sistemáticos en el llenado de los formularios.

\pagebreak

# Desarrollo de reglas de validación

A continuación se detallan las reglas globales que tienen que cumplirse en todas las preguntas y las reglas específicas de cada una con sus respectivas descripciones. Esto se hace con la finalidad de hacer un recorrido detallado del formulario y la validación a realizarse.  

Al final de esta sección, se encontrará el listado de reglas empleadas en un código de R que será probado en la base provista para identificar:

• Participantes limpios.

• Participantes con más inconscistencias.

• Errores más frecuentes.

## Reglas globales

Como regla principal y general para todas las respuestas del formulario, ninguna de ellas puede ser vacía, a no ser que alguna regla específica diga lo contrario.  

Además, se entiende que en las respuestas categóricas con codificación numérica ningún participante puso ni letras ni números que no formaba parte del listado de opciones. Se asume, por ejemplo, que nadie colocó _"No"_ en lugar de $0$, y que nadie utilizó otro número que no sea $0$, $1$ o $9$ ya que, al ser un campo cerrado sólo se admiten respuestas válidas. 

## Reglas específicas

Las reglas específicas se detallan para cada una de las preguntas del formulario que lo requieran, teniendo en cuenta cuáles son las condiciones que cada una requiere y qué implican sus respuestas. 

#### 1 - Edad materna

• No puede ser mayor o igual a 60 años.

#### 2 - Edad gestacional 

• No puede ser menor a 20 semanas (en dicho caso se considera aborto).

• No puede ser mayor a 50 semanas.

#### 3 - Cantidad de embarazos previos

• Si es 0, la respuesta 4 debe ser vacía.
• Si es 0, la respuesta 5 debe ser vacía.

#### 4 - ¿La mujer tuvo HPP en los embarazos anteriores?

• Sólo puede ser vacía si la respuesta 3 es 0.

#### 5 - ¿La mujer tuvo parto por cesárea anteriormente?

• Sólo puede ser vacía si la respuesta 3 es 0.

#### 6 - ¿Le indujeron el parto a la mujer cuando la ingresaron al hospital para este parto?

• Si es 0, las respuestas 6a1, 6a2, 6a3, 6a4 y 6as deben ser vacías.

#### 6a1 - Oxitocina

• No puede ser vacía si la respuesta 6 es 1.

#### 6a2 - Misoprostol

• No puede ser vacía si la respuesta 6 es 1.

#### 6a3 - Sonda de Foley con globo

• No puede ser vacía si la respuesta 6 es 1.

#### 6a4 - Otro

• No puede ser vacía si la respuesta 6 es 1.

#$$$ 6s - Si la repsuesta es <<Otro>>, especifique.

• No puede ser vacío si 6a4 es 1.
