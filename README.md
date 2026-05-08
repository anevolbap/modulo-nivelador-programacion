# Módulo nivelador de programación

Material (incompleto) del módulo nivelador de programación en R para la Maestría en
Estadística Matemática (UBA), edición 2022. El curso cubre las bases del
lenguaje pensadas para alumnos sin experiencia previa: tipos, vectores,
funciones, control de flujo, dataframes y un par de aplicaciones numéricas
(bisección, interpolación de Lagrange).

## Estructura

- `clases/`: notebooks Quarto con el material expositivo de cada clase.
- `ejercicios/`: consignas para alumnos, incluida la entrega final.
- `soluciones/`: resoluciones del docente (un `.R` por ejercicio y los
  notebooks que las acompañan).
- `tests/`: tests automáticos de cada `ejercicioN.R` para usar con el
  paquete `corrector`.
- `datos/`: dataset `bikeshare.csv` que se usa en `clases/dataframe.qmd`.
- `images/`: imágenes embebidas en los notebooks.

## Renderizar los notebooks

Requiere [Quarto](https://quarto.org) y R con los paquetes que carga cada
notebook (al menos `ISLR2`, `ggplot2`, `gganimate`).

```bash
quarto render clases/clase1-clase.qmd
```

Para renderizar todo el directorio:

```bash
quarto render clases/ ejercicios/ soluciones/
```

## Corregir entregas

La corrección automática se hace con el paquete
[`corrector`](https://github.com/anevolbap/corrector), que reemplaza al
script ad-hoc usado en 2022. Los tests bajo `tests/` son los que el paquete
consume.

```r
# install.packages("remotes")
remotes::install_github("anevolbap/corrector")

library(corrector)
resultados <- grade_submissions("ruta/a/entregas/", test_dir = "tests/")
grade_report(resultados)
```
