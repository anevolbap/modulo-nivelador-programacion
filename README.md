# Módulo nivelador de programación

Material del módulo nivelador de programación en R para la Maestría en
Estadística Matemática (UBA). Esta versión es una **revisión** del curso
dictado en 2022: mismo lugar y misma audiencia, ejercicios con espíritu
de data science y una buena práctica de programación por clase.

Cinco clases de 90 minutos para alumnos sin experiencia previa en
programación.

Sitio público: https://anevolbap.github.io/modulo-nivelador-programacion/

## Plan del curso

| Clase | Tema                                  | Buena práctica                          |
|-------|---------------------------------------|------------------------------------------|
| 1     | Tipos de datos y funciones            | Buenos nombres                           |
| 2     | Condicionales y bucles                | Comentar el porqué, no el qué            |
| 3     | Estructuras compuestas                | Una función, una responsabilidad         |
| 4     | Visualización y exploración de datos  | Probar antes de creer (`stopifnot`)      |
| 5     | Simulación y algoritmos iterativos    | Control de versiones (git)               |

## Estructura del repo

- `clases/`: notebooks Quarto con el material expositivo de cada clase.
- `ejercicios/`: consignas para alumnos, incluida la entrega final.
- `soluciones/`: resoluciones del docente (un `.R` por ejercicio).
- `tests/`: tests automáticos de cada `ejercicioN.R` para usar con el
  paquete `corrector`.
- `datos/`: datasets de ejemplo (`bikeshare.csv`). El bonus de clase 4
  baja la [encuesta sysarmy de salarios IT](https://github.com/openqube/openqube-sueldos)
  directamente desde el repo de openqube.
- `images/`: imágenes embebidas en los notebooks.
- `_quarto.yml`, `index.qmd`: configuración del sitio.

## Renderizar el sitio localmente

Requiere [Quarto](https://quarto.org). El sitio se construye con:

```bash
quarto render        # rinde todo el proyecto a _site/
quarto preview       # servidor local con auto-reload
```

Renderizar una sola clase:

```bash
quarto render clases/clase1-clase.qmd
```

## Publicar

`.github/workflows/publish.yml` rinde y publica a la rama `gh-pages`
en cada push a `main`. Para activar GitHub Pages la primera vez,
ir a Settings → Pages y elegir `gh-pages` como branch source.

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
