# Módulo nivelador de programación

Material del módulo nivelador de programación en R para la Maestría en Estadística Matemática (UBA). Claude's reversion based on the 2022 course.

Sitio: https://anevolbap.github.io/modulo-nivelador-programacion/

## Plan

| Clase | Tema                                 | Buena práctica                       |
|-------|--------------------------------------|--------------------------------------|
| 1     | Tipos de datos y funciones           | Buenos nombres                       |
| 2     | Condicionales y bucles               | Comentar el porqué                   |
| 3     | Estructuras compuestas               | Una función, una responsabilidad     |
| 4     | Visualización y EDA                  | Probar antes de creer (`stopifnot`)  |
| 5     | Simulación y algoritmos iterativos   | Control de versiones (git)           |

## Estructura

- `clases/`: notebooks Quarto.
- `ejercicios/`: consignas y entrega.
- `soluciones/`: resoluciones del docente.
- `tests/`: tests automáticos para el paquete [`corrector`](https://github.com/anevolbap/corrector).
- `datos/`: datasets de ejemplo.

## Renderizar

```bash
quarto preview
quarto render
```

`.github/workflows/publish.yml` rinde y publica a `gh-pages` en cada push a `main`.

## Corregir entregas

```r
remotes::install_github("anevolbap/corrector")
corrector::grade_submissions("ruta/a/entregas/", test_dir = "tests/")
```
