
# podemos usar 'devtools'
# se crea una nueva carpeta con lo necesario
# devtools::create("D:/factorito")
# ya no es necesario correrlo, ya se creo!

# conectarlo a un repositorio de git
usethis::use_git()
usethis::use_github()

# para ir al archivo de la funcion
usethis::use_r("tablaf")

# carga las funciones guardadas en /R
devtools::load_all()

# hace un chequeo  <--------- para chequear que todo vaya ok!
devtools::check()

# colocar los paquetes que estas usando (como buena practica)
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("magrittr")
usethis::use_package("lavaan")
usethis::use_package("psych")
usethis::use_package("semTools")
usethis::use_package("ggplot2")

# licencia? como funciona? coloquemos igual
usethis::use_mit_license()

# hacer esto cuando colocas una funcion para "documentar" <---------
devtools::document()
# ?tablaf, sip, se agrega! pero no salen las tildes! :S :(
# ?tablaf_col
# ?reporte_cfa_lavaan
# ?cor2
# ?reporte_pca
# ?mean_prop_grupo
# ?g_patron_missing

# usethis::use_readme_rmd() # para abrir la ventana de readme
devtools::build_readme() # para render

# esto soluciona: Non-standard file/directory found at top level
usethis::use_build_ignore("para_ir_probando.R", escape = TRUE)

# no visible binding for global variable ?
usethis::edit_r_environ()

