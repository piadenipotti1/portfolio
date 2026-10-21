# ¿Cuándo se vacía la pileta? Física + inferencia bayesiana

R (`brms`, Stan) · Estadística Bayesiana, Lic. en Estadística (UNR) · 2026 · Trabajo en equipo

A partir de la ley de Torricelli, la altura del agua cumple `dh/dt = −k·√h`, de modo que `√h` decrece linealmente con el tiempo. Con esa linealización se ajustó una regresión bayesiana en `brms` con priors elegidos a partir de la física del problema (chequeados con trayectorias predictivas a priori), usando solo tres mediciones. Con la predictiva a posteriori se estimó la altura esperada a una hora dada, la probabilidad de que queden más de 5 cm de agua y la probabilidad de que la pileta ya esté vacía.

`codigo/vaciado_pileta.R`
