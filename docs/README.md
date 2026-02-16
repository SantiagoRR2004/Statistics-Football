# Football

## Overview

This is a project for a subject called "Estadística" (Statistics) that added an extra point to the final grade. It was made individually. The submission took place on Saturday, the 15th of April of 2023, and earned an extra 0.75.

## Project Summary

For this project we had to simulate the results of the Spanish football league (La Liga) for the 2022-2023 season. We had to calculate the probabilities of each team finishing in each position, and then calculate the probabilities of the Celta being relegated and the Atlético de Madrid being in the top five. Here is an [explanation](./sim-campionato.pdf).

The mistake was changing the probabilities after each game (`predictionOriginal`), the correct version is to never change them so we can ensure the result converges (`prediction`).
