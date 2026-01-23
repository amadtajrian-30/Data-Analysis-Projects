Pennsylvania Influenza Surveillance Dashboard (October-December 2025)

Overview: This simple dashboard visualizes weekly laboratory-confirmed influenza activity in Pennsylvania during late 2025. The dashboard was developed in R using Shiny and is based on publicly available influenza surveillance data reported to the CDC.
This was a beginner project that aimed to use surveillance data to support clear interpretation of influenza trends through an accessible and user-friendly interface.

Influenza surveillance data were obtained from CDC FluView virologic surveillance, accessed via the Carnegie Mellon University Delphi API using the epidatr R package. These data were from the WHO/NREVSS collaborating laboratory network, which reports weekly counts of respiratory specimens collected and tested for influenza, and the number of positive tests for influenza A and B.

Methods: For this project, flu cases are defined as laboratory-confirmed positive influenza specimens, calculated as the total of specimens testing positive for influenza A and influenza B in a given surveillance week. The time period was restricted to late 2025, defined as MMWR weeks 40-52. These weeks corresponded to early October through late December 2025. This time period reflects the typical onset and early peak of the U.S. influenza season. Influenza surveillance data were extracted using the epidatr R package and processed using dplyr. Visualizations were created with ggplot2, and the interface was built using Shiny.

Contributed by: Tajrian Amad
