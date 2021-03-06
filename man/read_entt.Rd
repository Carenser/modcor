% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{read_entt}
\alias{read_entt}
\title{Chargement des fichiers de listing des entités aux formats prévues dans les règles SI décrivant les flux en provenance de RTE à destination des GRD}
\usage{
read_entt(dossiers, fichiers = NULL)
}
\arguments{
\item{dossiers}{le nom du répertoire contenant les fichiers passés en paramètre}

\item{fichiers}{un vecteur contenant les noms des fichiers de listing des entités  (facultatif)}
}
\value{
un dataframe comprenant 5 colonnes : MECANISME, CODE_ENTITE, METHODE, DEBUT, FIN
}
\description{
Chargement des fichiers de listing des entités aux formats prévues dans les règles SI décrivant les flux en provenance de RTE à destination des GRD
}
