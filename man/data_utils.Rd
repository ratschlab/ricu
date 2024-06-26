% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-utils.R
\name{id_origin}
\alias{id_origin}
\alias{id_orig_helper}
\alias{id_orig_helper.src_env}
\alias{id_orig_helper.miiv_env}
\alias{id_orig_helper.sic_env}
\alias{id_windows}
\alias{id_win_helper}
\alias{id_win_helper.mimic_env}
\alias{id_win_helper.eicu_env}
\alias{id_win_helper.sic_env}
\alias{id_win_helper.hirid_env}
\alias{id_win_helper.aumc_env}
\alias{id_win_helper.miiv_env}
\alias{id_win_helper.picdb_env}
\alias{id_map}
\alias{id_map_helper}
\alias{id_map_helper.src_env}
\title{Data loading utilities}
\usage{
id_origin(x, id, origin_name = NULL, copy = TRUE)

id_orig_helper(x, id)

\method{id_orig_helper}{src_env}(x, id)

\method{id_orig_helper}{miiv_env}(x, id)

\method{id_orig_helper}{sic_env}(x, id)

id_windows(x, copy = TRUE)

id_win_helper(x)

\method{id_win_helper}{mimic_env}(x)

\method{id_win_helper}{eicu_env}(x)

\method{id_win_helper}{sic_env}(x)

\method{id_win_helper}{hirid_env}(x)

\method{id_win_helper}{aumc_env}(x)

\method{id_win_helper}{miiv_env}(x)

\method{id_win_helper}{picdb_env}(x)

id_map(x, id_var, win_var, in_time = NULL, out_time = NULL)

id_map_helper(x, id_var, win_var)

\method{id_map_helper}{src_env}(x, id_var, win_var)
}
\arguments{
\item{x}{Object identify the ID system (passed to \code{\link[=as_src_env]{as_src_env()}})}

\item{id}{ID name for which to return origin times}

\item{origin_name}{String-valued name which will be used to label the origin
column}

\item{copy}{Logical flag indicating whether to return a copy of the memoized
\verb{0data.table} for safety}

\item{id_var}{Type of ID all returned times are relative to}

\item{win_var}{Type of ID for which the in/out times is returned}

\item{in_time, out_time}{column names of the returned in/out times}
}
\value{
\itemize{
\item \code{id_origin()}/\code{id_orig_helper()}: an \code{id_tbl} with admission time stamps
corresponding to the selected ID
\item \code{id_windows()}/\code{id_win_helper()}: an \code{id_tbl} holding all IDs and their
respective start and end times
\item \code{id_map()}/\code{id_map_helper()}: an \code{id_tbl} containing the selected IDs and
depending on values passed as \code{in_time} and \code{out_time}, start and end
times of the ID passed as \code{win_var}
}
}
\description{
Two important tools for smoothing out differences among used datasets are
\code{id_origin()} which returns origin times for a given ID and \code{id_map()}
which returns a mapping between two ID systems alongside start and end
columns of the target ID system relative to the source ID system. As both
these function are called frequently during data loading and might involve
somewhat expensive operations, both rely on internal helper functions
(\code{id_orig_helper()} and \code{id_map_helper()}) which perform the heavy lifting
and wrap those helper functions, providing a memoization layer. When adding
a new data source, a class specific implementation of the S3 generic
function \code{id_map_helper()} might be required, as this is used during data
loading using \code{\link[=load_id]{load_id()}} and \code{\link[=load_ts]{load_ts()}} via \code{\link[=change_id]{change_id()}}.
}
\details{
For the internal datasets, \code{id_map_helper()} relies on yet another S3
generic function \code{id_windows()}, which provides a table containing all
available ID systems, as well as all ID windows for a given data source. As
for the other two functions, the same helper-function approach is in place,
with the data loading function \code{id_win_helper()}. The function
\code{id_map_helper()} is then implemented in a data source agnostic manner
(dispatching on the \code{src_env} class), providing subsetting of this larger
ID map table and ensuring timestamps are relative to the correct ID system.
For adding a new data source however, this layer can be forgone. Similarly
for \code{id_origin()}, this is used for the internal datasets in
\code{\link[=load_difftime]{load_difftime()}}. An implementation of \code{\link[=load_difftime]{load_difftime()}}, specific to a
new data source can be provided that does not rely on \code{id_windows()},
making this function irrelevant for this specific dataset.
}
\keyword{internal}
