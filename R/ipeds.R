#' IPEDS (Integrated Postsecondary Education Data System) data, 2017
#'
#' A data set formed by extracting information from the surveys conducted by the Institute of Education Sciences in 2017 (link provided as a source). These surveys are conducted annually by NCES () to gather information from every postsecondary institution that participates in federal student financial aid programs.
#'
#' This data set represents a population of both degree-granting and non-degree-granting active post-secondary institutions of all levels and sectors. It includes both public and for-profit institutions. The vast majority of institutions in the data set are Title IV institutions (they participate in federal financial aid programs), although some non-Title IV institutions participate as well.
#'
#' @format A data frame with 6,440 rows and 60 columns. Each row corresponds to an institution.
#'
#' \describe{
#' \item{UNITID}{unique identification number of the institution, as listed in the IPEDS database}
#' \item{INSTNM}{institution name}
#' \item{ADDR}{street address or post office box}
#' \item{State}{two-letter US state abbreviation}
#' \item{ZIP}{ZIP code}
#' \item{GENTELE}{ten-digit telephone number}
#' \item{WEBADDR}{Internet website address}
#' \item{LONGITUD}{longitude of location}
#' \item{LATITUDE}{latitude of location}
#' \item{Urbanicity}{(categorical) whether the location of the institution is \code{rural} or in a \code{town}, \code{city}, or \code{suburb}}
#' \item{Degree_Level}{(categorical) highest level of degree offered; \code{1} = four or more years, \code{2} = at least two but less than four years, \code{3} = less than two years}
#' \item{Control_Level}{(categorical) whether the institution is publicly or privately run; \code{1} = public, \code{2} = private and not-for-profit, \code{3} = private and for-profit}
#' \item{HS_Remedial_Services}{(categorical) whether the institution offers adult basic remedial or high school equivalent courses}
#' \item{Remedial_Services}{(categorical) whether the institution offers remedial services, i.e. instructional activities designed for students deficient in the general competencies necessary for a regular postsecondary curriculum and educational setting.}
#' \item{PCT_GrantAid}{(continuous) percent of undergraduate students awarded federal, state, local, institutional or other sources of grant aid}
#' \item{PCT_Pell}{(continuous) percent of undergraduate students awarded Pell grants}
#' \item{PCT_FedLoans}{(continuous) percent of undergraduate students awarded federal student loans}
#' \item{PCT_AnyAid}{(continuous) percent of full-time first-time undergraduates awarded any financial aid}
#' \item{Retention_Fullt}{(continuous) full-time retention rate}
#' \item{Retention_Partt}{(continuous) part-time retention rate}
#' \item{Total_Undergrad}{(continuous) total no. of undergraduate students seeking a degree/certificate}
#' \item{PCT_Female}{(continuous) percentage of female undergraduate students enrolled for credit}
#' \item{PCT_Male}{(continuous) percentage of male undergraduate students enrolled for credit}
#' \item{PCT_NatAA}{(continuous) percentage of Native American or Alaska native undergraduate students enrolled for credit}
#' \item{PCT_Asian}{(continuous) percentage of Asian undergraduate students enrolled for credit}
#' \item{PCT_Black}{(continuous) percentage of black or African American undergraduate students enrolled for credit}
#' \item{PCT_Hispanic}{(continuous) percentage of Hispanic or Latino undergraduate students enrolled for credit}
#' \item{PCT_NatHPI}{(continuous) percentage of Native Hawaiian or Pacific Islander undergraduate students enrolled for credit}
#' \item{PCT_White}{(continuous) percentage of white students enrolled for credit}
#' \item{PCT_2more}{(continuous) percentage of students enrolled for credit who selected more than one race}
#' \item{PCT_unknown}{(continuous) percentage of students enrolled for credit who did not select a race}
#' \item{PCT_Foreign}{(continuous) percentage of foreign (non-resident alien) undergraduate students enrolled for credit}
#' \item{Admit_rate}{(continuous) total number of admitted students as a percentage of total number of applicants}
#' \item{TitleIV_Status}{(categorical) whether or not instituation has Title-IV status, i.e. whether or not the insitution processes US federal student aid }
#' \item{PCT_LowInc}{(continuous) percentage of students in income level 0-30,000}
#' \item{PCT_LowMidInc}{(continuous) percentage of students in income level 30,001-48,000}
#' \item{PCT_MidInc}{(continuous) percentage of students in income level 48,001-75,000}
#' \item{PCT_MidHighInc}{(continuous) percentage of students in income level 75,001-110,000}
#' \item{PCT_HighInc}{(continuous) percentage of students in income level 110,001 or more}
#' \item{Multisystem_Status}{(categorical) whether or not a school is part of a multi-institution or multi-campus organization}
#' \item{Parentsystem}{name of multi-institution or multi-campus organization}
#' \item{Public}{(binary) from \code{Control_Level}; \code{1} for public institutions, \code{0} otherwise}
#' \item{Private_Profit}{(binary) from \code{Control_Level}; \code{1} for private for-profit institutions, \code{0} otherwise}
#' \item{Private_Notforprofit}{(binary) from \code{Control_Level}; \code{1} for private not-for-profit institutions, \code{0} otherwise}
#' \item{City}{(binary) from \code{Urbanicity}; \code{1} for institutions in cities, \code{0} otherwise}
#' \item{Suburb}{(binary) from \code{Urbanicity}; \code{1} for institutions in suburbs, \code{0} otherwise}
#' \item{Town}{(binary) from \code{Urbanicity}; \code{1} for institutions in towns, \code{0} otherwise}
#' \item{Rural}{(binary) from \code{Urbanicity}; \code{1} for institutions in rural areas, \code{0} otherwise}
#' }
#' @source \url{https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx}
"ipeds"
