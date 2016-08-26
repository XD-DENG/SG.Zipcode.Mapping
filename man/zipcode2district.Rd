\name{zipcode2district}
\alias{zipcode2district}
\title{
Map Singapore Zipcode to the District
}
\description{
Given the zipcode, return the correspondingdistrict in Singapore.
}
\usage{
zipcode2district(postalcode)
}

\arguments{
  \item{postalcode}{
  Zip code of Singapore. Must be 6 digits. In character format.
}
}
\details{
Result will be returned as a list. The first element is the postal district and the second element is the general location.
}

\examples{
zipcode2district("117549")
}
