zipcode2district <- function(postalcode){

  # Check the length and type of the postalcode -----------------------------

  if(nchar(postalcode) != 6) {
    stop("The postal code of Singapore must be 6-digits.")
  }
  if(is.na(try(as.numeric(postalcode)))){
    stop("The postal code can only contain numbers.")
  }

  first_2_digit <- substr(postalcode, 1, 2)

  if((first_2_digit %in% SG_ZipCode.vs.District_Mapping$Postal.Sector) == FALSE){
    stop("The postal code is invalid.")
  } else{
    to_return <- list(SG_ZipCode.vs.District_Mapping$Postal.District[SG_ZipCode.vs.District_Mapping$Postal.Sector == first_2_digit],
                      SG_ZipCode.vs.District_Mapping$General.Location[SG_ZipCode.vs.District_Mapping$Postal.Sector == first_2_digit])
    names(to_return) <- c("Postal District", "General Location")
    return(to_return)
  }

}
