# Install and load packages
f_install_and_load <- function(packs) {
  new.packs <- packs[!(packs %in% installed.packages()[ ,"Package"])]
  lapply(new.packs, install.packages, repos="http://cran.rstudio.com/", dependencies=TRUE)
  lapply(packs, library, character.only=TRUE)
}

# Scale and center a numeric vector
f_center_and_scale <- function(vector, num.sd = 2) {
  # num.sd is how many sd to divide by
  (vector - mean(vector, na.rm=T)) / (num.sd * sd(vector, na.rm=T))
}

# Create an environment that contains the stata var labels
f_stata_to_env <- function(df) {
  lab_env <- new.env()
  for (i in seq_along(names(df))) {
    lab_env[[names(df)[i]]] <- attr(df, "var.labels")[i]
  }
  return(lab_env)
}

f_stata_to_df <- function(df) {
  lab_df <- data.frame(var.name = names(df),
                       var.label = attr(df, "var.labels"))
  return(lab_df)
}

# Split countryyear
f_splitcountryyear <- function(df) {
  library(Hmisc)
  # First remove Unicode
  ascii_country <- iconv(df$country, "latin1", "ASCII", sub="")
  # Split the countryyear into country and year
  country_year <- strsplit(ascii_country, "(?<=[a-zA-Z])(?=[0-9])", perl=TRUE)

  # Create new variables year2 and country2 according to the split
  df$year <- sapply(country_year, function(x) as.numeric(x[2]))
  df$country <- sapply(country_year, function(x) paste0(x[1:(length(x)-1)]))
  df$country <- capitalize(df$country)
  return(df)
}

# Function to clean name to fit with countrycode country names
f_cleancname <- function(ss) {
  require(plyr)
  revalue(ss,
          c("Antiguaandbarbuda"="Antigua and Barbuda",
            "BiH"="Bosnia and Herzegovina",
            "Bolivia"="Bolivia, Plurinational State of",
            "Burkinafaso"="Burkina Faso",
            "BurkinaFaso"="Burkina Faso",
            "Capeverde"="Cabo Verde",
            "CapeVerde"="Cabo Verde",
            "Cape Verde"="Cabo Verde",
            "Centralafricanrepublic"="Central African Republic",
            "Congo, The Democratic Republic Of"="Congo, the Democratic Republic of the",
            "Costarica"="Costa Rica",
            "CostaRica"="Costa Rica",
            "Cte d'Ivoire"="Cote d'Ivoire",
            "Czech"="Czech Republic",
            "DominicanRepublic"="Dominican Republic",
            "Drc"="Congo, the Democratic Republic of the",
            "DRC"="Congo, the Democratic Republic of the",
            "ElSalvador"="El Salvador",
            "Elsalvador"="El Salvador",
            "Fyr Macedonia"="Macedonia, the former Yugoslav Republic of",
            "FYROM"="Macedonia, the former Yugoslav Republic of",
            "GuineaBissau"="Guinea-Bissau",
            "Kosovo"="Kosovo", # FLAG FLAG
            "Kyrgyz Republic"="Kyrgyzstan",
            "Laos"="Lao People's Democratic Republic",
            "LaoPDR"="Lao People's Democratic Republic",
            "Moldova"="Moldova, Republic of",
            "Micronesia"="Micronesia, Federated States of",
            "Montenegro"="Yugoslavia",
            "Russia"="Russian Federation",
            "StKittsandNevis"="Saint Kitts and Nevis",
            "StLucia"="Saint Lucia",
            "StVincentandGrenadines"="Saint Vincent and the Grenadines",
            "Serbia"="Yugoslavia",
            "Serbia&Montenegro"="Yugoslavia",
            "Slovak Republic"="Slovakia",
            "SouthAfrica"="South Africa",
            "SouthKorea"="Korea, Republic of",
            "SriLanka"="Sri Lanka",
            "Syria"="Syrian Arab Republic",
            "Tanzania"="Tanzania, United Republic of",
            "Timor Leste"="Timor-Leste",
            "TrinidadandTobago"="Trinidad and Tobago",
            "Venezuela"="Venezuela, Bolivarian Republic of",
            "Vietnam-b"="Viet Nam",
            "Vietnam"="Viet Nam"), warn_missing=TRUE)
}

# ---- Balance table ----
# Get level names from variables (for balance table)
f_getlevelnames <- function(df, vars) {
  unlist(sapply(vars, function(var) paste(var, levels(df[ , var])[-1], sep="_")))
}

# Add significance level star to table
f_addstar <- function(df, pvaluevar="p.value") {
  pval <- df[ , pvaluevar]
  if (is.null(pval)) {
    stop("Variable not found.")
  } else {
    stars <- ifelse(pval <= .001, "***",
                    ifelse(pval <= .05, "**",
                           ifelse(pval <= .1, "*", "")))
  }
  return(cbind.data.frame(df, stars))
}

# Create a balance table based on MatchBalance result
f_create_balancetable <- function(df, balance_vars, bal_result) {
  res <- f_addstar(
    cbind.data.frame(
      var.name=f_getlevelnames(df, balance_vars),
      ldply(bal_result, function(x)
        data.frame(mean.Treatment=x$mean.Tr, mean.Control=x$mean.Co, p.value=x$p.value))
    )
  )
  # f <- Vectorize(function(x) if (is.null(d_pci_env[[x]])) x else d_pci_env[[x]])
  # res <- cbind.data.frame(f(rownames(res)), res[, -1])
  return(res)
}

# Re-order columns
# http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe
moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}

f_lm_eqn = function(m) {

  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));

  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  }

  as.character(as.expression(eq));
}