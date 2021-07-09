library(readxl)
library(tidyverse)

# set working directory to current path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# data preparations
city <- read_excel('excel/global-city-indicators.xlsx', sheet = 'World Cities data')
city2 <- filter(city, !is.na(city))
city3 <- column_to_rownames(city2, var = 'Geography')
#city4 <- select(city3, -c(1))
city4 <- select(city3, -c('London Data Source','Shanghai'))
# city5 <- as_tibble(t(city4), rownames = "row_names")
city5 <- data.frame(t(city4), rownames = "row_names")
city5[city5 == '-----'] <- NA 
#city6 <- sapply(city5, function(x) as.numeric(unlist(x)))
# Liste zu konvertierender Spalten
no.numeric.cols <- c('City.Population..millions.',
                     'Share.of.Global.500.Companies....',
                     'GDP.Per.Capita..thousands....PPP.rates..per.resident.',
                     'Number.of.Hospitals',
                     'Percent.of.Population.with.Higher.Education....',
                     'Higher.Education.Institutions',
                     'Physicians.per.100.000.People',
                     'Air.Quality.',
                     'Mass.Transit.Commuters',
                     'Green.Spaces..km2.',
                     'Hotel.Rooms..thousands.',
                     'Number.of.Cultural.and.Arts.Organizations',
                     'Number.of.Museums'
                     )
# kovertiere Spalten zu numerisch
city5[ , no.numeric.cols] <- apply(city5[ , no.numeric.cols], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))


# funktions zur Berechnung der LM Gleichung 
# quelle: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
lm_eqn <- function(df, y, x){
  formula = as.formula(sprintf('%s ~ %s', y, x))
  m <- lm(formula, data=df);
  # formating the values into a summary string to print out
  # ~ give some space, but equal size and comma need to be quoted
  eq <- substitute(italic(target) == a + b %.% italic(input)*","~~italic(r)^2~"="~r2*","~~p~"="~italic(pvalue), 
                   list(target = y,
                        input = x,
                        a = format(as.vector(coef(m)[1]), digits = 2), 
                        b = format(as.vector(coef(m)[2]), digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3),
                        # getting the pvalue is painful
                        pvalue = format(summary(m)$coefficients[2,'Pr(>|t|)'], digits=1)
                   )
  )
  as.character(as.expression(eq));                 
}


### plot

ggplot(data = city5, aes(x = City.Population..millions., y = Air.Quality.)) + 
  geom_point(na.rm = TRUE) +
  geom_text(
    label=rownames((city5)),
    nudge_x = -0.05, nudge_y = 0.05, 
    check_overlap = T
  ) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  geom_smooth(method='lm', se=FALSE, color='grey') +
  annotate(
    "text", label = lm_eqn(city5, 'City.Population..millions.','Air.Quality.'),
    x = 2, y = 10, size = 3, colour = "black", parse=T
  ) +
  theme_light()#theme_minimal() #


# infrastrukture. economy of scale. Skaliert sublinear (ca. Steigungs ca 0.85?)
# Sozioökoomische Faktoren (z.B.einkommen, gdp, Crime (binned), patents(binned)), Steigung 1.15

# funktion zum erstellen des Plots
create.plot <- function(df, col.x, col.y) {
  pl <- ggplot(data = df, aes(x = .data[[col.x]], y = .data[[col.y]])) + 
    geom_point(na.rm = TRUE, color='blue') +
    geom_text(
      label=rownames((df)),
      nudge_x = -0.05, nudge_y = 0.05, 
      color = 'grey',
      check_overlap = T
    ) +
    scale_x_continuous(trans='log10') +
    scale_y_continuous(trans='log10') +
    geom_smooth(method='lm', se=FALSE, color='black') +
    annotate(
      "text", label = lm_eqn(df, col.x,col.y),
      x = 1, y = max(df[col.y], na.rm = TRUE)*1.3, size = 3, colour = "black", parse=T
    ) +
    labs(
      title = col.y,
      subtitle = 'Untertitel'
    ) +
    theme_light()#theme_minimal() #
  return(pl)
}
p1 <- create.plot(city5,'City.Population..millions.','Number.of.Museums')
p1

# saving plots to file
for (column in no.numeric.cols[-1]) {
  print(column)
  p <- create.plot(city5,'City.Population..millions.',column)
  filename <- gsub('.','_',column,fixed = TRUE)
  filename <- paste(filename,'.png')
  ggsave(filename, path = 'plots', plot = p)
}


