cdata=read.csv("C:\\Users\\Nash\\Downloads\\countries.csv")

library(stringr)

colnames(cdata)

subsel = cdata[,c("name","region","intermediate.region","sub.region")]


sorted=subsel[order(subsel[,"region"],subsel[,"intermediate.region"],subsel[,"sub.region"]),]
names = sorted[,"name"]


data=read.csv("C:\\Users\\Nash\\Downloads\\merged.csv")
countries=data$Country


# countries in countries, but not names
for (countryWeNeed in setdiff(countries, names)){
  match=agrep(countryWeNeed,names,ignore.case=T,value=T,max.distance = 0.1, useBytes = FALSE)
  names=replace(names,which(names==match),countryWeNeed)
}

names[which(names=="Hong Kong")]="Hong Kong, China (SAR)"
names[which(names=="United States of America")]="United States"
names[which(names=="Korea, Republic of")]="Korea (Republic of)"
names[which(names=="Eswatini")]="Eswatini (Kingdom of)"


excess=setdiff(names, countries)

ordered_countries = names[! names %in% excess]

length(intersect(ordered_countries, countries))==188


library(plotly)
countries=data$Country
mat=data[,31:60]
M=as.matrix(mat)
M=M[,ncol(M):1]
years=rev(seq(1990, 2019))
colnames(M)=years
rownames(M)=countries
M=M[ordered_countries,]

# fill missing leichtenstein values from before they provided data with the first recorded value
M["Liechtenstein",is.na(M["Liechtenstein",])]=0.862

# fill missing usa values with average of preceeding and following values
M["United States", "1997"]=mean(M["United States", "1996"],M["United States", "1998"])
M["United States", "1992"]=mean(M["United States", "1991"],M["United States", "1993"])


# observing the table, it appears all of the rest of the countries with have NA values are similar to
# liechtenstein in that the nas all occur befpr a certain date. We handle these the same

hasNas = c()

for (name in rownames(M)){
  if (!all(is.na(M[name,])==FALSE)){
    hasNas = c(hasNas, name);
  }
}

for (country in hasNas){
  validHDIs = M[country,!is.na(M[country,])];
  #noHDIs = M[country,is.na(M[country,])];
  firstValid = validHDIs[length(validHDIs)];
  M[country,is.na(M[country,])] = firstValid;
}


combine = function(...){
  paste0("", levels(interaction(..., sep=" ")))
}


res = combine(years, ordered_countries)

res = apply(expand.grid(ordered_countries, years), 1, function(x) paste0(x, collapse=" "))
t = matrix(unlist(res), nrow=188, ncol=30)

fig = plot_ly(z = ~M) %>% add_surface(
  colorbar = list(title = "HDI"),
  text=t,
  hoverinfo="z+text",
  contours=list(
    x=list(highlight=FALSE),
    z=list(highlight=FALSE)
  )
);
fig = fig %>% layout(scene=list(
  yaxis = list(
    title="Country",
    ticktext=as.list(ordered_countries)[seq(1, length(ordered_countries), 8)],
    tickvals=seq(1, length(ordered_countries), 8),
    tickmode="array"
  ),
  
  xaxis = list(
    title="Year",
    text=as.list(years),
    ticktext=as.list(years[seq(1,length(years),2)]),
    tickvals=seq(0, length(as.list(years)), 2),
    tickmode="array"
  ),
  zaxis = list(
    title="HDI"
  ),
  aspectmode='manual',
  aspectratio=list(
    x=2,
    y=3,
    z=1
  )
)
);
fig

hasNas = c()

for (name in rownames(M)){
  if (!all(is.na(M[name,])==FALSE)){
    hasNas = c(hasNas, name);
  }
}
