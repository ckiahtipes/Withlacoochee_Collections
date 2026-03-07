###ARCHIVED CODE###

#Withlacoochee Data Exploration

WTL_sp <- read.csv("WTL_specimens.csv", header = TRUE)

par(mar = c(5, 6, 4, 2) + 0.1)

barplot(table(WTL_sp$county), horiz = TRUE, las = 1, xlim = c(-50,600))

title(main = "N 'Withlacoochee' Specimens in USF Herbarium by County")

par(mar = c(5, 4, 4, 2) + 0.1)

collector = table(WTL_sp$collector_Text)

par(mar = c(5, 8, 4, 2) + 0.1)

barplot(collector[order(collector)], horiz = TRUE, las = 1, main = "N 'Withlacoochee' Specimens in USF Herbarium by Collector", cex.axis = 0.8)

par(mar = c(5, 8, 4, 2) + 0.1)

barplot(table(WTL_sp$genus), horiz = TRUE, las = 1, main = "N Withlacoochee Specimens by Genera")

par(mar = c(5, 4, 4, 2) + 0.1)

#Okay, time for some more serious aggregation work. Need Family, Genus, Species, Collector, and Collection Date.

WTL_sp <- read.csv("WTL_specimens.csv", header = TRUE) #Reading "Withlacoochee" as Location
WSF_sp = read.csv("WSF_specimens.csv", header = TRUE) #Reading "Withlacoochee State Forest as Location"

comb_sp = rbind(WTL_sp, WSF_sp)

doubles_check = table(comb_sp$barcode)

cut_list = doubles_check[doubles_check > 1]

for(i in 1:length(cut_list)){
  cut = grep(names(cut_list[i]), comb_sp$barcode)
  #print(paste0("Cutting duplicate specimen ", names(cut_list[i])))
  comb_sp = comb_sp[-c(cut[1], cut[2]),]
  pull = WTL_sp[WTL_sp$barcode == as.numeric(names(cut_list[i])),]
  #print(paste0("Replacing with single instance of ",WTL_sp$genus[WTL_sp$barcode == as.numeric(names(cut_list[i]))], " ", WTL_sp$species[WTL_sp$barcode == as.numeric(names(cut_list[i]))]))
  comb_sp = rbind(comb_sp, pull)
}

#This section pulls info for use later...but what for? ###FLAGGED

WTL_taxa = data.frame(comb_sp$family, comb_sp$genus, comb_sp$species)

WTL_dates = comb_sp$Collection.Date

coll_dates = as.Date(comb_sp$Collection.Date, format = "%m/%d/%Y")
coll_yr = as.Date(coll_dates, format = "%Y")

#We've extracted collection years, going to go back and backfill some of the shitty data.
#Should probably do this after we've got an official export from the database.

genus = WTL_taxa$comb_sp.genus
specs = WTL_taxa$comb_sp.species
epithet = vector("character", length(genus))

for(i in 1:length(epithet)){
  epithet[i] = paste0(genus[i]," ",specs[i])
}

year = format(coll_dates, '%Y')
month = format(coll_dates, '%m')

WTL_data = data.frame(WTL_taxa, epithet, coll_dates, year, month, WTL_sp$LatDecL, WTL_sp$LongDecL, WTL_sp$county, row.names = WTL_sp$barcode)

#Let's winnow data by counties

WTL_data = WTL_data[WTL_data$WTL_sp.county == "Pasco Co." |
                      WTL_data$WTL_sp.county == "Hernando Co." |
                      WTL_data$WTL_sp.county == "Sumter Co." |
                      WTL_data$WTL_sp.county == "Citrus Co." |
                      WTL_data$WTL_sp.county == "Lake Co.", ]

#Okay now we have new cleaner dataframe with better tracking of names, families, dates, and locations.

#Wonder if we can use families to assign some other broader categories. Going to write file and do this manually. <--- Don't be stupid, get it from the atlas.

family_classes = read.csv("Families_index.csv", header = TRUE) #This list has families capitalized, need all caps.

# Source - https://stackoverflow.com/a/16516747
# Posted by juba
# Retrieved 2026-02-08, License - CC BY-SA 3.0

#data.frame(lapply(df, function(v) {
#  if (is.character(v)) return(toupper(v))
#  else return(v)
#}))

WTL_FAMILY = data.frame(sapply(WTL_data$comb_sp.family, function(x){
  if(is.character(x)) return(toupper(x))
  else return(x)
}))

WTL_data$comb_sp.family = WTL_FAMILY #Replace existing family names with caps

colnames(WTL_data[,1]) = "comb_sp.family" #Replace with old name.

Order = vector("character", nrow(WTL_data))
Class = vector("character", nrow(WTL_data))

for(i in 1:length(Order)){
  grab = WTL_data$comb_sp.family[i]
  Order[i] = family_classes$Order[family_classes$Name == grab]
  Class[i] = family_classes$Group.Name[family_classes$Name == grab]
}

WTL_data = cbind(WTL_data, Order, Class)

WTL_orders = unique(WTL_data$Order)
WTL_groups = unique(WTL_data$Class)

groupColors = vector("character", nrow(WTL_data))
groupPalette = c("darkgreen", "gold", "lightblue", "orange")

orderColors = vector("character", nrow(WTL_data))

for(i in 1:length(WTL_groups)){
  groupColors[WTL_data$Class == WTL_groups[i]] = groupPalette[i]
}

#Let's start with collections by family.

familyColors = colorRampPalette(c("darkgreen","gold"))
familyColors = colorRampPalette(c("red", "orange", "yellow", "green", "blue", "purple", "violet"))

family_count = table(WTL_data$comb_sp.family)
barplot(family_count[order(family_count)], 
        horiz = TRUE, 
        las = 1, 
        cex.axis = 0.5, 
        cex = 0.5, 
        width = 25, 
        main = "N Specimens by Family from Withlacoochee State Forest", 
        col = groupColors[order(family_count)])



#Okay what I see here is that we've got some REALLY abundant stuff and a lot of not-so-abundant stuff, typical of biodviersity, no?
#Let's look into what's only been collected once and things collected twice.

species_count = table(WTL_data$epithet)
singletons = species_count[species_count == 1]

single_names = names(singletons)

WTL_singletons = as.data.frame(matrix(nrow = length(single_names), ncol = ncol(WTL_data)))
WTL_srownames = vector("numeric", nrow(WTL_singletons))


for(i in 1:length(single_names)){
  WTL_singletons[i, ] = WTL_data[WTL_data$epithet == single_names[i], ]
  WTL_srownames[i] = row.names(WTL_data[WTL_data$epithet == single_names[i], ])
}

colnames(WTL_singletons) = colnames(WTL_data)
row.names(WTL_singletons) = WTL_srownames

#Create pch differences based on counties.

county_list = unique(WTL_data$WTL_sp.county)
county_symb = vector("numeric", nrow(WTL_singletons))

for(i in 1:length(county_list)){
  county_symb[WTL_singletons$WTL_sp.county == county_list[i]] = i+20
}

set_order = order(WTL_singletons$WTL_sp.county)

WTL_singletons = WTL_singletons[set_order,]

par(mar = c(5, 8, 4, 2) + 0.1)

plot(WTL_singletons$year[set_order], 
     1:length(WTL_singletons$year), 
     axes = FALSE, ann = FALSE, 
     xlim = c(1950,2030),
     pch = county_symb[set_order],
     bg = groupColors[set_order],
     cex = 0.75)

axis(1, at = seq(1950,2030,10))
axis(2, at = c(1:nrow(WTL_singletons)), labels = WTL_singletons$epithet, cex.axis = 0.5, las = 1, tick = FALSE)
title(main = "Single Collections from Withlacoochee State Forest")

par(mar = c(5, 4, 4, 2) + 0.1)

#Buildout for modifying the previous plot. Let's try separating by major groups.

par(mar = c(5, 8, 4, 2) + 0.1, mfrow = c(4,1))

for(i in 1:length(WTL_orders)){
  group_pull = WTL_singletons[WTL_singletons$Order == WTL_orders[i],]
  if(nrow(group_pull) == 0){
  } else {
    plot(group_pull$year,
         1:length(group_pull$year),
         xlim = c(1950,2030),
         axes = FALSE, ann = FALSE,
         pch = 22, 
         bg = "darkgreen")
    
    axis(1, at = seq(1950,2030,10))
    axis(2, at = c(1:nrow(group_pull)), labels = group_pull$epithet, cex.axis = 0.5, las = 1, tick = FALSE)
    
  }
  
}

par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))

#Let's subset it by counties and plot it.

par(mfrow = c(1,1))

for(i in 1:length(county_list)){
  par(mar = c(5, 8, 4, 2) + 0.1)
  
  sngbycnty = WTL_singletons[WTL_singletons$WTL_sp.county == county_list[i],]
  grp_col = groupColors[WTL_singletons$WTL_sp.county == county_list[i]]
  
  plot(sngbycnty$year, 
       1:length(sngbycnty$year), 
       axes = FALSE, ann = FALSE, 
       xlim = c(1950,2030),
       pch = 21,
       bg = grp_col,
       cex = 0.75)
  
  axis(1, at = seq(1950,2030,10))
  axis(2, at = c(1:nrow(sngbycnty)), labels = sngbycnty$epithet, cex.axis = 0.5, las = 1, tick = FALSE)
  title(main = paste0("Single Collections from Withlacoochee State Forest: ", county_list[i]))
}

par(mfrow = c(1,1))

#Another basic question - does the number of singletons follow the number of collections in general?

cnty_single = table(WTL_singletons$WTL_sp.county)
cnty_totals = table(WTL_data$WTL_sp.county)

cnty_cmb = rbind(cnty_single, cnty_totals)

barplot(cnty_cmb, horiz = TRUE, las = 1, beside = TRUE, main = "Total N Collected vs. Singletons at WTL")

#You bet your ass they do. Okay, let's break it out by class.

#Let's lose Lake Co., which isn't adding much and makes the next steps harder.

WTL_data = WTL_data[WTL_data$WTL_sp.county != "Lake Co.",]

WTL_classes = unique(WTL_data$Class)

WTL_biclass = sapply(WTL_classes, function(x){
  table(WTL_data$WTL_sp.county[WTL_data$Class == x])
})

WTL_biclass = as.data.frame(WTL_biclass)

barplot(t(WTL_biclass), beside = TRUE, horiz = TRUE, las = 1, main = "N Collected by Class WTL", col = c("gold","darkorange","darkgreen","purple"))
legend(250, 5, WTL_classes, pt.bg = c("gold","darkorange","darkgreen","purple"), pch = c(rep(22, length(WTL_classes))))

#The more species there are, the more are collected. Similar shapes in the barplots point to a shared underlying pattern.

#Think the message here is that you need to compare WTL with county-level data and we need to build this from the ground up in a new document/project.





