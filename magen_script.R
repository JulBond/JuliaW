library(dplyr)
library(ggplot2)
library(mapplots)
#library(plyr)
library(FSA)


path <- "D:/Matthias";

magen <- read.csv2(paste0(path,"/magen_data.csv"),sep=",");

magen <- subset(magen, Year==2018 & !is.na(Fish_Length_cm));

delta <- 5;

magen$Fish_Length_cm <- as.numeric(as.character(magen$Fish_Length_cm));
magen$Scientific_Name <- ifelse(is.na(as.character(magen$Scientific_Name)), "UNKNOWN",
as.character(magen$Scientific_Name));

magen$Ser_No_Fish <- as.character(magen$Ser_No_Fish);

#magen <- subset(magen, Scientific_Name!="UNKNOWN");

categ <- sort(as.character(unique(magen$Scientific_Name)));

magen$Length_class_5 <- lencat(magen$Fish_Length_cm, w=delta, startcat=0);

magen$Latitude_dd <- as.numeric(as.character(magen$Latitude_dd));
magen$Longitude_dd <- as.numeric(as.character(magen$Longitude_dd));

magen$RECTANGLE <- ices.rect2(magen$Longitude_dd,magen$Latitude_dd);

#p <- ggplot(data.frame(magen), aes(x=Scientific_Name)) +
#facet_wrap( ~ Length_class_5) +
#geom_bar(colour="black",fill="turquoise") + 
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#plot(p)


frequency_food_item_1_orig <- 
data.frame(magen %>% group_by(Length_class_5, Scientific_Name) %>% dplyr::summarise(Number_of_stomachs_with_prey_LC_orig = n()));

frequency_food_item_2_orig <- 
data.frame(magen %>% group_by(Length_class_5) %>% dplyr::summarise(Number_of_stomachs_LC_orig = n()));

frequency_food_item_orig <- merge(frequency_food_item_1_orig, frequency_food_item_2_orig, by="Length_class_5",all.x=TRUE);

frequency_food_item_orig$Percentage_of_stomachs_with_prey_LC_orig <- round(frequency_food_item_orig$Number_of_stomachs_with_prey_LC/frequency_food_item_orig$Number_of_stomachs_LC * 100, 2);




#######################  Bootstrap  ##########################

Fish_unique <- unique(magen$Ser_No_Fish);

B = 1000;
n = length(Fish_unique);

boot.samples = matrix(sample(Fish_unique, size = B * n, replace = TRUE), B, n);



#### Frequency of occurrence ####


U <- frequency_food_item_orig[,c("Scientific_Name","Length_class_5","Percentage_of_stomachs_with_prey_LC_orig")];

for (b in 1:nrow(boot.samples))
{
magen_boot <- c();
sub_s <- lapply(boot.samples[b,],function(x) {M <- subset(magen, Ser_No_Fish==x); return(M)});


magen_boot <- data.frame(data.table::rbindlist(sub_s));
#magen_boot <- dplyr::bind_rows(sub_s);
#magen_boot <- ldply(sub_s, rbind);
#magen_boot <- rbind.fill(sub_s);
#magen_boot <- fastDoCall(rbind, sub_s, quote=TRUE);
#magen_boot <- do.call(rbind, sub_s, quote=TRUE);



frequency_food_item_1 <- data.frame(magen_boot %>% group_by(Length_class_5, Scientific_Name) %>% dplyr::summarise(Number_of_stomachs_with_prey_LC = n()));
frequency_food_item_2 <- data.frame(magen_boot %>% group_by(Length_class_5) %>% dplyr::summarise(Number_of_stomachs_LC = n()));

frequency_food_item <- merge(frequency_food_item_1, frequency_food_item_2, by="Length_class_5",all.x=TRUE);

frequency_food_item$Percentage_of_stomachs_with_prey_LC <- round(frequency_food_item$Number_of_stomachs_with_prey_LC/frequency_food_item$Number_of_stomachs_LC * 100, 2);


FF <- merge(frequency_food_item_orig,frequency_food_item,by=c("Scientific_Name","Length_class_5"),all.x=TRUE);
names(FF)[which(names(FF)=="Percentage_of_stomachs_with_prey_LC")] <- "Percentage_boot";


U <- merge(U, FF[,c("Scientific_Name", "Length_class_5","Percentage_boot")], by=c("Length_class_5","Scientific_Name"),all.x=TRUE);

names(U)[which(names(U)=="Percentage_boot")] <- paste0("Percentage_boot_",b);

}

quantile_fun <- function(x, probs){
  if (anyNA(x)){
    return(quantile(x[!is.na(x)], probs = probs))
  } else {
    return(quantile(x, probs = probs))
  }
}

for (i in 1:nrow(U))
{
U$lower_boot_quantile[i] <- quantile_fun(as.numeric(U[i,4:ncol(U)]), probs=c(0.025));
U$upper_boot_quantile[i] <- quantile_fun(as.numeric(U[i,4:ncol(U)]), probs=c(0.975));
}

print(U)
Res_boot <- U[,c("Length_class_5","Scientific_Name","Percentage_of_stomachs_with_prey_LC_orig","lower_boot_quantile","upper_boot_quantile")];

Res_boot <- Res_boot[order(Res_boot$Length_class_5),];

write.csv2(Res_boot, file=paste0(path,"\\bootstrap_intervals.csv"), row.names=FALSE);

