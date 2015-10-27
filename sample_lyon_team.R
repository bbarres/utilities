###############################################################################
###############################################################################
#Script for sampling team for scientific seminar
###############################################################################
###############################################################################


#We want to randomly select the team
list_labo<-c("Animalerie","AVB","Epidemio","MND","Mycoplasmes","RPP","UCAS",
             "Virologie")
sample(list_labo,8)

#if some teams have already pick a slot, you can remove them from the list
sample(list_labo[!list_labo %in% c("RPP","AVB")],6)


###############################################################################
#END
###############################################################################