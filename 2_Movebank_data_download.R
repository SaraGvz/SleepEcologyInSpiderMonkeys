#### Downloading acc data from movebank ####

library(getPass)  
install.packages("move")
library(move)

#store password for movebank
pass <- getPass::getPass() ##keep password confidential

# read in individuals names
names=read.csv('~/Documents/FFT_Animal_Names.csv') #wd of the list with the animals names you will use
names=unique(names$individual.local.identifier)
names=strsplit(names, " ")
names=do.call(rbind,names)
names=names[,1]

#store the login for movebank
loginStored <- movebankLogin(username="Username", password=pass) #individual username from movebank account

#create dataframe for acc data 
accdata=c()

#download data from movebank
for(i in 1:length(names$Individual)){
  accdata[i]<-try(list(getMovebankNonLocationData(study=468460067 ,
                                                 sensorID="Acceleration", 
                                                 animalName=names$Individual[i], 
                                                 login=loginStored)
                      )
      )
  if(class(accdata[[i]])=="try-error"){
    next
    }
}

#store the data 
accdata
accdata2=plyr::ldply(accdata, data.frame)
accdata2$ID=paste(accdata2$individual_local_identifier, accdata2$tag_local_identifier, sep="_")
