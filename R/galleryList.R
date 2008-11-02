#!/usr/bin/Rscript

# this program generates an xml file listing the contents of a directory containing photos 

args = commandArgs(trailingOnly=T)

args = c('carolan','flextravel')

ngal = length(args)

paths = paste('galleries/',args,sep='')
nameFiles = system('ls data/*names.txt',inter=T)
galdesFiles = system('ls data/*galleryDescription.txt',inter=T)
photodesFiles = system('ls data/*photoDescription.txt',inter=T)


fileNames = vector("list",ngal)
names = vector("list",ngal)
galdes = vector("list",ngal)
photodes = vector("list",ngal)
nfiles = 0

for(i in 1:ngal){
        
        #get the names of the photos    
    fileNames[[i]] = system(paste('ls',paths[i]),inter=T)
    fileNames[[i]] = paste(args[i],fileNames[[i]],sep='/')

        #how many are there
    nfiles[i] = length(fileNames[[i]])

        #Nmaes of the description files for this gallery 
    thisGalDesFile = paste('data/',args[i],'_galleryDescription.txt',sep='')
    thisNameFile = paste('data/',args[i],'_names.txt',sep='')
    thisPhotoDesFile = paste('data/',args[i],'_photoDescription.txt',sep='')

        #get the description of this gallery
    galdes[[i]] = scan(thisGalDesFile, what="character", sep='\n')

        #get the name descriptions of each of the files and if it is not there fill it in
    names[[i]] = scan(thisNameFile, what="character", sep='\n')
    n = length(names[[i]])
    if( n < nfiles[i] ){
        names[[i]] = c(names[[i]],paste('Photo',(n+1):nfiles[i]))
    }
    
        #get the full descriptions of the individual photos 
    photodes[[i]] = scan(thisPhotoDesFile, what="character", sep='\n')
    n = length(photodes[[i]])
    if( n < nfiles[i] ){
        photodes[[i]] = c(photodes[[i]],paste('This is Photo',(n+1):nfiles[i]))
    }

}


con <- xmlOutputDOM(tag='galleries')

for(i in 1:ngal){

    con$addTag("gallery", attrs=c(id=args[i]), close=FALSE)
    con$addTag("description", galdes[[i]]) 

    for(j in 1:nfiles[i]){
        con$addTag("photo",  close=FALSE)
            con$addTag("name", names[[i]][j])
            con$addTag("description", photodes[[i]][j] )
            con$addTag("source", fileNames[[i]][j] )
        con$closeTag() # photo
    }
    
    con$closeTag() # gallery
}

saveXML(con$value(), file="data/galleries.xml")




