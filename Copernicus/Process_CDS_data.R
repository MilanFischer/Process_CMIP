library(terra)
library(ncdf4)

# Create the directory, suppressing warnings if it already exists
dir.create("./out", showWarnings = FALSE)

rm(list = ls()); gc()

Project <- "CMIP5"
Period <- "historical" # "historical", "RCP85"
Var <- "tas"
Domain <- ext(c(9, 21, 47, 54))

List_of_models <- read.table("../Inputs/model_names.csv", header = TRUE, sep = ",")

unzip_files = FALSE

rm_gc <- function(x){
  rm(x)
  gc()
}

# Unzip the downloaded files if needed
if(unzip_files == TRUE){
  Files <- list.files(path =  paste0(Project, "/", Period, "/", Var), pattern = "\\.zip$")
  
  # Unzip the file within a loop
  for(i in 1:length(Files)){
    unzip(paste0(Project, "/", Period, "/", Var, "/", Files[i]), exdir = paste0(Project, "/", Period, "/", Var))
  }
  rm(Files)
}

# Extract models within project from the list of names
ID <- which(List_of_models$Project == Project)
Model <- List_of_models$Model[ID]

# Create an empty data.frame with 12 rows and M columns, filled with NA
Data_out <- as.data.frame(matrix(NA, nrow = 12, ncol = length(Model)))
colnames(Data_out) <- List_of_models$Abbreviation[ID]

# Loop through the models
for(M in 1:length(Model)){
  # Load and crop
  Files <- list.files(
    path = paste0(Project, "/", Period, "/", Var),
    pattern = paste0(gsub("-", "[-]?", Model[M]), ".*\\.nc$"),
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if(length(Files) > 0){
    
    # Load the data
    Data <- rast(Files)
 
    # This is to ensure that extent is defined (in some cases terra is not able to get it)
    nc <- nc_open(Files[1])
    lat <- ncvar_get(nc, "lat")
    lon <- ncvar_get(nc, "lon")
    nc_close(nc)
    
    x_res <- lon[2] - lon[1]
    y_res <- lat[2] - lat[1]
    
    ext(Data) <- ext(min(lon)-x_res/2, max(lon)+x_res/2, min(lat)-y_res/2, max(lat)+y_res/2)
    
    Data_c <- crop(Data, ext(c(Domain[1] - 0.00001, Domain[2] + 0.00001,
                               Domain[3] - 0.00001, Domain[4] + 0.00001)), snap="near")
    
    # Data_c <- crop(Data, ext(c(Domain[1] + x_res/2, Domain[2] - x_res/2,
    #                          Domain[3]+ y_res/2, Domain[4] - y_res/2)), snap="out")
    rm_gc(Data)
    
    # Apply temporal subset
    if(Period == "historical"){
      START <- which(time(Data_c) == "1981-01-16")
      END <- which(time(Data_c) == "2005-12-16")
    }else if(Period == "RCP85"){
      START <- which(time(Data_c) == "2076-01-16")
      END <- which(time(Data_c) == "2100-12-16")
    }

    if(length(END)==0){
      END <- time(Data_c)[length(time(Data_c))]
      cat(paste0("END set to ", END, "\n"))
    }
    
    Data_c_s <- Data_c[[START:END]]
    cat(paste0("Number of timestamps is ", nlyr(Data_c_s), "\n"))
    rm_gc(Data_c)
    
    for(i in 1:nrow(Data_out)){
      Data_out[i, M] <- mean(values(mean(Data_c_s[[seq(from = i, by = 12, length.out = 25)]])))
      # time(Data_c_s[[seq(from = i, by = 12, length.out = 25)]])
    }
  }
}

write.table(Data_out, paste0("out/", Project, "_", Period, "_", Var, ".csv"), sep = ",", row.names = FALSE)

