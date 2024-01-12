################################################################################
# TITLE: Other_Functions
# DESCRIPTION: Additional functions to use, not related to plotting.
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: August 29, 2023
################################################################################

################################################################################
## FUNCTION: packs_check
## Checks if packages are installed, installs them if not, and loads required functions
##
## INPUTS: 
##    packs_to_load - List of all packages needed
################################################################################

#Call function and pass package names through
packs_check <- function(packs_to_load) {

  #check for package and require it, 
  package.check <- lapply(
    packs_to_load,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )
}

################################################################################
## FUNCTION: imsave_git & imsave_loc
## Save most recent plot to git folder or local folder with transparent background.
##
## INPUTS: 
##    name - Date to plot, the week will start on the day chosen. Enter as "name"
################################################################################
# Save to a git folder
imsave_git <- function(name) {
  ggsave(plot=last_plot(),path = here("Figures"), 
         filename = paste(name,".png", sep = ""),
         width = 12, height=8, units=c("cm"),dpi=300, bg = "transparent")  }

# Save to a loacl folder that is ignored by git
imsave_loc <- function(name) {
  ggsave(path = here("Figures (Local)"), 
         filename = paste(name,".png", sep = ""),
         width = 14, height=10, units=c("cm"),dpi=300, bg = "transparent")}

################################################################################
## FUNCTION: SaveRun_Loc
## Saves all plots to a new folder names after case
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
#NEEDS WORK
 SaveRun_Loc <- function(CaseName,FileName) {

  # Set up folder if it does not exist
  fold_name<-paste(CaseName,SourceDB)

  # Check if folder exists, if not, make one
  if (file.exists(here("Figures (Local)",paste(fold_name)))) {

    cat("The folder exists\n")

  } else {

    # Create the folder
    FoldLocation <-
    dir.create(here("Figures (Local)",paste(fold_name)))

  }

  # Create file name
  FileName <-paste(FileName,SourceDB)
  
# Save to a local file as exactly what is shown on the windows()
  savePlot(
    filename = here(paste("Figures (Local)/",paste(fold_name),"/",FileName,".png", sep = "")),
    type = "png",
    device = dev.cur())
  
}
  
################################################################################
## FUNCTION: SaveIm
## Saves all plots to a new folder names after case
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
#NEEDS WORK
SaveIm <- function(CaseName,FileName) {
  
  # Set up folder if it does not exist
  fold_name<-paste(CaseName)
  
  # Check if folder exists, if not, make one
  if (file.exists(here("Figures",paste(fold_name)))) {
    
    cat("The folder exists\n")
    
  } else {
    
    # Create the folder
    FoldLocation <-
      dir.create(here("Figures",paste(fold_name)))
    
  }
  
  # Create file name
  FileName <-paste(FileName)
  
  # Save to a local file as exactly what is shown on the windows()
  savePlot(
    filename = here(paste("Figures/",paste(fold_name),"/",FileName,".png", sep = "")),
    type = "png",
    device = dev.cur())
  
}

################################################################################
## FUNCTION: GGSave_Simple
## Saves all plots to figures folder
##
## INPUTS: 
##    CaseName - name for folder. Enter as "folder"
##    FileName - Name for image. Enter as "name"
################################################################################
GGSave_Simple <- function(FileName,plotinput,w,h) {

  # Save to a local file 
  ggsave(
    filename = here(paste("Figures/",FileName,".png", sep = "")),
    device = "png",
    plot = plotinput,
    width=w,
    height=h,
    dpi=300)
  
}