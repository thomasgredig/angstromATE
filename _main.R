pathData = "/Users/gredigcsulb/Library/CloudStorage/OneDrive-SharedLibraries-csulb/Gredig Molecular Thin Film Lab - Erin - Erin/RAW/ATE"
fileList = dir(pathData, pattern='csv$', recursive=TRUE)
fileList = file.path(pathData, fileList)



for(f in fileList) {
  file.exists(f)

  d = ATEimport(f)

  cat("Filename: ",basename(f),"\n")
  cat("Base Pressure: ",min(d$ChamberPressure)," torr\n")
  cat("Deposition Points:",length(m1),"\n")
  cat("Max Thickness Sensor2: ", max(d$Sensor.2.Thickness), "\n")
  cat("Date: ",d$Date[1],"\n\n")
}

