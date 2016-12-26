#big-data-revolution-r-enterprise-tutorial

??rxGetOption
# Declare the file paths for the csv and xdf files
myAirlineCsv <- file.path(rxGetOption("sampleDataDir"), "2007_subset.csv")
myAirlineXdf <- "2007_subset.xdf"

# Use rxImport to import the data into xdf format
rxImport(inData = myAirlineCsv, outFile = myAirlineXdf, overwrite = TRUE)
list.files()

## Calculate an additional variable: airspeed (distance traveled / time in the air). 
rxDataStep(inData = myAirlineXdf, 
           outFile = myAirlineXdf, 
           varsToKeep = c("AirTime", "Distance"),
           transforms = list(airSpeed = Distance / AirTime),
           append = "cols",
           overwrite = TRUE)

# Get Variable Information for airspeed
rxGetInfo(data = myAirlineXdf, 
          getVarInfo = TRUE,
          varsToKeep = "airSpeed")

# Summary for the airspeed variable
rxSummary(~airSpeed, 
          data = myAirlineXdf)

# Construct a histogtam for airspeed
rxHistogram(~airSpeed, 
            data = myAirlineXdf
)

# We can use additional arguments to limit the X-axis.
rxHistogram(~airSpeed, 
            data = myAirlineXdf,
            xNumTicks = 10,
            numBreaks = 1500,
            xAxisMinMax = c(0,12)
)