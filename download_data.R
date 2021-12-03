### DOWNLOAD AND UNZIP THE DATA

filename <- "Avtivity monitoring data.zip"

# download dataset and unzip files
if (!file.exists(filename)) {
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileURL, filename, method = "curl")
}

# unzip file
if (!file.exists("Activity monitoring data")) {
    unzip(filename)
}
