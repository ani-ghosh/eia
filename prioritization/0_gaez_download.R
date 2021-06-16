# https://gaez.fao.org/pages/data-viewer
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/LR/aez/aez_v9v2red_5m_CRUTS32_Hist_8110_100_avg.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/LR/soi1/hwsd_domi_30s.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/LR/lco/GLCSv11_01_5m.tif
# 
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/IPSL-CM5A-LR/rcp6p0/2020sH/teas200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/GFDL-ESM2M/rcp4p5/2050sH/bana200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/GFDL-ESM2M/rcp4p5/2050sH/bana200b_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/GFDL-ESM2M/rcp2p6/2020sH/alfa200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/GFDL-ESM2M/rcp2p6/2080sH/alfa200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/6190H/alfa200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/7100H/alfa200a_yld.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_yld.tif
# 
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_fc1.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_fc2.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_fc0.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_cbd.tif
# https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/res02/CRUTS32/Hist/8110H/alfa200a_Tsc.tif

# Theme 1: Land and Water Resources
# Theme 2: Agro-climatic Resources
# Theme 3: Agro-climatic Potential Yield
# Theme 4: Suitability and Attainable Yield
# Theme 5: Actual Yields and Production
# Theme 6: Yield and Production Gaps

baseurl <- "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org"
imageserver <- c("LR","res01", "res02", "res05", "res06", "res07")

# Theme 3: Agro-climatic Potential Yield
imgs <- "res02"
# yield -> yld 
# constraining factors: temp, moisture, "agro-climatic", "total climate" 
vars <- c("yld", "fc1", "fc2", "fc0", "cbd", "Tsc") 
cds <- c("CRUTS32","GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC-ESM-CHEM", "NorESM1-M")
periodhist <- c("6190H","7100H","8110H")
periodfut <-  c("2020sH","2050sH","2080sH")
rcp <- c("Hist",paste0("RCP", c("2.6", "4.5", "6.0", "8.5")))
water <- c("a", "b") # irrigation/no_irrigation
https://gaez.review.fao.org/server/rest/services/res06/ImageServer/identify?geometry=%7B%22x%22%3A-11046067.831544453%2C%22y%22%3A4451692.527327482%2C%22spatialReference%22%3A%7B%22wkid%22%3A102100%2C%22latestWkid%22%3A3857%7D%7D&geometryType=esriGeometryPoint&mosaicRule=%7B%22mosaicMethod%22%3A%22esriMosaicLockRaster%22%2C%22where%22%3A%22OBJECTID+in+%281249%29%22%2C%22ascending%22%3Afalse%2C%22lockRasterIds%22%3A%5B1249%5D%7D&renderingRule=&renderingRules=&pixelSize=&time=&returnGeometry=false&returnCatalogItems=true&returnPixelValues=false&maxItemCount=1&f=html