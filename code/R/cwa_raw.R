library(tidyverse)
library(bitops)
library(progress)
CWA_HEADER_METADATA = 0x444D
CWA_HEADER_ACCELEROMETER = 0x5841

HEADER_USAGEBLOCK = 0x4255
HEADER_UNDEFINED_BLOCK = 0xFFFF

xx=c()
cwa_init_data_structure <- function(){
  return (list(md=0,ax=0,ud=0,other=0,data=tibble()))
}

cwa_print_count <- function(cwa_dta){
  cat("MD Buffers:",cwa_dta$md,"AX Buffers:",cwa_dta$ax,"UD Buffers:",cwa_dta$ud,"other buffers:",cwa_dta$other)
}

cwa_unpack_sample <- function(v){
    get10bit2sComplement<-function(x){
      return (bitwAnd(bitwNot(x)+1,0x3FF) )
    }
    get10bit<-function(x,shift,e){
      bitwShiftR(x,shift) %>% 
        bitAnd(0x3FF) %>%
        {ifelse(.<0x300,bitShiftL(.,e) ,-bitShiftL(get10bit2sComplement(.),e))} 
    }
    
    e=bitShiftR(v,30)
    x=get10bit(v,0,e)
    y=get10bit(v,10,e)
    z=get10bit(v,20,e)
    return( c(x=x/256,y=y/256,z=z/256))
}
#uint16_t packetHeader;                      ///< @ 0  +2   ASCII "MD", little-endian (0x444D)
#uint16_t packetLength;                      ///< @ 2  +2   Packet length (1020 bytes, with header (4) = 1024 bytes total)
#uint8_t  hardwareType;                      ///< @ 4  +1   Hardware type (0x00/0xff/0x17 = AX3, 0x64 = AX6)
#uint16_t deviceId;                          ///< @ 5  +2   Device identifier
#uint32_t sessionId;                         ///< @ 7  +4   Unique session identifier
#uint16_t upperDeviceId;                     ///< @11  +2   Upper word of device id (if 0xffff is read, treat as 0x0000)
#cwa_timestamp_t loggingStartTime;           ///< @13  +4   Start time for delayed logging
#cwa_timestamp_t loggingEndTime;             ///< @17  +4   Stop time for delayed logging
#uint32_t loggingCapacity;                   ///< @21  +4   (Deprecated: preset maximum number of samples to collect, should be 0 = unlimited)
#uint8_t  reserved1[1];                      ///< @25  +1   (1 byte reserved)
#uint8_t  flashLed;                          ///< @26  +1   Flash LED during recording
#uint8_t  reserved2[8];                      ///< @27  +8   (8 bytes reserved)
#uint8_t  sensorConfig;                      ///< @35  +1   Fixed rate sensor configuration, 0x00 or 0xff means accel only, otherwise bottom nibble is gyro range (8000/2^n dps): 2=2000, 3=1000, 4=500, 5=250, 6=125, top nibble non-zero is magnetometer enabled.
#uint8_t  samplingRate;                      ///< @36  +1   Sampling rate code, frequency (3200/(1<<(15-(rate & 0x0f)))) Hz, range (+/-g) (16 >> (rate >> 6)).
#cwa_timestamp_t lastChangeTime;             ///< @37  +4   Last change meta-data time
#uint8_t  firmwareRevision;                  ///< @41  +1   Firmware revision number
#int16_t  timeZone;                          ///< @42  +2   (Unused: originally reserved for a "Time Zone offset from UTC in minutes", 0xffff = -1 = unknown)
#uint8_t  reserved3[20];                     ///< @44  +20  (20 bytes reserved)
#uint8_t  annotation[OM_METADATA_SIZE];      ///< @64  +448 Scratch buffer / meta-data (448 characters, ignore trailing 0x20/0x00/0xff bytes, url-encoded UTF-8 name-value pairs)
#uint8_t  reserved[512];                     ///< @512 +512 (Reserved for device-specific meta-data in the same format as the user meta-data) (512 bytes)
cwa_md_buffer <- function(cwa_data,buffer){
  message("MD_BUFFER")
  cwa_data$md <- cwa_data$md + 1;
  
  cwa_data$packetHeader <- readBin(buffer[1:2],integer(),n = 1,size = 2,signed = FALSE,endian = "little")
  message(cwa_data$packetHeader==CWA_HEADER_METADATA)
  stopifnot(cwa_data$packetHeader == CWA_HEADER_METADATA)
  hardwareType <- readBin(buffer[5:6],integer(),n = 1,size = 1,signed = FALSE,endian = "little")
  cwa_data$hardwareType <- ifelse(hardwareType %in% c(0x00,0xFF,0x17),"AX3",ifelse(hardwareType==0x64,"AX6","unknown"))
  cwa_data$deviceId <- readBin(buffer[6:8],integer(),n = 1,size = 2,signed = FALSE,endian = "little")
  cwa_data$sessionId <- readBin(buffer[8:12],integer(),n = 1,size = 4,endian = "little")
  cwa_data$upperDeviceId <- readBin(buffer[12:14],integer(),n = 1,size = 2,signed = FALSE,endian = "little")
  cwa_data$loggingStartTime <- cwa_time_format(readBin(buffer[14:18],integer(),n = 1,size = 4,endian = "little"))
  cwa_data$loggingEndTime <- cwa_time_format(readBin(buffer[18:22],integer(),n = 1,size = 4,endian = "little"))
  cwa_data$flashLed <- readBin(buffer[27:28],integer(),n = 1,size = 1,signed = FALSE,endian = "little")
  cwa_data$sensorConfig_samplingRate <- readBin(buffer[36:38],integer(),n = 2,size = 1,signed = FALSE,endian = "little")
  cwa_data$lastChangeTime <- cwa_time_format(readBin(buffer[38:42],integer(),n = 1,size = 4,endian = "little"))
  cwa_data$firmwareRevision <- readBin(buffer[42:43],integer(),n = 1,size = 1,signed = FALSE,endian = "little")
  
  return(cwa_data)
}

##YYYYYYMM MMDDDDDh hhhhmmmm mmssssss 
cwa_time_format<-function(x){
  year = 2000 + bitwAnd(bitwShiftR(x,26),0x3F) ## year
  month = bitwAnd(bitwShiftR(x,22),0x0F) ## month (R: month 1->12)
  day =  bitwAnd(bitwShiftR(x,17),0x1F) ## day
  hour = bitwAnd(bitwShiftR(x,12),0x1F) ##(x >> 12) & 0x1F
  minute = bitwAnd(bitwShiftR(x,6),0x3F) ##(x >> 6) & 0x3F
  sec = bitwAnd(x,0x3F)
  
  date=as.POSIXct(paste(paste(year,month,day,sep="-"),paste(hour,minute,sec,sep = ":")))
  
  
  return(date)
}

view_bits <- function(x){
  as.integer(rev(intToBits(x)))
}

#uint16_t packetHeader;                      ///< @ 0  +2   ASCII "AX", little-endian (0x5841)	
#uint16_t packetLength;                      ///< @ 2  +2   Packet length (508 bytes, with header (4) = 512 bytes total)
#uint16_t deviceFractional;                  ///< @ 4  +2   Top bit set: 15-bit fraction of a second for the time stamp, the timestampOffset was already adjusted to minimize this assuming ideal sample rate; Top bit clear: 15-bit device identifier, 0 = unknown;
#uint32_t sessionId;                         ///< @ 6  +4   Unique session identifier, 0 = unknown
#uint32_t sequenceId;                        ///< @10  +4   Sequence counter (0-indexed), each packet has a new number (reset if restarted)
#cwa_timestamp_t timestamp;                  ///< @14  +4   Last reported RTC value, 0 = unknown
#uint16_t lightScale;                        ///< @18  +2   AAAGGGLLLLLLLLLL Bottom 10 bits is last recorded light sensor value in raw units, 0 = none; top three bits are unpacked accel scale (1/2^(8+n) g); next three bits are gyro scale (8000/2^n dps)
#uint16_t temperature;                       ///< @20  +2   Last recorded temperature sensor value in raw units (bottom 10-bits), 0 = none; (top 6-bits reserved)
#uint8_t  events;                            ///< @22  +1   Event flags since last packet, b0 = resume logging, b1 = reserved for single-tap event, b2 = reserved for double-tap event, b3 = reserved, b4 = reserved for diagnostic hardware buffer, b5 = reserved for diagnostic software buffer, b6 = reserved for diagnostic internal flag, b7 = reserved)
#uint8_t  battery;                           ///< @23  +1   Last recorded battery level in scaled/cropped raw units (double and add 512 for 10-bit ADC value), 0 = unknown
#uint8_t  sampleRate;                        ///< @24  +1   Sample rate code, frequency (3200/(1<<(15-(rate & 0x0f)))) Hz, range (+/-g) (16 >> (rate >> 6)).
#uint8_t  numAxesBPS;                        ///< @25  +1   0x32 (top nibble: number of axes, 3=Axyz, 6=Gxyz/Axyz, 9=Gxyz/Axyz/Mxyz; bottom nibble: packing format - 2 = 3x 16-bit signed, 0 = 3x 10-bit signed + 2-bit exponent)
#int16_t  timestampOffset;                   ///< @26  +2   Relative sample index from the start of the buffer where the whole-second timestamp is valid
#uint16_t sampleCount;                       ///< @28  +2   Number of sensor samples (if this sector is full -- Axyz: 80 or 120 samples, Gxyz/Axyz: 40 samples)
#uint8_t  rawSampleData[480];                ///< @30  +480 Raw sample data.  Each sample is either 3x/6x/9x 16-bit signed values (x, y, z) or one 32-bit packed value (The bits in bytes [3][2][1][0]: eezzzzzz zzzzyyyy yyyyyyxx xxxxxxxx, e = binary exponent, lsb on right)
#uint16_t checksum;                          ///< @510 +2   Checksum of packet (16-bit word-wise sum of the whole packet should be zero)

cwa_ax_buffer <- function(cwa_data,buffer){
  cwa_data$ax  <- cwa_data$ax + 1;
  packetHeader     <- readBin(buffer[ 1:2],character(),1,1,endian = "little", signed=FALSE)# [2] = 0x5841 (ASCII "AX", little-endian)
  packetLength     <- readBin(buffer[ 3:512],integer(),1,2,endian = "little", signed = FALSE) # [2] = 508 bytes (contents of this packet is 508 bytes long, + 2 + 2 = 512 bytes total)
  deviceFractional <- readBin(buffer[ 4:512],integer(),1,2,endian = "little", signed = FALSE) # [2] Top bit set: 15-bit fraction of a second for the time stamp, the timestampOffset was already adjusted to minimize this assuming ideal sample rate; Top bit clear: 15-bit device identifier, 0 = unknown; 
  sessionId        <- readBin(buffer[ 7:512],integer(),1,4,endian = "little")	# [4] (32-bit unique session identifier, 0 = unknown)
  sequenceId       <- readBin(buffer[11:512],integer(),1,4,endian = "little")	# [4] (32-bit sequence counter, each packet has a new number -- reset if restarted?)
  timestamp        <- readBin(buffer[15:512],integer(),1,4,endian = "little")	# [4] (last reported RTC value, 0 = unknown)
  light            <- readBin(buffer[19:512],integer(),1,2,endian = "little", signed = FALSE)	# [2] (last recorded light sensor value in raw units, 0 = none)
  temperature      <- readBin(buffer[21:512],integer(),1,2,endian = "little", signed = FALSE)	# [2] (last recorded temperature sensor value in raw units)
  events           <- readBin(buffer[23:512],integer(),1,2,endian = "little", signed = FALSE)	# [1] (event flags since last packet, b0 = resume logging from standby, b1 = single-tap event, b2 = double-tap event, b3-b7 = reserved)
  battery          <- readBin(buffer[24:512],integer(),1,2,endian = "little", signed = FALSE)	# [1] (last recorded battery level in 6/256V, 0 = unknown)
  sampleRate       <- readBin(buffer[25:512],integer(),1,2,endian = "little", signed = FALSE)	#  <was reserved> [1] = sample rate code (3200/(1<<(15-(rate & 0x0f)))) Hz, if 0, then old format where sample rate stored in 'timestampOffset' field as whole number of Hz
  numAxesBPS       <- readBin(buffer[26:512],integer(),1,1,endian = "little", signed = FALSE)	#  [1] = 0x32 (top nibble: number of axes = 3; bottom nibble: number of bytes per axis sample - 2 = 3x 16-bit signed, 0 = 3x 10-bit signed + 2-bit exponent)
  timestampOffset  <- readBin(buffer[27:512],integer(),1,2,endian = "little", signed = FALSE)	# <was sampleRate> [2] = [if sampleRate is non-zero:] Relative sample index from the start of the buffer where the whole-second timestamp is valid [otherwise, if sampleRate is zero, this is the old format with the sample rate in Hz]
  sampleCount      <- readBin(buffer[29:512],integer(),1,2,endian = "little", signed = FALSE)	# [2] = 80 samples (number of accelerometer samples)

  sampleRate <- 3200/bitwShiftL(15-bitwAnd(12362,0x0f),1)
  numAxes <- bitwShiftR(numAxesBPS,4)
  bytesPerAxis <- bitwAnd(numAxesBPS,0x0F)
#  message( as.hexmode( numAxesBPS) )
  
  if (bytesPerAxis==2){
    sampleData     <- readBin(buffer[31:512],integer(),240,2,endian = "little", signed = TRUE) # [sampleCount * numAxes * bytesPerSample = 480] (sample data)
    sampleData <- as_tibble(matrix(sampleData,ncol=3,byrow = T),.name_repair = ~c("X","Y","Z"))
  }else{
    # The format of the data is packed 10 bits per sample with 2 exponent bits...
    ##   3            2           1
    ## 2109 8765|4321 0987|6543 2109|8765 4321
    ## eezz zzzz|zzzz yyyy|yyyy yyxx|xxxx xxxx
    stopifnot(bytesPerAxis==0)
    
    sampleData     <- readBin(buffer[31:512],integer(),120,4,endian = "little") %>% map_df(cwa_unpack_sample)
  }
  checksum         <- readBin(buffer[511:512],integer(),1,2,endian = "little", signed = FALSE) #[2] = Checksum of packet (0 = either checksum is zero or no checksum is recorded)
  
  
  cwa_data$data <- bind_rows(cwa_data$data,sampleData)
  

  
  if (FALSE){
    cat(cwa_data$ax,"\npacketHeader",packetHeader,"packetLength:",packetLength,"device fractional:",deviceFractional,"sessionId:",sessionId,"sequenceId",sequenceId,"\n",
        "timestamp",timestamp,"light",light,"temperature",temperature,"events",events,"battery",battery,"\n",
        "sampleRate",sampleRate,"numAxes",numAxes,"Bytes/axis",bytesPerAxis,"timestampOffset",timestampOffset,"sampleCount",sampleCount,"\n") 
  }
  return(cwa_data)
}

test_one_buffer<-function(cwa_file,expected){
  con <- file(cwa_file,"rb")
  hbuff <- readBin(con,raw(),1024,1)
  buffer <- readBin(con,raw(),512,1)
  close(con)
  
  cwa_data <- cwa_init_data_structure() %>% cwa_md_buffer(hbuff) %>% cwa_ax_buffer(buffer)
  all( expected$data$X[1:120] == cwa_data$data$x, expected$data$Y[1:120] == cwa_data$data$y,expected$data$Z[1:120] == cwa_data$data$z )
  
}
test_one_buffer("~/CWAfiles/1987073_90001_0_0.cwa",old)


test_ten_buffers<-function(cwa_file,expected){
  con <- file(cwa_file,"rb")
  hbuff <- readBin(con,raw(),1024,1)
  cwa_data <- cwa_init_data_structure() %>% cwa_md_buffer(hbuff)
  for (i in 1:10){
    buffer <- readBin(con,raw(),512,1)
    cwa_data %<>% cwa_ax_buffer(buffer)
  }
  
  close(con)
  
  all( expected$data$X[1:1200] == cwa_data$data$x, expected$data$Y[1:1200] == cwa_data$data$y,expected$data$Z[1:1200] == cwa_data$data$z )
}
test_ten_buffers("~/CWAfiles/1987073_90001_0_0.cwa",old)

test_entire_file <- function(cwa_file,expected){
  con <- file(cwa_file,"rb")
  hbuff <- readBin(con,raw(),1024,1)
  cwa_data <- cwa_init_data_structure() %>% cwa_md_buffer(hbuff)
  numBuffers <- (file.size(cwa_file)-1024)/512
  pb <- progress_bar$new(total=numBuffers,format="[:bar :percent] :current / :total :tick_rate buffers/sec :elapsedfull")
  for (i in 1:numBuffers){
     buffer <- readBin(con,raw(),512,1)
     cwa_data %<>% cwa_ax_buffer(buffer)
     pb$tick()
   }
  
  close(con)
  
  all( expected$data$X == cwa_data$data$x, expected$data$Y == cwa_data$data$y,expected$data$Z == cwa_data$data$z )
  
}
test_entire_file("~/CWAfiles/1987073_90001_0_0.cwa",old)

# cwa_ud_buffer <- function(cwa_data,buffer){
#   message("UD_BUFFER")
#   
#   cwa_data$ud <- cwa_data$ud + 1
#   return(cwa_data)
# }
# cwa_other_buffer <- function(cwa_data,buffer){
#   message("OTHER_BUFFER")  
#   cwa_data$other <- cwa_data$other + 1;
#   return(cwa_data)
# }
# 
# analyze_buffer<-function(cwa_data,buffer){
#   header <- readBin(buffer,integer(),1,2,signed = FALSE,endian = "little")
#   if (header == HEADER_ACCELEROMETER){
#     return(cwa_ax_buffer(cwa_data,buffer))
#   } else if (header == HEADER_METADATA){
#     return(cwa_md_buffer(cwa_data,buffer))
#   } else if (header == HEADER_UNDEFINED_BLOCK){
#     return(cwa_ud_buffer(cwa_data,buffer))
#   }
#   return(cwa_other_buffer(cwa_data,buffer))
# }

## analyze 1 buffer
# con <- file("~/CWAfiles/1987073_90001_0_0.cwa","rb")
# hbuff <- readBin(con,raw(),1024,1)
# buffer <- readBin(con,raw(),512,1)
# close(con)

### analyze subset
# con <- file("~/CWAfiles/1987073_90001_0_0.cwa","rb")
# data<-readBin(con,raw(),512*10,1)
# close(con)
# 
# 
# i<-0
# cwa_data <- cwa_init_data_structure()
# 
# for (p in seq(1,length(data),512)) {
#   i<-i+1
#   message(i," ",p)
#   buffer<-readBin(data[p:(p+512)],raw(),512,1)
#   cwa_data <- analyze_buffer(cwa_data,buffer)
# }
# cwa_print_count(cwa_data)
# print(i)  





### Analyze file...
# count <- init_count();
# cwafile = "~/CWAfiles/1987073_90001_0_0.cwa"
# numBuffers <- (file.size(cwafile)-1024)/512
# con <- file(cwafile,"rb")
# i<-0
# readBin(con,raw(),1024,1)
# for(i in 1:numBuffers){
#   ##while (length(buffer<-readBin(con,raw(),512,1))>0 ){
#   ##  i <- i+1
#   if(i %% 10000 == 0) message("read buffer ",i)
#   buffer <- readBin(con,raw(),512,1)
# }
# close(con)

# ~/CWAfiles/1987073_90001_0_0.cwa
# time in header: 2015-01-28 10:00:06 UTC

test_one_buffer()
clearData <- function(cwa_data){
  cwa_data$data = tibble()
  return(cwa_data)
}

old <- read.cwa::read_cwa("~/CWAfiles/1987073_90001_0_0.cwa")

