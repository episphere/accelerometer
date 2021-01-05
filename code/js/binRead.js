console.log("in binRead.js")


// read the 1K CWA header...
// uint16_t packetHeader;                      ///< @ 0  +2   ASCII "MD", little-endian (0x444D)
// uint16_t packetLength;                      ///< @ 2  +2   Packet length (1020 bytes, with header (4) = 1024 bytes total)
// uint8_t  hardwareType;                      ///< @ 4  +1   Hardware type (0x00/0xff/0x17 = AX3, 0x64 = AX6)
// uint16_t deviceId;                          ///< @ 5  +2   Device identifier
// uint32_t sessionId;                         ///< @ 7  +4   Unique session identifier
// uint16_t upperDeviceId;                     ///< @11  +2   Upper word of device id (if 0xffff is read, treat as 0x0000)
// cwa_timestamp_t loggingStartTime;           ///< @13  +4   Start time for delayed logging
// cwa_timestamp_t loggingEndTime;             ///< @17  +4   Stop time for delayed logging
// uint32_t loggingCapacity;                   ///< @21  +4   (Deprecated: preset maximum number of samples to collect, should be 0 = unlimited)
// uint8_t  reserved1[1];                      ///< @25  +1   (1 byte reserved)
// uint8_t  flashLed;                          ///< @26  +1   Flash LED during recording
// uint8_t  reserved2[8];                      ///< @27  +8   (8 bytes reserved)
// uint8_t  sensorConfig;                      ///< @35  +1   Fixed rate sensor configuration, 0x00 or 0xff means accel only, otherwise bottom nibble is gyro range (8000/2^n dps): 2=2000, 3=1000, 4=500, 5=250, 6=125, top nibble non-zero is magnetometer enabled.
// uint8_t  samplingRate;                      ///< @36  +1   Sampling rate code, frequency (3200/(1<<(15-(rate & 0x0f)))) Hz, range (+/-g) (16 >> (rate >> 6)).
// cwa_timestamp_t lastChangeTime;             ///< @37  +4   Last change meta-data time
// uint8_t  firmwareRevision;                  ///< @41  +1   Firmware revision number
// int16_t  timeZone;                          ///< @42  +2   (Unused: originally reserved for a "Time Zone offset from UTC in minutes", 0xffff = -1 = unknown)
// uint8_t  reserved3[20];                     ///< @44  +20  (20 bytes reserved)
// uint8_t  annotation[OM_METADATA_SIZE];      ///< @64  +448 Scratch buffer / meta-data (448 characters, ignore trailing 0x20/0x00/0xff bytes, url-encoded UTF-8 name-value pairs)
// uint8_t  reserved[512];                     ///< @512 +512 (Reserved for device-specific meta-data in the same format as the user meta-data) (512 bytes)

cwa = {
    METADATA_BUFFER_HEADER: 0x444D,
    HARDWARE_TYPE_UNKNOWN: 0x00,
    HARDWARE_TYPE_AX3: 0xFF,
    HARDWARE_TYPE_AX6: 0x17,
}

//YYYYYYMM MMDDDDDh hhhhmmmm mmssssss 
cwa.convertTime = function (x) {
    // let year = 2000 + ((x >> 26) & 0x3F)
    // let month = ((x >> 22) & 0x0F)
    // let day = ((x >> 17) & 0x1F)
    // let hour = (x >> 12) & 0x1F
    // let minute = (x >> 6) & 0x3F
    // let sec = x & 0x3F
    // console.log(year, month, day, hour, minute, sec)
    let dte = new Date(
        2000 + ((x >> 26) & 0x3F), // year
        ((x >> 22) & 0x0F) - 1, // month (0->11)
        ((x >> 17) & 0x1F),  // day
        (x >> 12) & 0x1F, //hour 
        (x >> 6) & 0x3F,//minute, 
        x & 0x3F) // sec
    //    console.log(dte)
    return (dte)
}

async function decodeAXBuffer(buffer) {
    let dv = new DataView(buffer, 0, 512)
    let bytes_per_sample = dv.getUint8(25) & 0x0F
    data = []
    if (bytes_per_sample == 0) {
        data = new Uint32Array(buffer, 120,)
    }
}

async function loadFirstDataBuffer(box_id) {
    let dta = await fetch(`https://api.box.com/2.0/files/${box_id}/content`,
        {
            method: 'GET',
            headers: {
                'content-type': 'multipart/byteranges',
                'range': `bytes=1024-1536`,
                Authorization: "Bearer " + epibox.oauth.token.access_token
            }
        }
    ).then(response => response.arrayBuffer()).then(buffer => decodeAXBuffer(buffer))
}

async function loadHeader(box_id) {

    let dta = await fetch(`https://api.box.com/2.0/files/${box_id}/content`,
        {
            method: 'GET',
            headers: {
                'content-type': 'multipart/byteranges',
                'range': `bytes=0-1023`,
                Authorization: "Bearer " + epibox.oauth.token.access_token
            }
        }
    ).then(response => response.arrayBuffer()).then(buffer => {
        let dv = new DataView(buffer, 0, 512)

        let hwType = dv.getUint8(4);
        let header = {
            packetHeader: dv.getUint16(0, true).toString(16),
            packetLength: dv.getUint16(2, true),
            hardwareType: hwType == cwa.HARDWARE_TYPE_AX3 ? "AX3" : hwType == cwa.HARDWARE_TYPE_AX6 ? "AX6" : "UNKNOWN",
            deviceId: dv.getUint16(5, true),
            sessionId: dv.getUint32(7, true),
            upperDeviceId: dv.getUint16(11, true),
            loggingStartTime: cwa.convertTime(dv.getUint32(13, true)),
            loggingEndTime: cwa.convertTime(dv.getUint32(17, true)),
            loggingCapacity: dv.getUint32(21, true),
            // ignore reseved byte...
            flashLed: dv.getUint8(26),
            // ignore 8 reserved bytes...
            sensorConfig: dv.getUint8(35),
            samplingRate: dv.getUint8(35),
            lastChangeTime: cwa.convertTime(dv.getUint32(37, true)),
            firmwareRevision: dv.getUint8(41),
            // time zone not used...
        }

        return (header)
    })
    console.log(dta)
}

async function f1() {
    console.log("in f1")
    await epibox.ini()
    // 1987073_90001_0_0.cwa
    //await loadHeader("749133275719")
    await loadFirstDataBuffer("749133275719")
    //await loadHeader("49132097899")
}
f1()



//"2015-01-28 10:00:06 UTC"
cwa.convertTime(0x3c78a000)