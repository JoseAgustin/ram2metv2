# Air Quality Measured Data Reformat for Model Evaluation
___
(RAMA2MET Tool v2.0)

[[Model Evaluation](https://dtcenter.org/community-code/model-evaluation-tools-met)]
[[ascii2nc](https://dtcenter.org/met-online-tutorial-metv8-0/point-processing-tool/ascii2nc)]
[WRF-chem](https://ruc.noaa.gov/wrf/wrf-chem/) [RAMA](http://www.aire.cdmx.gob.mx/default.php)
___
## Background
Strategies for air quality improvement requieres the use of  air quality numerical models. The statistical tools that can provide objective comparison between models results and air quality measurements are important.  One of those verification packages is the Model Evaluation Tools ([MET]) with it is possible to compare results from air quality models such WRF-chem [Grell et al 2005][^1] and observations from air quality networks such as [RAMA] in Mexico City.

However the format requiered for the MET suite is not the format provided by the  air quality network database. This code provide a way to reformat RAMA data base into MET ascii format in order to be used in **ascii2nc**  tool for providing the data to _pointstat_ for computing the different statistical metrics.
___
## Summary
This code reads, computes wind vectors, identifies the grib code and reformat the air quality data into [METv5][MET] ascii format. _This software was developed for an specific input ascii format however fuctions and subroutines can be used to convert other databases._

The MET ascii format has 11 columns it contains one point observation per line. Each row in the file has the following data in columns:

1. Message_Type
2. Station_ID
3. Valid_Time in YYYYMMDD_HHMMSS format
4. Lat in degrees North
5. Lon in degrees East
6. Elevation in meters above sea level
7. Grib_Code as the integer [GRIB][gribt] code value or variable name corresponding to this observation type
8. Level as the pressure level in hPa or accumulation interval in hours
9. Height in meters above sea level or above ground level
10. QC_String corresponding to the quality control value
11. Observation_Value in the units prescribed for the grib code

Meteorological data and chemical trace gases are measured at ground level then the **Message_Type** variable is set to _ADPSFC_.

 Information about stations are obtained from reading the file _est_rama.txt_ , it has the Station_ID, Lat, Lon and  Elevation

For Mexico City the pressure level is on average 776. hPa

Stations measured it variables at 10 m above ground level.

RAMA datafile in the first 10 rows contains metadata, the 11th has the header data and it has a single data per row.

    11  date,id_station,id_parameter,value,unit
    12  01-01-2020 01:00,ACO,RH,70,6
    13  01-01-2020 01:00,ACO,TMP,11.8,5
    14  01-01-2020 01:00,ACO,WDR,340,4
    15  01-01-2020 01:00,ACO,WSP,0.5,3

Because a file contains a year measurements it can be quite large (>1.5M rows) and in order to reformatting a fortran code was developed.

The [GRIB][gribt]  site contains a set of Parameter tables versions with the codes for each variable in order to map the variable to its description, units and abbreviation. In addition to the meteorological variables the chemical trace gases  are also considered. The following tables used and variables considered are presented. The grib codes follows the guide from World Meteorological Organization [WMO].

### Table 128
|Value| Parameter|Units| Abbrev.|
|---| --- | --- |--- |
001 | Pressure | Pa| PRES|
|011| Temperature|K|TEMP|
|031|Wind direction (from which blowing) | deg true | WDIR|
|032|Wind speed |m/s| WIND|
|033|u-component of wind |m/s|UGRD|
|034|v-component of wind | m/s | VGRD|
|052| Relative humidity | %| RH|

### Table 129

|Value| Parameter|Units| Abbrev.|
|---| --- | --- |--- |
|156 | Particulate matter (coarse) |  ug/m^3 | PMTC|
|157 |  Particulate matter (fine) | ug/m^3 | PMTF|
|180 | Ozone concentration |  ppbv | OZCON|

### Table 141

|Value| Parameter|Units| Abbrev.|
|---| --- | --- |  ---|
|141| Nitrogen Oxide | ppbv |NO|
|142| Nitrogen Dioxide | ppbv |NO2|
|148|Carbon Monoxide| ppbv |CO|
|232| Sulfur Dioxide| ppbv |SO2|
|249| Hydrophobic Organic Carbon | ug/m^3 |OC|

This mapping between text variable and grib code is performed in _vconvert_ subroutine.

### Wind components
The RAMA database contains wind speed and wind direction, the meteorological model provides wind components in W-E and S-N directions. A conversion from vector to it components for wind use the following equations:

    u = -S SIN(DD)
    v = -S COS(DD)

where
 * u - eastward wind component (m/s)
 * v - northward wind component (m/s)
 * S - wind speed (m/s)
 * DD -- Wind direction (from which blowing) deg

This computations are performed in _viento_ subroutine

For RAMA files the date and time are in one column separated by one space, the date in DD-MM-YYYY and  time  HH:MM subroutine _fconvert_ reformat the date to YYYYMMDD_HHMMSS  and computes the conversion from GMT-6 time zone to GMT.

Because the air quality network in Mexico City publish data after a QA/QC process the QC flags is set to 1.

## Description
The rama2met.exe program requires as input the following files:

    namelist.met
    est_rama.txt
    contaminantes_YYYY.csv
    meteorologia_YYYY.csv

**namelist.met** file contains the time period information

    &FECHA
    anio=2020  ! Year from input data  YYYY
    imes=02    ! Start month
    fmes=03    ! End month
    idia=18    ! Start day
    fdia=11    ! End day
    ihr=01     ! Start hour
    fhr=24     ! End hour
    /

  **est_rama.txt** contains information from the stations: a three character ID, latitud, longitud, height above sea level and description.

    Alias    Latitud     Longitud    Altitud    Estacion
      ACO    19.635501    -98.912003    2198    Acolman
      AJU    19.154286    -99.162611    2942    Ajusco
      HGM    19.411617    -99.152207	2234	Hospital General de México

  **meteorologia_YYYY.csv** contains the air pollutants concentrations during the year YYYY (i.e. 2020). [RAMA] format

  **contaminantes_YYYY.csv** contains the meteorological  variables during the year YYYY (i.e. 2020). [RAMA] format.

The program starts reading the _namelist.met_ file, then the stations file _est_rama.txt_. Then reads the _meteorolgia_YYYY.csv_ file, reformat the date, from the ID obtains the coordinates, and from the variable name the grib code. Finally writes the information in _ramaYYYY_met.txt_. After finishing the meteorological file starts with the pollutants file _contaminantes_YYYY.csv_ following the same procedure and writing the reformat data in _ramaYYYY_pol.txt_.

The RAMA2.sh bash script can generate files per month.
___
## References

[gribt]:https://www.nco.ncep.noaa.gov/pmb/docs/on388/table2.html
[MET]: https://dtcenter.org/community-code/model-evaluation-tools-met
[RAMA]:  http://www.aire.cdmx.gob.mx/default.php?opc=%27aKBhnmI=%27&opcion=Zg==
[WMO]: https://www.wmo.int/pages/prog/www/WMOCodes/Guides/GRIB/GRIB1-Contents.html#GRIB

[^1]:Grell, G.A., Peckham, S.E., Schmitz, R., McKeen, S.A., Frost, G.J., Skamarock, W.C., & Eder, B.K. (2005). Fully coupled "online" chemistry within the WRF model. _Atmospheric Environment_, **39**, 6957-6975.
