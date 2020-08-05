# Conversion of Air quality Measured data to be use for model evaluation
(RAMA2MET Tool v2.0)


## Background
Strategies for air quality improvement requieres the use of  air quality numerical models. The statistical tools that can provide objective comparison between models results and air quality measurements are important.  One of those verification packages is the [Model Evaluation Tools ([MET]) ] with it is possible to compare results from air quality models such WRF-chem ![Grell et al. 2005][GRELL2005] and observations from air quality networks such as [RAMA] in Mexico City.

However the format requiered for the MET suite is not the format provided by the  air quality network database. This code provide a way to reformat RAMA data base into MET ascii format in order to be used in **ascii2nc**  tool for providing the data to _pointstat_ for computing the different statistical metrics.

## Summary
This code reads, computes wind vectors, identifies the grib code and reformat the air quality data into METv5 ascii format. This software was developed for an specific input ascii format however fuctions and subroutines can be used to convert other databases.

The MET ascii format has 11 columns it contains one point observation per line. Each row in the file has the following data in columns:

1. Message_Type
2. Station_ID
3. Valid_Time in YYYYMMDD_HHMMSS format
4. Lat in degrees North
5. Lon in degrees East
6. Elevation in meters above sea level
7. Grib_Code as the integer GRIB code value or variable name corresponding to this observation type
8. Level as the pressure level in hPa or accumulation interval in hours
9. Height in meters above sea level or above ground level
10. QC_String corresponding to the quality control value
11. Observation_Value in the units prescribed for the grib code

Meteorological data and chemical trace gases are measured at ground level then the **Message_Type** variable is set to _ADPSFC_.

 Information about statios are obtained from reading the file _est_rama.txt_ , it has the Station_ID, Lat, Lon and  Elevation

    Alias    Latitud     Longitud    Altitud    Estacion (41)
    ACO    19.635501    -98.912003    2198    Acolman
    AJU    19.154286    -99.162611    2942    Ajusco


For Mexico City the pressure level is on average 776. hPa

Stations measured it variables at 10 m above ground level.

The ![GRIB][grib128]  table contains a set of tables with the codes for each variable in order to map the variable to its description, units and abbreviature. In addition to the meteorological variables the chemical gas traces are  also considered. The following tables used and variables considered are presented. The grib codes follows the guide from World Meteorological Organization [WMO].

### Table 128
|Value| Parameter|Units| ABBREV.|
|---| --- | --- |--- |
001 | Pressure | Pa| PRES|
|011| Temperature|K|TEMP|
|031|Wind direction (from which blowing) | deg true | WDIR|
|032|Wind speed |m/s| WIND|
|033|u-component of wind |m/s|UGRD|
|034|v-component of wind | m/s | VGRD|
|052| Relative humidity | %| RH|

### Table 129

|Value| Parameter|Units| ABBREV.|
|---| --- | --- |--- |
|156 | Particulate matter (coarse) |  ug/m^3 | PMTC|
|157 |  Particulate matter (fine) |  ppb | PMTF|
|180 | Ozone concentration |  ppb | OZCON|

### Table 141

|Value| Parameter|Units| ABBREV.|
|---| --- | --- |--- |
|141| Nitrogen Oxide | ppbv |NO|
|142| Nitrogen Dioxide | ppbv |NO2|
|148|Carbon Monoxide| ppbv |CO|
|232| Sulfur Dioxide| ppbv |SO2|

This mapping is performed in _vconvert_ subroutine.

### Wind components
The RAMA database contains wind speed and wind direction, the meteorological model provides wind componens in W-E and S-N directions. A conversion from vector to it components for wind use the following equations:


    u = -S SIN( DD)
    v = -S COS(DD)


where
* u - eastward wind component (m/s)
* v - northward wind component (m/s)
* S - wind speed (m/s)
* DD -- Wind direction (from which blowing) deg

This computations are performed in _viento_ subroutine

For RAMA files the date and time are in one column separated by one space,  the date in DD-MM-YYYY and  time  HH:MM subroutine _fconvert_ reedit the date to YYYYMMDD_HHMMSS format and computes the change from  the GMT-6 time zone to GMT.

Because the air quality networ in Mexico City publish data after a QA/QC process the QC flags is set to 1.

## References

[grib128]:https://www.nco.ncep.noaa.gov/pmb/docs/on388/table2.html
[MET]: https://dtcenter.org/community-code/model-evaluation-tools-met
[RAMA]:  http://www.aire.cdmx.gob.mx/default.php?opc=%27aKBhnmI=%27&opcion=Zg==
[WMO]: https://www.wmo.int/pages/prog/www/WMOCodes/Guides/GRIB/GRIB1-Contents.html#GRIB

[GRELL2005]: Grell, G.A., Peckham, S.E., Schmitz, R., McKeen, S.A., Frost, G.J., Skamarock, W.C., & Eder, B.K. (2005). Fully coupled "online" chemistry within the WRF model. _Atmospheric Environment_, **39**, 6957-6975.


@article{GRELL20056957,
title = "Fully coupled “online” chemistry within the WRF model",
journal = "Atmospheric Environment",
volume = "39",
number = "37",
pages = "6957 - 6975",
year = "2005",
issn = "1352-2310",
doi = "https://doi.org/10.1016/j.atmosenv.2005.04.027",
url = "http://www.sciencedirect.com/science/article/pii/S1352231005003560",
author = "Georg A. Grell and Steven E. Peckham and Rainer Schmitz and Stuart A. McKeen and Gregory Frost and William C. Skamarock and Brian Eder",
keywords = "Urban and regional pollution, Urban and regional air quality modeling, Air quality forecasting, Aerosols and particles",
abstract = "A fully coupled “online” Weather Research and Forecasting/Chemistry (WRF/Chem) model has been developed. The air quality component of the model is fully consistent with the meteorological component; both components use the same transport scheme (mass and scalar preserving), the same grid (horizontal and vertical components), and the same physics schemes for subgrid-scale transport. The components also use the same timestep, hence no temporal interpolation is needed. The chemistry package consists of dry deposition (“flux-resistance” method), biogenic emission as in [Simpson et al., 1995. Journal of Geophysical Research 100D, 22875–22890; Guenther et al., 1994. Atmospheric Environment 28, 1197–1210], the chemical mechanism from RADM2, a complex photolysis scheme (Madronich scheme coupled with hydrometeors), and a state of the art aerosol module (MADE/SORGAM aerosol parameterization). The WRF/Chem model is statistically evaluated and compared to MM5/Chem and to detailed photochemical data collected during the summer 2002 NEAQS field study. It is shown that the WRF/Chem model is statistically better skilled in forecasting O3 than MM5/Chem, with no appreciable differences between models in terms of bias with the observations. Furthermore, the WRF/Chem model consistently exhibits better skill at forecasting the O3 precursors CO and NOy at all of the surface sites. However, the WRF/Chem model biases of these precursors and of other gas-phase species are persistently higher than for MM5/Chem, and are most often biased high compared to observations. Finally, we show that the impact of other basic model assumptions on these same statistics can be much larger than the differences caused by model differences. An example showing the sensitivity of various statistical measures with respect to the treatment of biogenic volatile organic compounds emissions illustrates this impact."
