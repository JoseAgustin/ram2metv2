# ram2metv2 — RAMA Air Quality Data Reformatter for MET

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Language: Fortran](https://img.shields.io/badge/Language-Fortran%2090-orange.svg)]()
[![Latest Release](https://img.shields.io/badge/Release-v2.5%20Aztlan-green.svg)](https://github.com/JoseAgustin/ram2metv2/releases/tag/v2.5)
[![Topics](https://img.shields.io/badge/Topics-air--quality%20%7C%20wrf--chem%20%7C%20met--dtc-lightgrey.svg)]()

> Converts the RAMA (Mexico City air quality network) ASCII database into the **METv5 ASCII point-observation format**, enabling direct use with the [`ascii2nc`](https://dtcenter.org/met-online-tutorial-metv8-0/point-processing-tool/ascii2nc) tool and the full [Model Evaluation Tools (MET)](https://dtcenter.org/community-code/model-evaluation-tools-met) suite for WRF-Chem verification.

---

## Table of Contents

- [Background](#background)
- [Requirements](#requirements)
- [Installation](#installation)
- [Repository Structure](#repository-structure)
- [Input Files](#input-files)
  - [namelist.met](#namelistmet)
  - [est\_rama.txt](#est_ramatxt)
  - [meteorologia\_YYYY.csv](#meteorologia_yyyycsv)
  - [contaminantes\_YYYY.csv](#contaminantes_yyyycsv)
- [Output Files](#output-files)
- [MET ASCII Format](#met-ascii-format)
- [GRIB Code Mapping](#grib-code-mapping)
  - [Table 128 — Meteorological Variables](#table-128--meteorological-variables)
  - [Table 129 — Particulate Matter](#table-129--particulate-matter)
  - [Table 141 — Trace Gases](#table-141--trace-gases)
- [Wind Component Conversion](#wind-component-conversion)
- [Date and Time Conversion](#date-and-time-conversion)
- [Program Logic](#program-logic)
- [Running the Program](#running-the-program)
- [Source Code Overview](#source-code-overview)
- [References](#references)

---

## Background

Improving air quality requires numerical models that can be rigorously evaluated against observations. The [Model Evaluation Tools (MET)](https://dtcenter.org/community-code/model-evaluation-tools-met) package provides objective statistical comparison between model output and measurements, but it requires observations in a specific ASCII format that differs from the format provided by air quality monitoring networks.

`ram2metv2` bridges this gap for the **RAMA** network (Red Automática de Monitoreo Atmosférico, Mexico City). It reads annual RAMA CSV files, reformats dates and time zones, derives GRIB codes for each measured variable, computes wind vector components, and writes the result in the 11-column MET ASCII format ready for `ascii2nc` → `pointstat` processing.

Although the converter was developed specifically for RAMA, the internal subroutines and modules are general enough to be adapted for other air quality monitoring databases.

**Relevant external links:**
- [MET — Model Evaluation Tools](https://dtcenter.org/community-code/model-evaluation-tools-met)
- [ascii2nc tool documentation](https://dtcenter.org/met-online-tutorial-metv8-0/point-processing-tool/ascii2nc)
- [WRF-Chem](https://ruc.noaa.gov/wrf/wrf-chem/)
- [RAMA network](http://www.aire.cdmx.gob.mx/default.php)

---

## Requirements

| Component | Notes |
|---|---|
| Fortran compiler | `gfortran` ≥ 6 or Intel `ifort` ≥ 17 |
| Autotools | `autoconf` and `automake` for the build system |
| Bash | Required for the `RAMA2.sh` monthly-processing script |

No external libraries (e.g., NetCDF) are required — all I/O is plain ASCII.

---

## Installation

```bash
# 1. Clone the repository
git clone https://github.com/JoseAgustin/ram2metv2.git
cd ram2metv2

# 2. (Optional) Regenerate the autotools configuration
bash autogen.sh

# 3. Configure and compile
./configure
make

# 4. Install the executable (optional)
make install
```

The compiled executable is named `rama2met.exe`.

---

## Repository Structure

```
ram2metv2/
├── rama2metv2.F90          # Main program — orchestrates the conversion workflow
├── rama2metop.F90          # Operator routines (I/O, data handling)
├── module_rama2met.F90     # Shared module — data types, constants, parameters
├── namelist.met            # Runtime configuration (time period)
├── est_rama.txt            # Station metadata (ID, coordinates, elevation)
├── RAMA2.sh                # Bash script for month-by-month batch processing
├── testsuite/              # Test cases and validation data
├── autotools/              # Autotools helper files
├── Doxyfile_raam2met.dox   # Doxygen configuration for API documentation
├── references.bib          # BibTeX references
├── Makefile.am             # Automake configuration
├── configure.ac            # Autoconf configuration
└── README.md               # This file
```

---

## Input Files

All four input files must be present in the working directory when `rama2met.exe` is executed.

### `namelist.met`

Fortran namelist file specifying the time window to process:

```fortran
&FECHA
  anio = 2020   ! Year of the input data  (YYYY)
  imes = 02     ! Start month             (1–12)
  fmes = 03     ! End month               (1–12)
  idia = 18     ! Start day               (1–31)
  fdia = 11     ! End day                 (1–31)
  ihr  = 01     ! Start hour              (1–24)
  fhr  = 24     ! End hour                (1–24)
/
```

### `est_rama.txt`

Station metadata file. Each row contains the three-character station alias, latitude (°N), longitude (°E), elevation (m a.s.l.), and station name:

```
Alias     Latitud       Longitud     Altitud   Estacion
ACO      19.635501    -98.912003      2198     Acolman
AJU      19.154286    -99.162611      2942     Ajusco
HGM      19.411617    -99.152207      2234     Hospital General de México
```

> **Note:** Mexico City stations measure at **10 m above ground level**. The average surface pressure at Mexico City elevation is **776 hPa**.

### `meteorologia_YYYY.csv`

Annual meteorological observations from RAMA in its native CSV format. Replace `YYYY` with the four-digit year (e.g., `meteorologia_2020.csv`). The file contains 10 header rows of metadata; data begins at row 11:

```
date,id_station,id_parameter,value,unit
01-01-2020 01:00,ACO,RH,70,6
01-01-2020 01:00,ACO,TMP,11.8,5
01-01-2020 01:00,ACO,WDR,340,4
01-01-2020 01:00,ACO,WSP,0.5,3
```

### `contaminantes_YYYY.csv`

Annual air pollutant concentrations from RAMA, same format as the meteorological file. Replace `YYYY` with the four-digit year (e.g., `contaminantes_2020.csv`).

> Annual files can exceed **1.5 million rows**, which is the primary reason a compiled Fortran program is used instead of a scripting language.

---

## Output Files

The program writes two plain-text files in MET ASCII format:

| File | Contents |
|---|---|
| `ramaYYYY_met.txt` | Reformatted meteorological observations |
| `ramaYYYY_pol.txt` | Reformatted air pollutant concentrations |

These files are passed directly to the MET `ascii2nc` tool, which converts them to NetCDF for use in `pointstat` statistical verification.

---

## MET ASCII Format

Each output file contains one point observation per line in 11 fixed columns:

| Column | Field | Description |
|---|---|---|
| 1 | `Message_Type` | Observation type; always `ADPSFC` for surface air quality data |
| 2 | `Station_ID` | Three-character RAMA station alias (e.g., `ACO`) |
| 3 | `Valid_Time` | Timestamp in `YYYYMMDD_HHMMSS` format (UTC) |
| 4 | `Lat` | Latitude in decimal degrees North |
| 5 | `Lon` | Longitude in decimal degrees East |
| 6 | `Elevation` | Station elevation in meters above sea level |
| 7 | `Grib_Code` | Integer GRIB code or variable name for the observation type |
| 8 | `Level` | Pressure level (hPa) or accumulation interval (hours) |
| 9 | `Height` | Height above sea level or above ground level (meters) |
| 10 | `QC_String` | Quality control flag; set to `1` (RAMA data are post-QA/QC) |
| 11 | `Observation_Value` | Measured value in the units defined by the GRIB code |

Because all observations are surface measurements, `Message_Type` is set to `ADPSFC` for every record. The GRIB code mapping is handled internally by the `vconvert` subroutine (see [GRIB Code Mapping](#grib-code-mapping) below).

---

## GRIB Code Mapping

Variable names in the RAMA CSV files are mapped to GRIB codes following the [WMO GRIB1 parameter tables](https://www.wmo.int/pages/prog/www/WMOCodes/Guides/GRIB/GRIB1-Contents.html#GRIB) and the [NCEP ON388 Table 2](https://www.nco.ncep.noaa.gov/pmb/docs/on388/table2.html). This mapping is performed by the `vconvert` subroutine in `rama2metop.F90`.

### Table 128 — Meteorological Variables

| GRIB Code | Parameter | Units | Abbreviation |
|---|---|---|---|
| 001 | Pressure | Pa | `PRES` |
| 011 | Temperature | K | `TEMP` |
| 031 | Wind direction (from which blowing) | deg true | `WDIR` |
| 032 | Wind speed | m/s | `WIND` |
| 033 | U-component of wind (eastward) | m/s | `UGRD` |
| 034 | V-component of wind (northward) | m/s | `VGRD` |
| 052 | Relative humidity | % | `RH` |

### Table 129 — Particulate Matter

| GRIB Code | Parameter | Units | Abbreviation |
|---|---|---|---|
| 156 | Particulate matter (coarse, PM10) | µg/m³ | `PMTC` |
| 157 | Particulate matter (fine, PM2.5) | µg/m³ | `PMTF` |
| 180 | Ozone concentration | ppbv | `OZCON` |

### Table 141 — Trace Gases

| GRIB Code | Parameter | Units | Abbreviation |
|---|---|---|---|
| 141 | Nitrogen oxide | ppbv | `NO` |
| 142 | Nitrogen dioxide | ppbv | `NO2` |
| 148 | Carbon monoxide | ppbv | `CO` |
| 232 | Sulfur dioxide | ppbv | `SO2` |
| 249 | Hydrophobic organic carbon | µg/m³ | `OC` |

---

## Wind Component Conversion

RAMA reports wind as **speed** and **direction** (meteorological convention: direction from which the wind is blowing). WRF-Chem and MET work with Cartesian wind components. The conversion is performed by the `viento` subroutine using:

```
u = −S · sin(DD)
v = −S · cos(DD)
```

where:

| Symbol | Description | Units |
|---|---|---|
| `u` | Eastward wind component | m/s |
| `v` | Northward wind component | m/s |
| `S` | Wind speed | m/s |
| `DD` | Wind direction (from which blowing) | degrees |

Both the original scalar wind (`WIND`, `WDIR`) and the derived vector components (`UGRD`, `VGRD`) are written to the output file.

---

## Date and Time Conversion

RAMA timestamps follow the format `DD-MM-YYYY HH:MM` in the **GMT−6** time zone (Central Standard Time, Mexico City). The `fconvert` subroutine performs two transformations:

1. **Reformats** the date string from `DD-MM-YYYY HH:MM` to the MET-required `YYYYMMDD_HHMMSS`.
2. **Converts** from GMT−6 to **UTC (GMT)** by adding 6 hours, with correct day/month/year rollover.

---

## Program Logic

`rama2met.exe` executes the following steps in sequence:

1. Read runtime parameters from `namelist.met`.
2. Load station metadata (ID, coordinates, elevation) from `est_rama.txt`.
3. Open and read `meteorologia_YYYY.csv` row by row:
   - Reformat the date/time and convert time zone via `fconvert`.
   - Look up station coordinates from `est_rama.txt`.
   - Map the variable name to a GRIB code via `vconvert`.
   - If the variable is wind speed or direction, compute `u`/`v` components via `viento`.
   - Write the formatted record to `ramaYYYY_met.txt`.
4. Repeat step 3 for `contaminantes_YYYY.csv`, writing output to `ramaYYYY_pol.txt`.

---

## Running the Program

### Single period

```bash
# 1. Edit namelist.met to set the desired time window
# 2. Ensure all input files are in the working directory
# 3. Run
./rama2met.exe
```

### Monthly batch processing

The `RAMA2.sh` script automates month-by-month processing for a full year:

```bash
bash RAMA2.sh
```

Edit the script to set the target year and any path variables before running.

---

## Source Code Overview

| File | Role |
|---|---|
| `rama2metv2.F90` | Main program; controls the conversion workflow |
| `rama2metop.F90` | Operator routines: `vconvert` (GRIB mapping), `viento` (wind components), `fconvert` (date/time), I/O handlers |
| `module_rama2met.F90` | Shared module: constants, derived types, global parameters |

API documentation can be regenerated with [Doxygen](https://www.doxygen.nl/):

```bash
doxygen Doxyfile_raam2met.dox
```

---

## References

Grell, G. A., Peckham, S. E., Schmitz, R., McKeen, S. A., Frost, G. J., Skamarock, W. C., & Eder, B. K. (2005). Fully coupled "online" chemistry within the WRF model. *Atmospheric Environment*, **39**, 6957–6975. https://doi.org/10.1016/j.atmosenv.2005.04.027

---

*README last updated: March 2026*
