#!/bin/sh
#
# Created by Agustin Garcia on 29/06/11.
# Copyright 2011 CCA-UNAM. All rights reserved.
anio=2020
imes=1
idia=1
ihr=1

while [ ${imes} -le 12 ]
do
    if [[ ${imes} -lt 10 ]]; then
        echo "Haciendo mes 0"${imes}
    else
        echo "Haciedo mes "${imes}
    fi

    if [ ${imes} -lt 12 ]
        then fmes=$(( ${imes} + 1 ))
        else fmes=${imes}
    fi
    if [ ${imes} -lt 12 ]
        then fdia=1
        else fdia=31
    fi
    if [ ${imes} -lt 12 ]
        then fhr=7
        else fhr=24
    fi

if [[ ${imes} -lt 9 ]]; then
cat << END_namelist >  namelist.met
&FECHA
anio=${anio}
imes=0${imes}
fmes=0${fmes}
idia=0${idia}
fdia=0${fdia}
ihr=0${ihr} ! Start hour
fhr=0${fhr} ! End hour
/
END_namelist
fi

if [[ ${imes} -eq 9 ]]; then
cat << END_namelist >  namelist.met
&FECHA
anio=${anio}
imes=0${imes}
fmes=${fmes}
idia=0${idia}
fdia=0${fdia}
ihr=0${ihr}
fhr=0${fhr}
/
END_namelist
fi

if [[ ${imes} -eq 10 ]]; then
cat << END_namelist >  namelist.met
&FECHA
anio=${anio}
imes=${imes}
fmes=${fmes}
idia=0${idia}
fdia=0${fdia}
ihr=0${ihr}
fhr=0${fhr}
/
END_namelist
fi

if [[ ${imes} -ge 11 ]]; then
cat << END_namelist >  namelist.met
&FECHA
anio=${anio} ! Year from input data
imes=${imes} ! Start month
fmes=${fmes} ! End month
idia=0${idia} ! Start day
fdia=0${fdia} ! End day
ihr=0${ihr}   ! Start hour
fhr=0${fhr}   ! End hour
/
END_namelist
fi

    ./rama2met.exe

if [[ ${imes} -lt 10 ]];
then mv rama${anio}_met.txt rama${anio}_met_mes0${imes}.csv
     mv rama${anio}_pol.txt rama${anio}_pol_mes0${imes}.csv
else mv rama${anio}_met.txt rama${anio}_met_mes${imes}.csv
     mv rama${anio}_pol.txt rama${anio}_pol_mes${imes}.csv
fi

imes=$(( $imes + 1 ))

done

echo "Cerrando RAMA2.sh"
