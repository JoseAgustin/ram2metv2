#!/bin/csh

# do_real.csh
#
#
# Created by Agustin Garcia on 29/06/11.
# Copyright 2011 CCA-UNAM. All rights reserved.

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
&SCALE
imes=0${imes}
fmes=0${fmes}
idia=0${idia}
fdia=0${fdia}
ihr=0${ihr}
fhr=0${fhr}
/
END_namelist
fi

if [[ ${imes} -eq 9 ]]; then
cat << END_namelist >  namelist.met
&SCALE
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
&SCALE
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
&SCALE
imes=${imes}
fmes=${fmes}
idia=0${idia}
fdia=0${fdia}
ihr=0${ihr}
fhr=0${fhr}
/
END_namelist
fi

    ./rama2met.exe

if [[ ${imes} -lt 10 ]];
then mv rama2011_met.csv rama2011_met_mes0${imes}.csv
     mv rama2011_pol.csv rama2011_pol_mes0${imes}.csv
else mv rama2011_met.csv rama2011_met_mes${imes}.csv
     mv rama2011_pol.csv rama2011_pol_mes${imes}.csv
fi

imes=$(( $imes + 1 ))

done

echo "Cerrando RAMA2.sh"
