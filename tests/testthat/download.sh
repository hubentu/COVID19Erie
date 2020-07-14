#!/bin/bash

rm erie.html
`dirname "$0"`/save_page_as "https://erieny.maps.arcgis.com/apps/opsdashboard/index.html#/dd7f1c0c352e4192ab162a1dfadc58e1" --load-wait-time 20 --save-wait-time 5 -d erie.html
rm -r erie_files
