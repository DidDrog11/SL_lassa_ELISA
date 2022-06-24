# Seroprevalence for LASV in rodents trapped in Eastern Province, Sierra Leone.

**IN PROGRESS**

## Rodent trapping

The main pipeline for the pre-processing of caught rodents, including, speciation and habitat can be found in the linked [rodent_trapping](https://github.com/DidDrog11/rodent_trapping) repository.

## ELISA protocol

The lab protocol has been minimally adapted for use in Sierra Leone from the providers published protocol [BLACKBOX protocol](https://www.european-virus-archive.com/sites/default/files/detection_kit/LASV_IgG_IFU_11Feb2022.pdf). The protocol used is available within this repository.

## Sample inventory

Samples have been mislabelled. Samples have been able to be allocated to the correct trapping session and therefore the correct rodent in most cases. However, filter paper samples from several visits remain uncertain. Blood if available will be prioritised for ELISA and PCR for these individuals, otherwise filter paper will be used. An excel document in `input` contains the inventory.

## Sample prioritisation

The samples to be tested and their priority is produced in the `rodent_trapping` project. The allocated priority is produced in a datestamped file within `data/clean_data/lab` this file should be copied into the `input` folder in this repository.

### Sample results

Results are stored in the `output` folder in the a date stamped `processed_lab` excel file.