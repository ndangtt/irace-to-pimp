# create a directory for saving output
mkdir -p results

# convert irace.Rdata to PIMP's input format
../../irace-to-pimp.R --out-dir results --instance-feature-file features.csv --default-configuration-index 1 irace.Rdata

# call pimp
cd results
pimp -S scenario.txt -H runhistory.json -M all
cd ..
