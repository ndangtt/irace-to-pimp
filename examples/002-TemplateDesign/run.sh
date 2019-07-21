# create a directory for saving output
mkdir -p results

# convert irace.Rdata to PIMP's input format
../../irace-to-pimp.R --out-dir results --normalise --filter-conditions "n_templates_middle<=30" irace.Rdata

# call pimp
cd results
pimp -S scenario.txt -H runhistory.json -M all --marginalize_over_instances # note: the flag --marginalize_over_instances is for using fanova in this particular example, as the variance from all random forest's trees will be zero without this options, which will cause error 
cd ..
