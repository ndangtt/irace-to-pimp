This is a small `R` script to convert the `Rdata` file created by the automated algorithm configuration tool [irace](http://iridia.ulb.ac.be/irace/) to input format supported by the algorithm parameter analysis tool [PyImp](https://github.com/automl/ParameterImportance).

####Requirements
- [irace](http://iridia.ulb.ac.be/irace/) (>=3.0)
- [PyImp](https://github.com/automl/ParameterImportance) 
- [argparser](https://cran.r-project.org/web/packages/argparser/index.html) (>=0.4)

####Examples

There are two examples in `examples` folder. To run each example, simply execute the script `run.sh` in that folder

####Usage

```
./irace-to-pimp.R [--help] [--normalise] [--normalise-scope NORMALISE-SCOPE] [--out-dir OUT-DIR] [--instance-feature-file INSTANCE-FEATURE-FILE] [--filter-conditions FILTER-CONDITIONS] [--default-configuration-index DEFAULT-CONFIGURATION-INDEX] irace-rdata-file

irace-to-pimp command line arguments

positional arguments:
  irace-rdata-file			  irace Rdata file

flags:
  -h, --help			      show this help message and exit 
  -n, --normalise			
                              Normalise the cost metric values before converting to PIMP format.
                              By default, the normalisation is instance-based. However, sometimes
                              there are several instances with the same feature values, and we
                              might want to normalise based on feature values instead. See option
                              -normlisation-scope for details

optional arguments:
  -ns, --normalise-scope NORMALISE-SCOPE			
                              Scope of the normalisation. Values:
	                              instance: normalisation cost is calculated based on instances
	                              feature: normalisation cost is calculated based on instance
                                           features. Instance features must be provided 
                              [default: instance]
  -d, --out-dir OUT-DIR			
                              directory where all generated data are stored. [default: ./output]
  -fea, --instance-feature-file INSTANCE-FEATURE-FILE			
                              a .csv file containing instance features (one line per instance,
                              sorted in the same order as the list of instances input to irace).
                              The first line contains feature names.
  -c, --filter-conditions FILTER-CONDITIONS			
                              Only extract data that satisfies the given conditions. The
                              conditions are in R expression format [default: no filter]
  -i, --default-configuration-index DEFAULT-CONFIGURATION-INDEX			
                              Index of default configuration (starting from 1), used by ablation
                              analysis [default: 1]
```