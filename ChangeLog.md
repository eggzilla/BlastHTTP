-*-change-log-*-

### 1.4.2 [Florian Eggenhofer](mailto:egg@informatik.uni-freiburg.de) 6. June 2021

	* Compatibility with ghc 9.0.1
	* Swutched testing to github actions


### 1.4.1 [Florian Eggenhofer](mailto:egg@informatik.uni-freiburg.de) 20. August 2019

	* Adjusting version constraint for BiobaseBlast

### 1.4.0 [Florian Eggenhofer](mailto:egg@informatik.uni-freiburg.de) 17. June 2019

	* Compatibility with Biobase
	* Added requests with JSON2 response as default
	* Using Biobase.Fasta for encoding query sequences
	* Removed XML queries

### 1.3.0 [Florian Eggenhofer](mailto:egg@informatik.uni-freiburg.de) 12. March 2018

	* Added tabular format request use BiobaseBlast attoparsec parser
	* Improved travis testing
	* Compatibility with http-conduit-2.3.0

### 1.2.1 [Florian Eggenhofer](mailto:egg@informatik.uni-freiburg.de) 12. November 2016

	* Changed used NCBI URL to HTTPS

### 1.2.0 [Florian Eggenhofer](mailto:florian.eggenhofer@univie.ac.at) 20. April 2015

	* Added experimental support for the european bioinformatics institute blast REST interface
	* Added support for multiple sequences in one request

### 1.0.1 [Florian Eggenhofer](mailto:florian.eggenhofer@univie.ac.at) 09. October 2014

	* Added README.md, travis CI support
	* Added new optionalArgument parameter enabling submission of optional blast search parameters, like e-value cutoff
