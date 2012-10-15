all:
	Rscript run_all.R

##Delete the cache
clean-cache:
	rm -f cache/*.RData
	rm -f clean-data/*.csv.bz2
	rm -f clean-data/*.sqlite
	rm -f clean-data/*.RData
	rm -f ssa-database/*.DOC
	rm -f ssa-database/*.xls
	rm -f ssa-database/*.doc
	rm -f ssa-database/*.DBF
	rm -f ssa-database/*.dbf
	rm -f ssa-database/*.DOC

##Delete the cache and downloaded files from SINAIS
clean:
	rm -f cache/*.RData
	rm -f clean-data/*.csv.bz2
	rm -f clean-data/*.sqlite
	rm -f clean-data/*.RData
	rm -f ssa-database/*.zip
	rm -f ssa-database/*.DOC
	rm -f ssa-database/*.xls
	rm -f ssa-database/*.doc
	rm -f ssa-database/*.DBF
	rm -f ssa-database/*.dbf
	rm -f ssa-database/*.DOC
