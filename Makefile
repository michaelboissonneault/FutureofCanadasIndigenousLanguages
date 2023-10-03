
TARGETS=scdata DemographicParameters backcast excluded IntergenerationalTransmission Results \
    dormancy projection

all: $(TARGETS)

$(shell mkdir -p Figures)

scdata:
	Rscript 1DataPreparation_Linguistic.R

DemographicParameters:
	Rscript 2DataPreparation_Demographic.R

backcast: scdata DemographicParameters
	Rscript 3Backcast.R

excluded: backcast
	Rscript 4DataEvaluation.R

IntergenerationalTransmission: excluded
	Rscript 5IntergenerationalTransmission.R

Results: DemographicParameters IntergenerationalTransmission backcast excluded
	Rscript 6Projection.R

dormancy: Results scdata coordinates.xlsx
	Rscript 7Analysis.R

projection: DemographicParameters
	Rscript 8MethodEvaluation.R

## clean: removes auto-generated files
.PHONY: clean
clean:
	rm -rf Figures $(TARGETS)
	rm -rf Rplots.pdf

