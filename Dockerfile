FROM rocker/rstudio:3.5.2

WORKDIR /

# Copy directories
COPY main main
COPY inputs inputs
COPY ancillary ancillary

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
	wget \
	zip

# Get input files
WORKDIR /inputs
RUN wget 'http://fenixservices.fao.org/faostat/static/bulkdownloads/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip'
RUN wget 'http://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Crops_E_All_Data_(Normalized).zip'
RUN wget 'https://apps.fas.usda.gov/psdonline/downloads/psd_grains_pulses_csv.zip'

# Unzip input files
RUN unzip 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip'
RUN unzip 'Production_Crops_E_All_Data_(Normalized).zip'

RUN rm 'psd_grains_pulses.csv'
RUN unzip 'psd_grains_pulses_csv.zip'

# Remove zip files
RUN rm 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip'
RUN rm 'Production_Crops_E_All_Data_(Normalized).zip'
RUN rm 'psd_grains_pulses_csv.zip'

# Run set up R Scripts
WORKDIR /
RUN Rscript /main/Requirements.R
RUN Rscript /main/MatrixCreation.R

# Container entrypoint
ENTRYPOINT ["Rscript", "/main/main.R"]