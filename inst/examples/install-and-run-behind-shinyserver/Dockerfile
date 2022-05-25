FROM rocker/shiny-verse:4.1.2

# usage
# docker build -t tmirus/idh-prediction-shinyserver:latest --no-cache inst/examples/install-and-run-behind-shinyserver
# docker run -it -p 3838:3838 tmirus/idh-prediction-shinyserver:latest
# http://localhost:3838/shinyserver/IDHprediction

RUN Rscript -e "devtools::install_github('tmirus/IDHprediction')"
COPY \
	shiny-server.conf \
	/etc/shiny-server/shiny-server.conf
RUN mkdir /srv/shiny-server/IDHprediction
COPY \
	app.R \
	/srv/shiny-server/IDHprediction/app.R
USER shiny
EXPOSE 3838
CMD ["/init"]
