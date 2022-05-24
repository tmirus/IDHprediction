FROM rocker/shiny-verse:4.1.2

RUN Rscript -e "devtools::install_github('tmirus/IDHprediction')"
COPY \
	shiny-server.conf \
	/etc/shiny-server/shiny-server.conf
RUN mkdir /srv/shiny-server/IDHpredcition
COPY \
	app.R \
	/srv/shiny-server/IDHprediction/app.R
USER shiny
EXPOSE 3838
CMD ["/init"]
