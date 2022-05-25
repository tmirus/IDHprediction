FROM rocker/shiny-verse:4.1.2

# Usage:
#   docker build -t gitlab.spang-lab.de:4687/containers/idhprediction:1.0.3 inst/examples/install-and-run-behind-shinyserver
#   docker run -it -p 80:8080 gitlab.spang-lab.de:4687/containers/idhprediction:1.0.3
#   docker push gitlab.spang-lab.de:4687/containers/idhprediction:1.0.3
#   start http://localhost/

RUN apt update
RUN apt install -y git
RUN git clone https://glpat-mVg7bApfhktEvjtwS3aN:glpat-mVg7bApfhktEvjtwS3aN@gitlab.spang-lab.de/containers/idhprediction.git
RUN Rscript -e "devtools::install('idhprediction/')"
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
