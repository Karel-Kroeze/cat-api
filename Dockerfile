FROM r-base:latest

RUN apt-get update && apt-get install -y \
    libsodium-dev \
    libssl-dev \
    libcurl4-openssl-dev

WORKDIR /etc/app/cat-api
COPY renv.lock .

RUN install2.r renv
RUN Rscript -e "renv::restore()"

COPY data ./data
COPY R ./R
COPY spec.json .

EXPOSE 80

CMD ["Rscript", "./R/run.R"]
