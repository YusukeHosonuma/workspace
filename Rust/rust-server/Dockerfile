FROM rust:1.26.0

LABEL Name=rust-server Version=0.1.0

WORKDIR /usr/src/app

COPY . .

# Note: 時間かかるのでやらない
# RUN apt-get -y update && apt-get -y upgrade

RUN cargo install

CMD rustserver

EXPOSE 5000
