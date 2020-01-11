FROM hseeberger/scala-sbt:8u222_1.3.6_2.12.10

WORKDIR /RightFit

ADD . /RightFit

CMD sbt run
