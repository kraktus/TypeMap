#!/bin/sh

sbt clean coverage test && sbt coverageReport && open ./target/scala-3.4.2/scoverage-report/index.html