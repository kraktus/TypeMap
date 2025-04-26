#!/bin/sh

sbt clean coverage test && sbt coverageReport && open ./target/scala-3.6.4/scoverage-report/index.html