#!/usr/bin/env bash
scalac GraphicsCanvas.scala
scalac -cp ".:TurtleEDSL.jar" TurtleStandalone.scala
