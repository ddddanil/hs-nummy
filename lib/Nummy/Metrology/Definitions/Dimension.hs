{-# OPTIONS_HADDOCK hide #-}
module Nummy.Metrology.Definitions.Dimension where

import Nummy.Metrology.Dimension as D


-- *** Base dimensions

dimless = D.dimless

length = baseDim Length

mass = baseDim Mass

time = baseDim Time

current = baseDim Current

temp = baseDim Temp

info = baseDim Information


-- *** Derived dimensions

area = length |^| 2

volume = length |^| 3

speed = length |/| time

acceleration = speed |/| time

impulse = mass |*| speed

force = mass |*| acceleration

pressure = force |/| area

energy = force |*| length

power = energy |/| time

frequency = time |^| (-1)

charge = current |*| time

voltage = power |/| current

resistance = voltage |/| current
