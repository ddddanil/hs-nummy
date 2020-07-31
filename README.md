# Nummy

## Overview

An universal calculator inspired by [Numi](https://github.com/nikolaeu/numi)

Handling of dimensions and units is inspired by the [`units` package](http://hackage.haskell.org/package/units)

## Usage

Nummy is designed to compute and convert different values in a human-readable format

Generally any query line looks like `expression | format`

### Physical units

You can compute an arithmetic expression of units and present them in another unit you prefer.

```
> 3ft in m
0.9144 m

> 60 km/h + 3 m/s
70.8 km/h

> 3 m^2 - 1ft^2 | mm^2
2907096.96 mm²
```

### Currency

You can mix currency with other units, the exchange rate is provided courtesy of
[exchangeratesapi.io](https://exchangeratesapi.io/)

```
> 5 USD | EUR
4.22 EUR

> 40000 EUR/year | USD/hour
5.41 USD/h
```

## Contact

- Danil Doroshin [email](mailto:ddddanil@vivaldi.net)
