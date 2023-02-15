# BrAPI Provider Connector

## Overview

Provides support for connecting to multiple providers of phenotype and genotype data providers using BrAPI.  This is a work in progress. The goal is to provide a way to easily configure and use multiple breeding data mangement systems using a single interface. At its core, this code uses the [QBMS package](https://github.com/icarda-git/QBMS) to handle the BrAPI calls.

### Usage

The following code shows how to use the package to connect to a phenotype and genotype data provider list the crops and databases vailable.
```R
source("BrAPIProvider.R")

pheno_provider <- BrapiBms$new(url="your_url")
pheno_provider$login(username="your_username", password="your_password")

geno_provider = BrapiGigwa$new(url="your_url")
geno_provider$login(username="your_username", password="your_password")


pheno_provider$list_crops()
geno_provider$gigwa_list_dbs()
```
> Note: The BrAPI Provider package is still in development.  The above code is not guaranteed to work. And might change in the future. Certain function names might change to be more uniform e.g. `gigwa_list_dbs` could be changed to list_dbs.
