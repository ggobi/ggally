# Twitter spambots

A network of spambots found on Twitter as part of a data mining project.

## Usage

``` r
data(twitter_spambots)
```

## Format

An object of class `network` with 120 edges and 94 vertices.

## Details

Each node of the network is identified by the Twitter screen name of the
account and further carries five vertex attributes:

- location user's location, as provided by the user

- lat latitude, based on the user's location

- lon longitude, based on the user's location

- followers number of Twitter accounts that follow this account

- friends number of Twitter accounts followed by the account

## Author

Amos Elberg
