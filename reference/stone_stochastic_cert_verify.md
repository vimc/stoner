# Validate stochastic parameter certificate

Montagu generates a certificate when a group uploads their stochastic
parameter set. Stoner can check the certificate applies to the right
touchstone and modelling group.

## Usage

``` r
stone_stochastic_cert_verify(
  con,
  certfile,
  modelling_group,
  touchstone,
  disease
)
```

## Arguments

- con:

  DBI connection to production. Used for verifying certificate against
  expected properties

- certfile:

  Name of the certificate file to be verified.

- modelling_group:

  The expected modelling group. We expect that the owner of this
  certificate logged in as this modelling_group, when uploading their
  stochastic parameter set.

- touchstone:

  The expected touchstone. We expect that the owner of the provided
  certificate uploaded a stochastic parameter set to this touchstone in
  order to get it.

- disease:

  The expected disease. We expect that the certificate provided was
  uploaded into Montagu with reference to this disease.
