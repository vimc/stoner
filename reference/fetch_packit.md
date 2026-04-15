# Fetch packet from a packit server

Download a file from a packit, for example an artefact containing
central burden estimates, which we could then plot on top of
stochastics. But more generally, we can use this for fetching any file
from packet (ie, from the Montagu Reporting Portal).

## Usage

``` r
fetch_packit(
  packet_id,
  filename,
  server = "https://montagu.vaccineimpact.org/packit/api/"
)
```

## Arguments

- packet_id:

  The id of the packet containing the artefact.

- filename:

  The filename of the file within the packet.

- server:

  By default, the URL to the packit API on Montagu, but this can be set
  to other packit API's if we want.

## Value

The filename of the temporary file which has been downloaded.
