##' Download a file from a packit, for example an artefact containing
##' central burden estimates, which we could then plot on top of
##' stochastics. But more generally, we can use this for fetching any
##' file from packet (ie, from the Montagu Reporting Portal).
##'
##' @export
##' @title Fetch packit
##' @import httr2
##' @importFrom tools file_ext
##' @param packit_id The id of the packit containing the artefact.
##' @param filename The filename of the file within the packit.
##' @param server By default, the URL to the packit API on Montagu,
##' but this can be set to other packit API's if we want.
##' @returns The filename of the temporary file which has been downloaded.
fetch_packit <- function(packet_id, filename,
  server = "https://montagu.vaccineimpact.org/packit/api/") {

  # First we have to create a client, and a flow...

  client <- httr2::oauth_client(
    id = "orderly",
    token_url = sprintf("%s/deviceAuth/token", server),
    name = "orderly"
  )

  # You will be asked to type a code at this point...

  flow <- httr2::oauth_flow_device(
    client = client,
    auth_url = sprintf("%s/deviceAuth", server),
    pkce = FALSE,
    scope = NULL,
    open_browser = FALSE,
    auth_params = list(),
    token_params = list()
  )

  # Now we have a flow$access_token we can use, to get
  # a one-time token to download the file.

  req <- httr2::request(sprintf("%s/packets/%s/files/token", server, packet_id))
  req <- req |>
    httr2::req_headers("Accept" = "application/json",
                       "Content-Type" = "application/json",
                       "Authorization" = paste("Bearer", flow$access_token)) |>
         httr2::req_method("POST") |>
         httr2::req_body_json(list(paths = list(filename)))

  x <- httr2::req_perform(req)
  ott <- httr2::resp_body_json(x)$id

  # This is the URL to the file we want, including the token.

  url <- sprintf("%s/packets/%s/file?path=%s&token=%s&filename=%s&inline=false",
                 server, packet_id, filename, ott, filename)

  req <- httr2::request(url)
  req <- req |>
    httr2::req_headers("Authorization" = paste("Bearer", flow$access_token))


  # And finally, we download the file in chunks.

  out <- tempfile(fileext = tools::file_ext(filename))
  outfile <- file(out, open = "wb")
  con <- httr2::req_perform_connection(req, blocking = TRUE)
  while (!httr2::resp_stream_is_complete(con)) {
    chunk <- httr2::resp_stream_raw(con, kb = 32)
    if (length(chunk) == 0) break
    writeBin(chunk, outfile)
  }
  close(outfile)
  out
}



