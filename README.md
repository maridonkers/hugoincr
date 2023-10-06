The Hugo static site generator produces a site in a public directory but it doesn't do an incremental generate. This Haskell program fixes that by touching all unchanged files (checksum) with the modified date/time from the previous Hugo generate.

# GitHub

[maridonkers/hugoincr](https://github.com/maridonkers/hugoincr)

## Releases

Linux: [binaries-linux-x64-static](https://github.com/maridonkers/hugoincr/actions/runs/6430699989)

## Blog article

[Incrementer for generated Hugo site](https://photonsphere.org/post/2023-10-06-hugoincr/)
