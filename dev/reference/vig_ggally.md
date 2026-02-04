# View GGally vignettes

This function will open the directly to the vignette requested. If no
`name` is provided, the index of all GGally vignettes will be opened.

## Usage

``` r
vig_ggally(name)
```

## Arguments

- name:

  Vignette name to open. If no name is provided, the vignette index will
  be opened

## Details

This method allows for vignettes to be hosted remotely, reducing
GGally's package size, and installation time.

## Examples

``` r
# \donttest{
# View `ggnostic` vignette
vig_ggally("ggnostic")

# View all vignettes by GGally
vig_ggally()
# }
```
