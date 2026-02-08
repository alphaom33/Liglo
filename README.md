# Liglo
A vim-first terminal web browser designed for reading documentation with minimal distraction.

## Usage
Type desired search as arguments to the executable.
```bash
$ Liglo Java map
```
Search results will pop up. Use vim motions to select desired website, then press enter.
![Search Results](/images/search_results.png)
Read the rendered page, then quit with q
![Rendered Page](/images/rendered_page.png)

## Installation
download executable, add to path, then register API key at https://programmablesearchengine.google.com/ and add it as the GOOGLE_API_KEY environment variable.

## Changes
- [ ] Add a config file
- [ ] Put API key in config rather than an environment variable
- [ ] Add website-specific handlers
    - [ ] Make them user-configurable
- [ ] Color theme support
- [ ] Add more thorough testing
- [ ] Do more performance testing
- [ ] Remove Brick as a dependency
- [ ] Figure out something better than forcing google registration
