# Liglo
A vim-first terminal web browser designed for reading documentation with minimal distraction.

## Usage
Just type your search as arguments to the executable.
```bash
$ Liglo Java map
```
search results will pop up, press enter to select the desired website.
![Search Results]("images/search_results.png")

## Installation
download executable, add to path, then register API key at [[https://programmablesearchengine.google.com/]] and add it as the GOOGLE_API_KEY environment variable.

## Changes
- [ ] Add a config file
- [ ] Put API key in config rather than an environment variable
- [ ] Add website-specific handlers
    - [ ] Make user-configurable
- [ ] Add more thorough testing
- [ ] Do more performance testing
- [ ] Remove Brick as a dependency
- [ ] Figure out something better than forcing google registration
