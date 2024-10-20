# Changelog for `mtg`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## Fixed

- Support mutate layout

## 0.1.1.1 - 2024-10-19

### Fixed

- Remove extraneous percent encoding

### Added

- Download missing emblems based on output from Forge


## 0.1.1.0 - 2024-10-16

### Fixed

- Sets with separate foil boosters (e.g. 9ED) don't append numbers to file names
- Properly name tokens with multiple printings in a single set
- Properly uppercase sets

### Added

- Download missing images based on output from Forge

## 0.1.0.0 - 2024-10-07

### Added

- Parsing Scryfall responses
- Searching Scryfall
- Downloading Scryfall card images to Forge format
- Downloading Scryfall token images to Forge format
