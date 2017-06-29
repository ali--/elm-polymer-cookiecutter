# Work In Progress (do not use)


# Elm Polymer Cookie Cutter

Features:
  * preconfigured SPA using service-worker
  * includes google-map element
  * includes paper datetime-picker
  * preconfigured to use on Google Cloud Platform (free deployment)

## Prerequisites

Must have the Elm 0.18 installed already.

```
$ npm install -g elm-live
$ npm install -g bower
```

Install [polymer-cli](https://github.com/Polymer/polymer-cli):
(Need at least npm v0.3.0)
```
$ npm install -g polymer-cli
```

### Start the development server

    polymer serve

### Run web-component-tester tests

    polymer test

### Build

    polymer build

### Test the build

This command serves the minified version of the app in an unbundled state, as it would be served by a push-compatible server:

    polymer serve build/unbundled
    
This command serves the minified version of the app generated using fragment bundling:

    polymer serve build/bundled

## Running the Example

```shell
$ git clone git@github.com:ali--/elm-polymer-cookiecutter.git
$ cd elm-polymer-cookiecutter
$ bower install
$ elm-live src/Main.elm --output=app.js --open -- --debug
```
