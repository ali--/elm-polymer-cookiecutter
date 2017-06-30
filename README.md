# Work In Progress (do not use)

# Elm Polymer Flask Cookie Cutter

Features:
  * includes Flask back-end
  * 2 different front-ends using polymer and elm-mdl
  * preconfigured SPA using service-worker
  * includes google-map, datetime-picker, and datatable elements
  * preconfigured to use on Google Cloud Platform (for free deployment)

## Prerequisites

Must have the Elm 0.18 installed already.

## Running the elm-mdl example

```shell
$ git clone git@github.com:ali--/elm-polymer-cookiecutter.git
$ cd elm-polymer-cookiecutter/mdl_example
$ elm-make Main.elm --output=../static/app.js
# cd ..
```

## Running the polymer example 

```shell
$ git clone git@github.com:ali--/elm-polymer-cookiecutter.git
$ cd elm-polymer-cookiecutter/static
$ npm install -g bower
$ bower install
$ cd ../polymer_example/
$ elm-make Main.elm --output=../static/app.js
$ cd ..
```
$ pip install flask
$ FLASK_APP=app.py flask run --port=8888 --host=0.0.0.0
