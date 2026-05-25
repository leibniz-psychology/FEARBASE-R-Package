# FEARBASE-R-Package

**Short description:**  < short description that explains what this repo is used for >

|               |           |
|---------------|-----------|
| Product Owner | < email > |
| Developer     | < email > |
| dev url       | < xxxxx > |
| prod url      | < xxxxx > |
| Language      | R         |
| ...           |           |
| License       |           |

## Further optional information

### R packages used in feabse package

...

## Development

### Testing with a local OpenCPU server

You can test how any changes you make to the fearbase R package affect the deployment of the OpenCPU server by running an
OpenCPU server locally.

#### Start the OpenCPU server

In the `dev_local` directory of this repository run:

```bash
docker compose up -d --build
```

#### Stop the OpenCPU server

```bash
docker compose down
```

#### Update the OpenCPU server

After making changes to the fearbase package. Simply run

```bash
docker compose up -d --build
```

again.
If you **don't see any changes** it might be the browser caching an old state of the Docker container.
In that case either delete your browser cache or open the OpenCPU API Explorer in a new **private/icognito session**.

#### Testing on the OpenCPU server

Open [http://localhost/ocpu/](http://localhost:80/ocpu/) in your browser to access the OpenCPU API Explorer interface.

In the interface you can run any functions and access included data of the fearbase package by making requests in `../library/fearbase/`.
Check the [OpenCPU API docs](https://www.opencpu.org/api.html#api-methods) for information on how to use the OpenCPU API Explorer

#### Example Request

To run `jsonSummary` on the `randomData` enter:
* Method: `POST`
* Endpoint: `../library/fearbase/R/jsonSummary`
* Param Name: `d`
* Param Value: `"randomData"`

To see the result of the analysis, pick one of the links in the response and run a get request on it:
* Method: `GET`
* Endpoint: `/ocpu/tmp/<hash>/stdout`

#### IMPORTANT

If you want to use functions that require datasets (e.g. data_long or metadata), you need to provide these datasets first:

- send a ``POST`` request with Endpoint `../library/fearbase/R/createCsv`
- click `Add file`
  - name: `file` (same as parameter of `createCsv`)
  - browse to the csv file you want to upload
  - send the request
- If everything goes according to plan: you should see some entries that start with something like ``/ocpu/tmp/x0d715a6f605d54/``
  - the part starting with ``x`` is the session key (sth. like a unique identifier) of the returned "object", e.g. ``/ocpu/tmp/``**x0d715a6f605d54**``/console``
  - this unique session key is then the input parameter value for functions that require that csv file to run.

Example: If you want to run the `age` function:

- if you uploaded the long data csv file and the session key for that csv file was *x0d715a6f605d54*
- send a ``POST`` Ajax request with Endpoint `../library/fearbase/R/age`
  - add parameter:
    - `param name`: dl (short for data_long)
    - `param value`: x0d715a6f605d54 (the session key for the csv file)
- you should again get a list with multiple values starting with `/ocpu/tmp/<new session key>/graphics/`
  - for the age example, the actual graphic would be ``/ocpu/tmp/x0d715a6f605d54/graphics/1``
  - to access the graphic, open a new tab with the address`localhost:8004/ocpu/tmp/x0d715a6f605d54/graphics/1`

### useful Links

### Mapping maintenance

The study-to-condition mapping is now part of the package itself.

- The editable source file lives at `data-raw/mapping.csv`.
- The runtime object used by the package is the internal dataset `mapping` in `R/sysdata.rda`.
- After changing `data-raw/mapping.csv`, rebuild the internal mapping with:

```bash
"C:\\Program Files\\R\\R-4.6.0\\bin\\x64\\Rscript.exe" data-raw/build-mapping.R
```

#### Book on R Packages
https://r-pkgs.org/whole-game.html

#### R formatter
https://posit-dev.github.io/air/formatter.html
In the interface you can run any functions and access included data of the fearbase package by making requests in `../library/fearbase/`. 
Check the [OpenCPU API docs](https://www.opencpu.org/api.html#api-methods) for information on how to use the OpenCPU API Explorer
