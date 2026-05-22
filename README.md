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

#### Preparation and Installation

On Uni Bielefeld administrated PCs:

- install Docker (not system-wide, otherwise you'll be greeted with the UAC pop-up)
- restart and run Docker:
- most likely Docker will open but throw an error message due to virtualization not working yada yada. By default the Uni Bi PCs have Hyper-V disabled and you have no permission/rights to enable it. Furthermore, the PCs come without WSL installed. You can check the WSL status by opening a terminal and typing `wsl --status`. This should tell you that WSL is not installed.
  - install WSL: Just execute the command to install WSL that is shown when you run `wsl --status`. Wait for the installation to finish and restart your PC.
- after restarting you PC, Docker should open without a warning. In that case, you're good to go (or, proceed with the next step - [Start the OpenCPU Server](#start-the-opencpu-server))

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

### useful Links

#### Book on R Packages
https://r-pkgs.org/whole-game.html

#### R formatter
https://posit-dev.github.io/air/formatter.html
In the interface you can run any functions and access included data of the fearbase package by making requests in `../library/fearbase/`. 
Check the [OpenCPU API docs](https://www.opencpu.org/api.html#api-methods) for information on how to use the OpenCPU API Explorer
